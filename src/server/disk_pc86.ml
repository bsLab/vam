(*
**      ==================================
**      OOOO   OOOO OOOO  O      O   OOOO
**      O   O  O    O     O     O O  O   O
**      O   O  O    O     O     O O  O   O
**      OOOO   OOOO OOOO  O     OOO  OOOO
**      O   O     O    O  O    O   O O   O
**      O   O     O    O  O    O   O O   O
**      OOOO   OOOO OOOO  OOOO O   O OOOO
**      ================================== 
**      BSSLAB, Dr. Stefan Bosse sci@bsslab.de
**
**    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF: 
**    Free Software Foundation-Europe, GNU GPL License, Version 2
**
**    $MODIFIEDBY:  BSSLAB
**    $AUTHORS:     
**    $INITIAL:     (C)
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.04
**
**    $INFO:
**
**  PC86 dependent disk management (partition read/write support).
**
**  Hostlabel: one for each physical disk. Contans physical partition table!
**
**    $ENDOFINFO
**
*)



open Amoeba
open Stderr
open Bytebuf
open Machtype
open Buf
open Disk_common


let new_part () =
        {
            p_firstblk= int32 0;
            p_numblks = int32 0;
            p_piece   = int16 0;
            p_many    = int16 0;    
            p_cap     = cap_copy nilcap;
            p_sysid   = "";
        } 
let new_geom () =
        {
            g_bpt =  int16 0;   
            g_bps =  int16 0;
            g_numcyl = int16 0; 
            g_altcyl = int16 0; 
            g_numhead = int16 0;
            g_numsect = int16 0; 
        } 

      

   
(*
** size of boot programs in blocks (512 bytes)
*)

let master_BOOT_SIZE = 1    (* size of primary boot *)
let amoeba_BOOT_SIZE = 17   (* size of amoeba's secondary boot *)

(*
** every pc boot block is marked by a magic number
*)
let boot_MAGIC_BYTE_0 = 0x55    (* magic byte 0 for boot blocks *)
let boot_MAGIC_BYTE_1 = 0xAA    (* magic byte 1 for boot blocks *)
let boot_MAGIC_OFFSET_0 = (512-2)   (* offset for magic byte 0  *)
let boot_MAGIC_OFFSET_1 = (512-1)   (* offset for magic byte 1  *)

(*
** PC label constants 
*)
let pcl_NPART       = 4       (* # of partition entries *)
let pcl_PARKCYLS    = 1       (* # of cylces reserved for parking zone *)
let pcl_BOOTSIZE    = 512     (* boot block size *)
let pcl_LG2BOOTSIZE = 9       (* log2(pcl_BOOTSIZE) *)
let pcl_PCP_OFFSET  = 446     (* offset of pc partition table *)

let pcl_MAGIC buf =
    let _,b0 = buf_get_int8 buf boot_MAGIC_OFFSET_0 in
    let _,b1 = buf_get_int8 buf boot_MAGIC_OFFSET_1 in
    (b0 = boot_MAGIC_BYTE_0) &&
    (b1 = boot_MAGIC_BYTE_1)


(*
** boot indicator values 
*)
let pcp_ACTIVE     = 0x80    (* active (bootable) partition *)
let pcp_PASSIVE    = 0x00    (* passive (non bootable) partition *)

(*
** Special system identifiers 
*)

let pcp_NOSYS      = 0x00    (* no system index *)
let pcp_AMOEBA     = 0x93    (* Amoeba partition *)
let pcp_BADBLK     = 0x94    (* Amoeba bad block partition *)

(*
** Known system identifier list
*)
let sysid_list = [
        0,     "empty?";
        1,     "dos_12";
        2,     "xenix_root";
        3,     "xenix_usr";
        4,     "dos_16";
        5,     "dos_ext";
        6,     "dos_16";
        7,     "os2_or_qnx";          (* or QNX? *)
        8,     "aix";
        9,     "aix_boot";
        10,    "os2_boot";
        0xb,   "win95";
        0x40,  "venix_286";
        0x51,  "novell";
        0x52,  "microport";    (* or CPM? *)
        0x63,  "hurd";                        (* or System V/386? *)
        0x64,  "novell_net_286";
        0x65,  "novell_net_386";
        0x75,  "pc_ix";
        0x80,  "old_minix";           (* Minix 1.4a and earlier *)
        0x81,  "minix";      (* Minix 1.4b and later *)
        0x82,  "linux_swap";
        0x83,  "linux";
        0x93,  "amoeba";
        0x94,  "amoeba_bbt";          (* (bad block table) *)
        0xa5,  "bsd_386";
        0xb7,  "bsdi_fs";
        0xb8,  "bsdi_swap";
        0xc7,  "syrinix";
        0xdb,  "cpm";                     (* or Concurrent DOS? *)
        0xe1,  "dos_acc";
        0xe3,  "dos_ro";
        0xf2,  "dos_sec";
        0xff,  "bbt";                     (* (bad track table) *)
    ]

(*
** Convert sysid number to identifier string
*)    
let sysid_to_str id =
    let rec iter l =
        match l with
        | hd::tl -> let id',str=hd in
                   if id=id' then str else iter tl;
        | [] -> ""
        in
    iter sysid_list

 
(*
** Partition table entry
*)
type pc_partition = {
    mutable pp_bootind: uint8;          (* boot indicator PART_PASSIVE /PART_ACTIVE *)
    mutable pp_start_head: uint8;       (* head value for first sector *)
    mutable pp_start_sec: uint8;        (* sector value + cyl bits for first sector *)
    mutable pp_start_cyl: uint8;        (* track value for first sector *)
    mutable pp_sysind: uint8;           (* system indicator *)
    mutable pp_last_head: uint8;        (* head value for last sector *)
    mutable pp_last_sec: uint8;         (* sector value + cyl bits for last sector *)
    mutable pp_last_cyl: uint8;         (* track value for last sector *)
    mutable pp_first: uint32;            (* logical first sector *)
    mutable pp_size: uint32;             (* size of partion in sectors *)
}


(*
** Get PC partition table (block 0 - block size 512 bytes)
*)


let get_hostlabel buf =
    (*
    ** Is it a valid partition ?
    *)
    if not (pcl_MAGIC buf) then
        std_ARGBAD,None
    else
    begin
        let pt = Array.init max_PARTNS_PDISK (fun _ -> new_part ()) in
        for i = 0 to (pcl_NPART-1)
        do
          let n = i*16 in
          let pe = {
            pp_bootind = (let _,i = 
                            buf_get_mach buf (pcl_PCP_OFFSET+0+n) Uint8 in i);
            pp_start_head = (let _,i =
                            buf_get_mach buf (pcl_PCP_OFFSET+1+n) Uint8 in i);
            pp_start_sec = (let _,i =
                            buf_get_mach buf (pcl_PCP_OFFSET+2+n) Uint8 in i);
            pp_start_cyl = (let _,i =
                            buf_get_mach buf (pcl_PCP_OFFSET+3+n) Uint8 in i);
            pp_sysind = (let _,i =
                            buf_get_mach buf (pcl_PCP_OFFSET+4+n) Uint8 in i);
            pp_last_head = (let _,i =
                            buf_get_mach buf (pcl_PCP_OFFSET+5+n) Uint8 in i);
            pp_last_sec = (let _,i =
                            buf_get_mach buf (pcl_PCP_OFFSET+6+n) Uint8 in i);
            pp_last_cyl = (let _,i =
                            buf_get_mach buf (pcl_PCP_OFFSET+7+n) Uint8 in i);
            pp_first = (let _,i = 
                            buf_get_mach buf (pcl_PCP_OFFSET+8+n) Uint32 in i);
            pp_size = (let _,i =
                            buf_get_mach buf (pcl_PCP_OFFSET+12+n) Uint32 in i);
            } in
            (*
            ** fill amoeba label with partition information
            *)
            pt.(i).p_firstblk <- int32 (to_int pe.pp_first);
            pt.(i).p_numblks  <- int32 (to_int pe.pp_size);
            pt.(i).p_sysid <- sysid_to_str (to_int pe.pp_sysind);
            if ((to_int pe.pp_sysind) = pcp_AMOEBA) then
            begin
                (*
                ** First block(s) of partition are used for booting
                *)
                pt.(i).p_firstblk <- pt.(i).p_firstblk + (int32 amoeba_BOOT_SIZE);
                pt.(i).p_numblks  <- pt.(i).p_numblks  - (int32 amoeba_BOOT_SIZE);
            end;
        done;
        std_OK,(Some 
            {
                d_disktype = Disk_physical "";
                d_geom = new_geom (); (* Empty geometry informations *)
                d_ptab = pt;
                d_magic = int32 0;
                d_chksum = uint16 0;
                
            }
        )
    end
    

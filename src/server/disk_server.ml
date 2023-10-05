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
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2004-2005 BSSLAB
**    $CREATED:     
**    $VERSION:     1.12
**
**    $INFO:
**
**  Logical and physical Disk with partition table support - server parts
**  (Low level interface - high level interface can be found in the
**   vdisk module)
**  - contains machine dependent parts !!! -
**
**    $ENDOFINFO
**
*)

open Amoeba
open Machtype
open Disk_common
open Disk_client
open Disk_pc86
open Stderr
open Ar
open Stdcom 

open Unix
open Bytebuf
open Printf
open Machtype
open Buf
open Name
open Syslog

let nl = print_newline 
let info str =
    print_string ("Disk_server: "^str); nl ()

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

      

let get_diskgeom buf pos =
    let pos,bpt = buf_get_mach buf pos Int16 in
    let pos,bps = buf_get_mach buf pos Int16 in
    let pos,numcyl = buf_get_mach buf pos Int16 in
    let pos,altcyl = buf_get_mach buf pos Int16 in
    let pos,numhead = buf_get_mach buf pos Int16 in
    let pos,numsect = buf_get_mach buf pos Int16 in
    pos,
    {
        g_bpt=bpt;
        g_bps=bps;
        g_numcyl=numcyl;
        g_altcyl=altcyl;
        g_numhead=numhead;
        g_numsect=numsect;
    }

let get_diskpart buf pos =
    (pos+max_PARTNS_PDISK*sizeof_part_tab),(new_part ())     

(*
** Extract a disklabel (sub partition) from the already read 
** buffer content.
*)
let get_amdisklabel buf =
    (*
    ** Check for an Amoeba label
    *)
    
    let pos = filler_SIZE in
    let len = typeLEN in
    let disktype = buf_gets buf pos len in
    let pos = pos + len in
    let pos,geom = get_diskgeom buf pos in
    let parts = Array.init max_PARTNS_PDISK (fun _ -> new_part()) in
    for i = 0 to max_PARTNS_PDISK-1
    do
        let n = i * sizeof_part_tab in
        parts.(i).p_firstblk <- (let _,i = 
                                    buf_get_mach buf (pos+n+0) Int32 in i);
        parts.(i).p_numblks <- (let _,i = 
                                    buf_get_mach buf (pos+n+4) Int32 in i);
        parts.(i).p_piece <- (let _,i = 
                                    buf_get_mach buf (pos+n+8) Int16 in i);
        parts.(i).p_many <- (let _,i = 
                                    buf_get_mach buf (pos+n+10) Int16 in i);
        parts.(i).p_cap <- (let _,c = 
                                    buf_get_cap buf (pos+n+12) in c);
        
    done;
    let pos,ptab = get_diskpart buf pos in
    let pos,magic = buf_get_mach buf pos Int32 in   

    if magic = (int32 am_DISK_MAGIC) 
        then std_OK,(Some {
                d_disktype = Disk_amoeba disktype;
                d_geom = geom;
                d_ptab = parts;
                d_magic = magic;
                d_chksum = uint16 0;
            })
        else std_OK,None
   
let new_vdisk () = {    
        v_name ="";
        v_cap = nilcap;
        v_numblks = 0;
        v_many = 0;
        v_parts = Array.init max_PARTNS_PDISK (fun i -> {
                            i_physpart = None;
                            i_part = 0;
                            i_firstblk = 0;
                            i_numblks = 0;
                        }) ;
    }

(*
** Print out informations about the partitions list of one device.
*)
let info_disklabels ~dev =
    let labellist = dev.dev_labels in
    let str = ref "" in
    let info s = str := !str ^ s in
 
    let len = List.length labellist in
    let hostlabel = 
        if len >= 1 then [List.hd labellist] else [] in    
    let amlabels =
        if len > 1 then (List.tl labellist) else [] in
    if hostlabel <> [] then
    begin
        let l = List.hd hostlabel in
        info (sprintf "Physical label: %s\n" (match l.d_disktype
                with | Disk_physical str -> sprintf "PHYS   <%s>" str;
                     | Disk_logical str -> sprintf  "OTHER  <%s>" str;
                     | Disk_amoeba str -> sprintf   "AMOEBA <%s>" str));
        info "  Geometry informations:\n";
        let g = l.d_geom in
        info (sprintf "    Cylinders=%d   Heads=%d   Sectors=%d  BytesPerSec=%d [Blocksize]\n"
                (to_int g.g_numcyl) 
                (to_int g.g_numhead) 
                (to_int g.g_numsect)
                (to_int g.g_bps)); 
        info "  Partitions [Block units]:\n";
        Array.iter (fun p ->
                info (sprintf 
                    "    Start=%8d  Size=%8d Id=%s\n                 Cap=%s\n"
                    (to_int p.p_firstblk)
                    (to_int p.p_numblks)
                    p.p_sysid
                    (Ar.ar_cap p.p_cap));
            ) l.d_ptab;
        if amlabels <> [] then
        begin
            List.iter (fun l->
                info (sprintf "Label: %s\n" (match l.d_disktype
                with | Disk_physical str -> sprintf "PHYS   <%s>" str;
                     | Disk_logical str -> sprintf  "OTHER  <%s>" str;
                     | Disk_amoeba str -> sprintf   "AMOEBA <%s>" str));
                info "  Geometry informations:\n";
                let g = l.d_geom in
                info (sprintf 
                        "    Cylinders=%d   Heads=%d   Sectors=%d  BytesPerSec=%d [Blocksize]\n"
                        (to_int g.g_numcyl) 
                        (to_int g.g_numhead) 
                        (to_int g.g_numsect)
                        (to_int g.g_bps)); 
                info "  Partitions [Block units]:\n";
                Array.iter (fun p ->
                    info (sprintf 
                    "    Start=%8d Size=%8d Id=%s\n                   Cap=%s\n"
                    (to_int p.p_firstblk)
                    (to_int p.p_numblks)
                    p.p_sysid
                    (Ar.ar_cap p.p_cap));
                  ) l.d_ptab;
                ) amlabels;
        end;
    end;
    !str

(*
** Get infos about all virtual disks (v,l,p)
*)

let info_vdisks ~dev ~disks =
    let str = ref "" in
    let info s = str := !str ^ s in
    let vl = disks.vdisks in
    let pl = disks.pdisks in
    let ll = disks.ldisks in

    info "Amoeba Virtual disks:\n";
    info (sprintf "  Blocksize=%d bytes\n" dev.dev_blksize);

    List.iter (fun v ->
            info (sprintf "\n  %s: cap=%s\n" v.v_name (ar_cap v.v_cap));
            info (sprintf "    numblocks=%d  partitions=%d\n"
                            v.v_numblks v.v_many);
            for i = 0 to (max_PARTNS_PDISK-1)
            do 
                let vp = v.v_parts.(i) in
                if vp.i_numblks <> 0 then
                begin
                    info (sprintf "    Partition %d\n" i);
                    info (sprintf "      firstblk=%d   numblocks=%d\n"
                            vp.i_firstblk vp.i_numblks);
                    match vp.i_physpart with
                    | Some p ->
                    begin
                        info (sprintf 
                            "      Physical partition: dev=%s  labelblock=%d   (Amoeba Disklabel)\n"
                            p.dev.dev_path p.labelblock);
                        (*
                        ** Amoeba disk label! Only one parttion table entry
                        ** if any.
                        *)
                        let lb = p.label in
                        let ge = lb.d_geom in
                        info (sprintf "       CHS=%d/%d/%d\n"
                                        (to_int ge.g_numcyl) 
                                        (to_int ge.g_numhead)
                                        (to_int ge.g_numsect));

                        for i = 0 to (max_PARTNS_PDISK-1)
                        do
                            let pt = lb.d_ptab.(i) in
                            if pt.p_numblks <> (int32 0) then
                                info (sprintf
                                    "        %d start=%d blocks=%d sysid=%s\n"
                                    i 
                                    (to_int pt.p_firstblk)
                                    (to_int pt.p_numblks)
                                    pt.p_sysid);
                        done;
                    end;
                    | None -> ();
                end;
            done;
        ) vl;
    info "Physical disks:\n";
    info (sprintf "  Blocksize=%d bytes\n" dev.dev_blksize);

    List.iter (fun v ->
            info (sprintf "\n  %s: cap=%s\n" v.v_name (ar_cap v.v_cap));
            info (sprintf "    numblocks=%d  partitions=%d\n"
                            v.v_numblks v.v_many);
            for i = 0 to (max_PARTNS_PDISK-1)
            do 
                let vp = v.v_parts.(i) in
                if vp.i_numblks <> 0 then
                begin
                    info (sprintf "    Partition %d\n" i);
                    info (sprintf "      firstblk=%d   numblocks=%d\n"
                            vp.i_firstblk vp.i_numblks);
                    match vp.i_physpart with
                    | Some p ->
                    begin
                        info (sprintf 
                            "      Physical partition: dev=%s  labelblock=%d (Physical Disklabel)\n"
                            p.dev.dev_path p.labelblock);
                        (*
                        ** Physical disk label! 
                        *)
                        let lb = p.label in
                        let ge = lb.d_geom in
                        info (sprintf "       CHS=%d/%d/%d\n"
                                        (to_int ge.g_numcyl) 
                                        (to_int ge.g_numhead)
                                        (to_int ge.g_numsect));

                        for i = 0 to (max_PARTNS_PDISK-1)
                        do
                            let pt = lb.d_ptab.(i) in
                            if pt.p_numblks <> (int32 0) then
                                info (sprintf
                                    "        %d start=%d blocks=%d sysid=%s\n"
                                    i 
                                    (to_int pt.p_firstblk)
                                    (to_int pt.p_numblks)
                                    pt.p_sysid);
                        done;
                    end;
                    | None -> ();
                end;
            done;
        ) pl;
    !str
    
(*
** Print the virtual disk table
*)

let print_vdisk_table vt =
    let i = ref 0 in
    let str = ref "" in
    let info s = str := !str ^ s in

    info "Virtual Disk Table:\n";

    if vt <> [||] then
    Array.iter (fun vs ->
        incr i;
        match vs with
        | Some v ->
            info (sprintf "\n  [%d] %s: cap=%s\n" 
                        (!i-1) v.v_name (ar_cap v.v_cap));
            info (sprintf "    numblocks=%d  partitions=%d\n"
                            v.v_numblks v.v_many);
            for i = 0 to (max_PARTNS_PDISK-1)
            do 
                let vp = v.v_parts.(i) in
                if vp.i_numblks <> 0 then
                begin
                    info (sprintf "    Partition %d\n" i);
                    info (sprintf "      firstblk=%d   numblocks=%d\n"
                            vp.i_firstblk vp.i_numblks);
                    match vp.i_physpart with
                    | Some p ->
                    begin
                        info (sprintf 
                            "      Physical partition: dev=%s  labelblock=%d\n"
                            p.dev.dev_path p.labelblock);

                        let lb = p.label in

                        for i = 0 to (Array.length lb.d_ptab)-1
                        do
                            let pt = lb.d_ptab.(i) in
                            if pt.p_numblks <> (int32 0) then
                                info (sprintf
                                    "        %d start=%d blocks=%d sysid=%s\n"
                                    i 
                                    (to_int pt.p_firstblk)
                                    (to_int pt.p_numblks)
                                    pt.p_sysid);
                        done;
                    end;
                    | None -> ();
                end;
            done;
        | None -> (); 
        ) vt;
    !str
    

(*
** Initialize one disk:
**  1. Read physical (host) partition table.
**  2. Search for amoeba partitions, read the amoeba disklabels
**      (subpartition table).
**  3. Create and initialize all virtual disk structures.
*)

let vdisk_init ~dev ~disks = 
    let label_copy l =
          let plen = Array.length l.d_ptab in
          let l'=  {
                d_disktype = l.d_disktype;
                d_geom = new_geom (); 
                d_ptab = Array.init plen (fun _ -> new_part ());
                d_magic = int32 0;
                d_chksum = uint16 0;
            } in
        for i = 0 to (plen-1)
        do
            let lp = l.d_ptab.(i) in
            l'.d_ptab.(i) <- {  p_firstblk = lp.p_firstblk;
                                p_numblks = lp.p_numblks;
                                p_piece = lp.p_piece;
                                p_many = lp.p_many;
                                p_cap = cap_copy lp.p_cap;
                                p_sysid = lp.p_sysid}; 
        done;
        l'
        in

    
    let buf = buf_create dev.dev_blksize in
    let stat = dev.dev_read ~first:0 ~size:1 ~buf:buf ~off:0 in
    if stat <> std_OK then
        failwith ("disk_init: can't read block 0 of disk:"^(err_why stat));

    (*
    ** Add one amoeba partition and extract all virtual disks
    ** found in the amoeba label of this physical partition. 
    ** Check for already exisiting 
    ** vdisks and extend them in this case.
    *)
    let add_ampartn name num vtab firstblock label =
        let added = ref false in

        let part = {
                dev = dev;
                labelblock = firstblock;    
                label = label;
            } in

        (*
        ** Looks through the list of currently known virtual disks for one with 
        ** the same capability as 'cap'.  If none is found it returns None.
        ** Otherwise it returns the vdisk entry. 
        *)
        let rec capmatch l cap =
            match l with
            | hd::tl -> if (prv_number cap.cap_priv) =
                           (prv_number hd.v_cap.cap_priv)
                            then (Some hd)
                            else capmatch tl cap;
            | [] -> None;
            in

        Array.iter (fun pt ->
            if pt.p_numblks <> (int32 0) then
            begin
                let vdp = capmatch !vtab pt.p_cap in
                match vdp with
                | None ->
                begin
                    (*
                    ** A new one was found.
                    *)
                    let n = to_int pt.p_piece in
                    let vdp = new_vdisk () in
                    vdp.v_many <- to_int pt.p_many;
                    vdp.v_cap <- pt.p_cap;
                    vdp.v_name <- (sprintf "%s:%.2d" name !num);
                    vdp.v_parts.(n).i_physpart <- Some part; 
                    vdp.v_parts.(n).i_firstblk <- 
                            (to_int pt.p_firstblk) + 1 
                            (* + host_partn_offset *);
                    vdp.v_parts.(n).i_numblks <- (to_int pt.p_numblks) - 1;
                    vdp.v_numblks <- (to_int pt.p_numblks) - 1;
                    vtab := !vtab @ [vdp];
                    incr num;
                end;
                | Some vdp ->
                try
                begin
                    (*
                    ** This partition seems to belong to an already
                    ** existing vdisk. Append it.
                    *)
                    if not (portcmp pt.p_cap.cap_priv.prv_random
                                    vdp.v_cap.cap_priv.prv_random) ||
                       not (portcmp pt.p_cap.cap_port 
                                    vdp.v_cap.cap_port) then
                    begin
                        sys_log Sys_fatal "VDISK: disk_init/add_partn: invalid vdisk cap\n";
                        raise Exit;
                    end;
                    (*
                    ** Check v_many matches the value in the 
                    ** vdisk table
                    *)
                    if pt.p_many <> (int16 vdp.v_many) then
                    begin
                        sys_log Sys_fatal "VDISK: disk_init/add_partn: inconstistent vdisk part.\n";
                        raise Exit;
                    end;
                    (*
                    ** Do this partition already exist ?
                    *)
                    let n = to_int pt.p_piece in
                    if n >= max_PARTNS_PDISK ||
                       vdp.v_parts.(n).i_physpart <> None then
                    begin
                        sys_log Sys_fatal 
                                "VDISK: disk_init/add_partn: two parts. for same slot.\n";
                        raise Exit;
                    end;
    
                    (*
                    ** The +1's & -1's below are to step over partn label 
                    *)
                     
                    vdp.v_parts.(n).i_physpart <- Some part;
                    vdp.v_parts.(n).i_firstblk <- 
                            (to_int pt.p_firstblk) + 1 
                            (* + host_partn_offset *);
                    vdp.v_parts.(n).i_numblks <-
                            (to_int pt.p_numblks) - 1;
                    vdp.v_numblks <- vdp.v_numblks +
                                     (to_int pt.p_numblks) - 1;
       
                end
                with    | Exit -> ();
            end;
            ) label.d_ptab;
        in

    (*
    **
    ** This routine makes a fake Amoeba partition which becomes a single
    ** virtual disk.   The partition does not have an Amoeba label on it
    ** since it belongs to other OS's.
    ** The capability is created later during creation of the 
    ** vdisk table. The log partitions object number start with
    ** vdisknum+1. But for now we don't know the maximal vdisknum!
    *)
    let mk_log_partn firstblk disknum label geom =
        let pt = label.d_ptab.(0) in

        (*
        ** There is no amlabel block! Decrement first block number.
        ** Will be incremented by add_ampartn.
        *)
        pt.p_firstblk <- pt.p_firstblk - (int32 1);
        pt.p_piece <- int16 0;
        pt.p_many <- int16 1;

        pt.p_cap <- nilcap;
        label.d_geom <- geom;
        (*
        ** Now it's a valid 'amoeba vdisk'
        *)
        label.d_magic <- int32 am_DISK_MAGIC;
        in

    (*
    **
    ** This routine makes a fake Amoeba partition which becomes a single
    ** virtual disk.   The partition does not have an Amoeba label on it
    ** since that would use block 0 of the partition and we need block 0
    ** for our application.  In fact this partition is the boot partition
    ** and starts at physical block zero of the disk.  It is not a good
    ** idea to let normal mortals near this since it probably contains the
    ** partition information and the secondary bootstrap.
    ** The capability is created later during creation of the 
    ** vdisk table. The log partitions object number start with
    ** vdisknum+1. But for now we don't know the maximal vdisknum!
    *)
    
    let mk_phy_partn dev disknum label =
        let pt = label.d_ptab.(0) in

        (*
        ** There is no amlabel block! Decrement first block number.
        ** Will be incremented by add_ampartn.
        *)
        pt.p_firstblk <- (int32 (-1)); 
                        (* - host_partn_offset *)
        pt.p_piece <- int16 0;
        pt.p_many <- int16 1;

        let di32 = int32 d_PHYSBLKSZ in
        let bs32 = int32 dev.dev_blksize in
        pt.p_numblks <- int32 (dev.dev_geom.dev_numcyl *
                               dev.dev_geom.dev_numhead *
                               dev.dev_geom.dev_numsect);

        if dev.dev_blksize >= d_PHYSBLKSZ
            then pt.p_numblks <- pt.p_numblks * (bs32 / di32)
                            + (int32 1)
            else pt.p_numblks <- pt.p_numblks / (di32 / bs32)
                            + (int32 1);

        pt.p_cap <- nilcap;
        label.d_geom <- {
                g_bpt = int16 0;
                g_bps = int16 0;  
                g_altcyl = int16 0;
                g_numcyl = int16 dev.dev_geom.dev_numcyl;
                g_numhead = int16 dev.dev_geom.dev_numhead;
                g_numsect = int16 dev.dev_geom.dev_numsect;
            };
        (*
        ** Only one fake partition. Clear the remaining parts.
        *)
        for i = 1 to max_PARTNS_PDISK-1
        do
            let pt = label.d_ptab.(i) in
            pt.p_firstblk <- int32 0;
            pt.p_numblks <- int32 0;
        done;

        (*
        ** Now it's a valid 'amoeba vdisk'
        *)
        label.d_magic <- int32 am_DISK_MAGIC;
        in


    (*
    ** Is there a hostlabel (partition table) ?
    *)
    let stat,hostlabel = get_hostlabel buf in

    let p_only = 
        if stat <> std_OK then
        begin
            info ("disk_init: warning: no hostlabel found (creating only physical disk): "^(err_why stat));
            true
        end
        else
            false in


    let ptab = ref [] in
    let vtab = ref [] in
    let ltab = ref [] in

    let num_ldisks = ref (List.length disks.ldisks) in
    let num_vdisks = ref (List.length disks.vdisks) in
    let num_pdisks = ref (List.length disks.pdisks) in

    (*
    ** Build the list of all found partitions. 
    ** First the host disklabel (physical partition table) if any, followed by
    ** the Amoeba disklabels (with sub partitions, too), and 
    ** logical disklabels (other OS partitions found on this disk) if any.
    *)

    let labels = 
      let disk_labels = ref [] in
      if p_only then
      begin
        (*
        ** Make a fake disklabel for the physical disk! For example
        ** a floppy disk without a hostlabel...
        ** We need access to the physical disk for several reasons:
        ** Boot sector manipulation, fdisk support...
        *)
        let name = "pdisk" in
        
        let hl = 
          let plen = max_PARTNS_PDISK in
          {
                d_disktype = Disk_physical "";
                d_geom = new_geom(); 
                d_ptab = Array.init plen (fun _ -> new_part ());
                d_magic = int32 0;
                d_chksum = uint16 0;
          } in

        hl.d_geom.g_numcyl <- int16 dev.dev_geom.dev_numcyl;
        hl.d_geom.g_numsect <- int16 dev.dev_geom.dev_numsect;
        hl.d_geom.g_numhead <- int16 dev.dev_geom.dev_numhead;

        mk_phy_partn dev num_pdisks hl;
        add_ampartn name num_pdisks ptab (-1) hl;
        disks.pdisks <- !ptab;
        std_OK    
      end else
      match hostlabel with
      | Some hl -> 
      begin
        disk_labels := !disk_labels @ [hl];
        
        (*
        ** Now look for each partition to see if it is
        ** for Amoeba.
        *)
        Array.iter (fun p ->
            (*
            ** Skip empty partitions
            *)
            if p.p_numblks <> (int32 0) then
            begin
                (*
                ** Read first block of this partition
                *)

                let firstblk = (to_int p.p_firstblk) in
                let stat = dev.dev_read ~first:firstblk 
                                        ~size:1 
                                        ~buf:buf 
                                        ~off:0 in
                                                      
                if stat <> std_OK then
                    failwith "disk_init: can't read partition data";

                let stat,disklabel = get_amdisklabel buf in    
                match disklabel with

                | Some dl -> 
                    (*
                    ** Disk_amoeba:
                    ** It's an Amoeba partition containing
                    ** subpartitions (vdisks).
                    *)
                    Array.iter (fun p' -> p'.p_sysid <- p.p_sysid) dl.d_ptab;
                    disk_labels := !disk_labels @ [dl];
                    
                    (*
                    ** Amoeba labels contain disk geometry informations.
                    ** If the device geometry is currently unknown, use
                    ** the values from the amoeba disklabel.
                    *)
                    if dev.dev_geom.dev_numcyl = 0 &&
                       dl.d_geom.g_numcyl <> (int16 0) then
                    begin
                        let g = dev.dev_geom in
                        let a = dl.d_geom in
                        g.dev_numcyl <- to_int a.g_numcyl;
                        g.dev_numhead <- to_int a.g_numhead;
                        g.dev_numsect <- to_int a.g_numsect;
                    end;
                    (*
                    ** But if there is an already supplied disk
                    ** geometry, check it with this label...
                    *) 
                    if dev.dev_geom.dev_numcyl <> 0 then
                    begin
                        let g = dev.dev_geom in
                        let a = dl.d_geom in
                        let cmp = g.dev_numcyl = (to_int a.g_numcyl) &&
                                  g.dev_numhead = (to_int a.g_numhead) &&
                                  g.dev_numsect = (to_int a.g_numsect) in
                        if not cmp then
                        begin
                            sys_log Sys_warn
                            "VDISK: Warning: expecting CHS=%d,%d,%d, but read from amoeba disklabel CHS'=%d,%d,%d\n"
                            g.dev_numcyl g.dev_numhead g.dev_numsect
                            (to_int a.g_numcyl) 
                            (to_int a.g_numhead)
                            (to_int a.g_numsect);
                        end;
                    end;
                    (*
                    ** Add partitions content to vdisk table.
                    ** Each subpartition of this partition can hold one vdisk.     
                    ** But look for partitions belonging to the same vdisk.
                    *)
                    let name = "vdisk" in

                    add_ampartn name num_vdisks vtab firstblk dl;

                | None -> 
                    let name = sprintf "ldisk@%s"
                                       (match p.p_sysid with
                                        | "dos_12"
                                        | "dos_16"
                                        | "win95" -> "dos";
                                        | str -> str) in
                                        
                    (*
                    ** It's not an Amoeba partition label!
                    ** Make a generic logical disk.
                    *)
                    let label = {
                                d_disktype = Disk_logical "";
                                d_geom = new_geom();
                                d_ptab = [|p|]; 
                                d_magic = int32 0;
                                d_chksum = uint16 0;
                            } in
                    mk_log_partn firstblk num_ldisks label hl.d_geom;
                    add_ampartn name num_ldisks ltab firstblk label;

                    disk_labels := !disk_labels @ [label];
            end;    
            ) hl.d_ptab;

        (*
        ** Make a fake disklabel for the physical disk!
        ** We need access to the physical disk for several reasons:
        ** Boot sector manipulation, fdisk support...
        *)
        let name = "pdisk" in
        
        let hl' = label_copy hl in
        mk_phy_partn dev num_pdisks hl';
        add_ampartn name num_pdisks ptab (-1) hl';

        (*
        ** Update (global) disklabel and virtual disk lists.
        ** All devices share the same lists...
        *)

        dev.dev_labels <- !disk_labels;
        disks.pdisks <- disks.pdisks @ !ptab;
        disks.vdisks <- disks.vdisks @ !vtab;
        disks.ldisks <- disks.ldisks @ !ltab;

        std_OK
      end;
      | None -> 
        (*
        ** Perhaps an amoeba disklabel is directly stored at 
        ** block 0 ?
        ** TODO
        *)
        std_NOTNOW 
      in


    labels


(*
** Create the vdisk table array:
**  1. The real virtual amoeba disks (with highest vdisk#)
**  2. Physical disks (objnum = vdisk#+1...)
**  3. Logical disks
** The capabilities of the 2. & 3. disks must recomputed with
** respect to theit object numbers!
*)

let vdisk_table disks =
    let server_port = ref nilport in

    let dknum = ref 0 in
    let vlen = (List.iter ( fun vd ->
                            let Objnum vn = prv_number vd.v_cap.cap_priv in
                            dknum := max vn !dknum;
                        ) disks.vdisks;
                !dknum) +
               (List.length disks.pdisks) +
               (List.length disks.ldisks) in
    let var = Array.create (vlen+1) None in
    (*
    ** The real amoeba virtual disks have already an known object number and
    ** therefore the place in the table
    *)
    List.iter (fun vd ->
                let Objnum vn = prv_number vd.v_cap.cap_priv in
                var.(vn) <- Some vd;
                if vd.v_cap.cap_port <> nilport &&
                   !server_port = nilport 
                    then server_port := vd.v_cap.cap_port;
            ) disks.vdisks;

    incr dknum;
    if !server_port = nilport 
        then server_port := uniqport ();

    (*
    ** The physical and logical disks must get first a unique 
    ** object number.
    *)
    List.iter (fun pd ->
                let rnd = uniqport () in
                let prv = prv_encode ~obj:(Objnum !dknum)
                                     ~rights:prv_all_rights
                                     ~rand:rnd in
                pd.v_cap <- {cap_port = !server_port;
                             cap_priv = prv};
                var.(!dknum) <- Some pd;
                incr dknum;
            ) disks.pdisks;
    List.iter (fun ld ->
                let rnd = uniqport () in
                let prv = prv_encode ~obj:(Objnum !dknum)
                                     ~rights:prv_all_rights
                                     ~rand:rnd in
                ld.v_cap <- { cap_port = !server_port;
                              cap_priv = prv};
                var.(!dknum) <- Some ld;
                incr dknum;
            ) disks.ldisks;
    var


(*
** Publish virtual disk capabilities (in directory
** dirname) somewhere in the DNS tree.
**
** Note: the virtual disk structure (v_cap) holds the private (get) port!
*)

let vdisk_publish ~disk_table ~dirname =
  try
  begin
    (*
    ** First check for exitisting directory path
    *)
    let stat,dircap = name_lookup dirname in
    if stat <> std_OK then raise (Error stat);
    let stat,_ = std_info dircap 100 in
    if stat <> std_OK then raise (Error stat);
 
    Array.iter (fun ovd ->
            match ovd with
            | Some vd ->
            begin
                let cap  = vd.v_cap in 
                let cap' = {cap with cap_port = priv2pub cap.cap_port } in
                let path = dirname ^ "/" ^ vd.v_name in
                (*
                ** Check for old remains ...
                *)
                let stat,oldcap = name_lookup path in
                if stat = std_OK then 
                begin
                    let stat = name_delete path in
                    if stat <> std_OK then
                    begin
                        sys_log Sys_err "VDISK: Can't delete old vdisk cap %s\n"
                                        path;
                        raise (Error stat);
                    end;
                end;
                let stat = name_append path cap' in
                if stat <> std_OK then
                begin
                    sys_log Sys_err "VDISK: Can't append new vdisk cap %s\n"
                                    path;
                    raise (Error stat);
                end;
            end;
            | None -> ();
        ) disk_table;
    std_OK
  end;
  with
    | Error stat -> stat

(*
** Remove published vdisk caps.
**
** Note: the virtual disk structure (v_cap) holds the private (get) port!
*)

let vdisk_remove ~disk_table ~dirname =
  try
  begin
    (*
    ** First check for exitisting directory path
    *)
    let stat,dircap = name_lookup dirname in
    if stat <> std_OK then raise (Error stat);
    let stat,_ = std_info dircap 100 in
    if stat <> std_OK then raise (Error stat);
 
    Array.iter (fun ovd ->
            match ovd with
            | Some vd ->
            begin
                let cap  = vd.v_cap in 
                let cap' = {cap with cap_port = priv2pub cap.cap_port } in
                let path = dirname ^ "/" ^ vd.v_name in
                (*
                ** Check for old remains ...
                *)
                let stat,oldcap = name_lookup path in
                if stat = std_OK then 
                begin
                    let stat = name_delete path in
                    if stat <> std_OK then
                    begin
                        sys_log Sys_err "VDISK: Can't delete current vdisk cap %s\n"
                                        path;
                        raise (Error stat);
                    end;
                end;
            end;
            | None -> ();
        ) disk_table;
    std_OK
  end;
  with
    | Error stat -> stat
    
(*
** Server requests
*)




(*
** DISK_RW
**
**      Returns STD_OK if it could successfully read/write
**      "vblksz" * "num_vblks" bytes beginning at disk block "start" from/to
**      the virtual disk specified by "priv" into/from "buf".  Otherwise it
**      returns an error status indicating the nature of the fault.
**
*)

let vdisk_rw ~hdr ~vblksz ~vstartblk ~num_vblks ~buf ~vdisks =
    let prv = hdr.h_priv in
    let cmd = hdr.h_command in

    let vlen = Array.length vdisks in
    (*
    ** Validate capability 
    *)
    let Objnum vdiskn = prv_number prv in
    if vdiskn < 0 ||
       vdiskn > vlen ||
       vdisks.(vdiskn) = None ||
       not (let vdp = match vdisks.(vdiskn) with
                      | Some vdp -> vdp;
                      | None -> failwith "progerr" in
            prv_decode prv vdp.v_cap.cap_priv.prv_random)
        then std_CAPBAD
    else
    try
    begin
        (*
        ** Check operation and rights
        *)
        (
            match cmd with
            | cmd when cmd = disk_WRITE -> 
                let Rights_bits rights = prv_rights prv in
                let Rights_bits req = disk_RGT_WRITE in
                if (rights land req) <> req 
                    then raise (Error std_DENIED);
            | cmd when cmd = disk_READ -> 
                let Rights_bits rights = prv_rights prv in
                let Rights_bits req = disk_RGT_READ in
                if (rights land req) <> req 
                    then raise (Error std_DENIED);
            | _ -> raise (Error std_COMBAD);
        );

        let vdp = match vdisks.(vdiskn) with
                      | Some vdp -> vdp;
                      | None -> failwith "progerr" in        
        (*
        ** Convert virtual blocks to physical blocks.
        ** VBS must be mutiples of PBS!
        *)
        let l2vblksz = let rec iter v n =
                        if v > 0 
                            then iter (v lsr 1) (n+1)
                            else (n-1) in
                       iter vblksz 0 
            in

        let scale = l2vblksz - d_PHYS_SHIFT in
        let pstartblk = ref (vstartblk lsl scale) in
        let num_pblks = ref (num_vblks lsl scale) in

        (*
        ** Some safety checks against bad parameters 
        *)
        
        if num_vblks <= 0 ||
           vblksz < d_PHYSBLKSZ ||
           !pstartblk < 0 ||
           !pstartblk > vdp.v_numblks ||
           !pstartblk + !num_pblks > vdp.v_numblks 
            then raise (Error std_ARGBAD);

        (*
        ** Find partition where i/o starts. One vdisk may consist
        ** of several phyiscal partitions!
        *)

        let partn = ref 0 in
        let found = ref false in

        for i = 0 to vdp.v_many - 1
        do
            let vp = vdp.v_parts.(i) in
            if not !found then
            begin
                if vp.i_numblks <= !pstartblk then
                begin    
                    pstartblk := !pstartblk - vp.i_numblks
                end
                else 
                begin
                    partn := i;
                    found := true;
                end;
            end;
        done;    

        if not !found then
            failwith "disk_rw: inconsistent virtual disk - start part. not found.";

        let bufoff = ref 0 in

        while (!num_pblks > 0)
        do
            if !partn > vdp.v_many then
                failwith "disk_rw: inconsistent virtual disk - not enough phys. parts.";

            let vp = vdp.v_parts.(!partn) in
            let pp = match vp.i_physpart with
                     | Some pp -> pp;
                     | None -> failwith "disk_rw: inconsistent virtual disk - no phys. part.";
                    in
            let size = min !num_pblks (vp.i_numblks - !pstartblk) in
            let dev = pp.dev in

            let iofun =
                match cmd with
                | cmd when cmd = disk_WRITE -> dev.dev_write;
                | cmd when cmd = disk_READ  -> dev.dev_read;
                | _ -> raise (Error std_COMBAD);
                in
            let stat = iofun ~first:(vp.i_firstblk + !pstartblk)
                             ~size:size
                             ~buf:buf
                             ~off: !bufoff in
            if stat <> std_OK then
                raise (Error stat);

            bufoff := !bufoff + (size lsl d_PHYS_SHIFT);
            num_pblks := !num_pblks - size;

            (*
            ** subsequent I/O starts at beginning of a partition 
            *)
            pstartblk := 0;  
            incr partn;
        done;

        std_OK
    end
    with
        | Error err -> err

(*
** DISK_INFO
**
**      This routine returns in 'buf' a pointer to an array 
**      containing the disk partition startblock and size for each
**      physical disk partition comprising a virtual disk.  The information
**      was calculated at boot time and stored in network byte order at
**      initialisation time.
**      hdr.h_size is modified.
*)

let vdisk_info ~hdr ~buf ~vdisks =
    let prv = hdr.h_priv in
    let cmd = hdr.h_command in

    let vlen = Array.length vdisks in
    (*
    ** Validate capability 
    *)
    let Objnum vdiskn = prv_number prv in
    if vdiskn < 0 ||
       vdiskn > vlen ||
       vdisks.(vdiskn) = None ||
       not (let vdp = match vdisks.(vdiskn) with
                      | Some vdp -> vdp;
                      | None -> failwith "progerr" in
            prv_decode prv vdp.v_cap.cap_priv.prv_random)
        then std_CAPBAD
    else
    try
    begin
        (*
        ** Check operation and rights
        *)
        (
            match cmd with
            | cmd when cmd = disk_INFO -> 
                let Rights_bits rights = prv_rights prv in
                let Rights_bits req = disk_RGT_READ in
                if (rights land req) <> req 
                    then raise (Error std_DENIED);
            | _ -> raise (Error std_COMBAD);
        );

        let vdp = match vdisks.(vdiskn) with
                      | Some vdp -> vdp;
                      | None -> failwith "progerr" in        

        let pos = ref 0 in
        for i = 0 to vdp.v_many - 1
        do
            let vp = vdp.v_parts.(i) in
            let pos' = buf_put_int32 buf !pos 0 in   (* cntrl unit *)
            let pos' = buf_put_int32 buf pos' vp.i_firstblk in
            let pos' = buf_put_int32 buf pos' vp.i_numblks in
            pos := pos';
        done;    
        hdr.h_size <- !pos;
        std_OK
    end
    with
        | Error err -> hdr.h_size <- 0; err

(*
** DISK_SIZE
**
**      Returns STD_CAPBAD if the capability was invalid or referred to a
**      virtual disk with size <= 0.  Otherwise it returns STD_OK and
**      returns in "maxblocks" the maximum number of virtual blocks of size
**      2^"l2vblksz" that fit on the virtual disk specified by "priv".  
**      Returns status and size.
**
*)

let vdisk_size ~hdr ~vblksz ~vdisks =
    let l2vblksz = let rec iter v n =
                        if v > 0 
                            then iter (v lsr 1) (n+1)
                            else (n-1) in
                       iter vblksz 0 
            in
    let prv = hdr.h_priv in
    let cmd = hdr.h_command in

    let vlen = Array.length vdisks in
    (*
    ** Validate capability 
    *)
    let Objnum vdiskn = prv_number prv in

    if vdiskn < 0 ||
       vdiskn > vlen ||
       vblksz < d_PHYSBLKSZ ||
       vdisks.(vdiskn) = None ||
       not (let vdp = match vdisks.(vdiskn) with
                      | Some vdp -> vdp;
                      | None -> failwith "progerr" in
            prv_decode prv vdp.v_cap.cap_priv.prv_random)
        then std_CAPBAD,0
    else
    try
    begin
        (*
        ** Check operation and rights
        *)
        (
            match cmd with
            | cmd when cmd = disk_SIZE -> 
                let Rights_bits rights = prv_rights prv in
                let Rights_bits req = disk_RGT_READ in
                if (rights land req) <> req 
                    then raise (Error std_DENIED);
            | _ -> raise (Error std_COMBAD);
        );

        let vdp = match vdisks.(vdiskn) with
                      | Some vdp -> vdp;
                      | None -> failwith "progerr" in        

        let scale = l2vblksz - d_PHYS_SHIFT in
        let numblks = vdp.v_numblks lsr scale in
        std_OK,numblks
    end
    with
        | Error err -> err,0
     

(*
** DISK_GETGEOMETRY
**
** Because the geometry information in 386/AT bus machines is not on the
** disk but in eeprom the fdisk program cannot get at it.  Therefore we
** provide this ugly hack to let it get it.
** hdr.h_size is modified.
*)
let vdisk_getgeom ~hdr ~buf ~vdisks =
    let prv = hdr.h_priv in
    let cmd = hdr.h_command in

    let vlen = Array.length vdisks in
    (*
    ** Validate capability 
    *)
    let Objnum vdiskn = prv_number prv in

    if vdiskn < 0 ||
       vdiskn > vlen ||
       vdisks.(vdiskn) = None ||
       not (let vdp = match vdisks.(vdiskn) with
                      | Some vdp -> vdp;
                      | None -> failwith "progerr" in
            prv_decode prv vdp.v_cap.cap_priv.prv_random)
        then std_CAPBAD
    else
    try
    begin
        (*
        ** Check operation and rights
        *)
        (
            match cmd with
            | cmd when cmd = disk_GETGEOMETRY -> 
                let Rights_bits rights = prv_rights prv in
                let Rights_bits req = disk_RGT_READ in
                if (rights land req) <> req 
                    then raise (Error std_DENIED);
            | _ -> raise (Error std_COMBAD);
        );

        (*
        ** get the geometry of the first partition used by the vdisk 
        *)

        let vdp = match vdisks.(vdiskn) with
                      | Some vdp -> vdp;
                      | None -> failwith "progerr" in        

        let vp = vdp.v_parts.(0) in
        let pp = match vp.i_physpart with
                 | Some pp -> pp;
                 | None -> failwith "disk_getgeom: inconsistent virtual disk - no phys. part.";
               in
        let label = pp.label in
        let geom = label.d_geom in
        let dev = pp.dev in
        (* 
        ** if the geometry hasn't been filled in yet, we try to fill it in 
        *)
        if geom.g_numcyl = (int16 0) then
        begin
            let dg = dev.dev_geom in
            geom.g_numcyl <- int16 dg.dev_numcyl;
            geom.g_numhead <- int16 dg.dev_numhead;
            geom.g_numsect <- int16 dg.dev_numsect;
        end;
        let bpt = int16 ((to_int geom.g_numsect) * d_PHYSBLKSZ) in
        let bps = int16 d_PHYSBLKSZ in
        
        let pos = buf_put_mach buf 0 bpt in
        let pos = buf_put_mach buf pos bps in
        let pos = buf_put_mach buf pos geom.g_numcyl in
        let pos = buf_put_mach buf pos (int16 0) in
        let pos = buf_put_mach buf pos geom.g_numhead in
        let pos = buf_put_mach buf pos geom.g_numsect in
        hdr.h_size <- pos;

        std_OK
    end
    with
        | Error err -> hdr.h_size <- 0; err

(*     
**  DISK_STD_INFO
**
**      Returns standard string describing the disk server object.
**      This includes the size of the disk in kilobytes.
**      hdr.h_size is modified.
*)
let vdisk_std_info ~hdr ~buf ~vdisks =
    (*
    ** see if the disk is valid and get its size in kilo byte units
    *)
    hdr.h_command <- disk_SIZE;
    let stat,size = vdisk_size hdr 1024 vdisks in
    if stat = std_OK then
    begin
        let pos = buf_put_string buf 0 
                    (sprintf "@ %8d KB" size) in
        hdr.h_size <- pos;
        std_OK 
    end
    else
        stat


(*     
**  DISK_STD_STATUS
**
**      Returns status string describing the disk server object.
**      Because we tell secrets, full rights are required!
**      hdr.h_size is modified.
*)
let vdisk_std_status ~hdr ~buf ~vdisks =
    (*
    ** see if the disk is valid and get its size in kilo byte units
    *)

    (*
    ** Validate capability 
    *)
    let prv = hdr.h_priv in
    let rights = prv_rights prv in
    let Objnum vdiskn = prv_number prv in
    let vlen = Array.length vdisks in

    if vdiskn < 0 ||
       vdiskn > vlen ||
       vdisks.(vdiskn) = None ||
       not (let vdp = match vdisks.(vdiskn) with
                      | Some vdp -> vdp;
                      | None -> failwith "progerr" in
            (prv_decode prv vdp.v_cap.cap_priv.prv_random) &&
            rights = prv_all_rights)
        then std_DENIED
    else
    begin
        let str = print_vdisk_table vdisks in
        let pos = buf_put_string buf 0 str in
        hdr.h_size <- pos;
        std_OK 
    end

(*
**  DISK_STD_RESTRICT
**
**      The following implements the STD_RESTRICT command
**      There is only one rights bit anyway so it isn't too exciting.
**      The new private is stored in hdr.h_priv.
*)

let vdisk_std_restrict ~hdr ~rights_mask ~vdisks =
    let prv = hdr.h_priv in
    let cmd = hdr.h_command in

    let vlen = Array.length vdisks in
    (*
    ** Validate capability 
    *)
    let Objnum vdiskn = prv_number prv in
    if vdiskn < 0 ||
       vdiskn > vlen ||
       vdisks.(vdiskn) = None ||
       not (let vdp = match vdisks.(vdiskn) with
                      | Some vdp -> vdp;
                      | None -> failwith "progerr" in
            prv_decode prv vdp.v_cap.cap_priv.prv_random)
        then std_CAPBAD
    else
    try
    begin
        (*
        ** Check operation and rights
        *)
        (
            match cmd with
            | cmd when cmd = std_RESTRICT -> 
                let Rights_bits rights = prv_rights prv in
                let Rights_bits req = disk_RGT_READ in
                if (rights land req) <> req 
                    then raise (Error std_DENIED);
            | _ -> raise (Error std_COMBAD);
        );

        let vdp = match vdisks.(vdiskn) with
                      | Some vdp -> vdp;
                      | None -> failwith "progerr" in        

        let Rights_bits rights = prv_rights prv in
        let rights' = rights land rights_mask in

        let prv' = prv_encode ~obj:(Objnum vdiskn)
                               ~rights:(Rights_bits rights')
                               ~rand:vdp.v_cap.cap_priv.prv_random in
        hdr.h_priv <- prv';
        std_OK
    end
    with
        | Error err -> hdr.h_size <- 0; err

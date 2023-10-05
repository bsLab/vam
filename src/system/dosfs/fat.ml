
(* 
** This file is part of the FIREBALL AMOEBA System.
**
** Written by:
**              Stefan Bosse
**              sbosse@physik.uni-bremen.de
**
** Last modified:
**              20/03/2003
**
** Changes:
**
**
**
** FIREBALL AMOEBA is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public License as 
** published by the Free Software Foundation; version 2.
**
** The FIREBALL AMOEBA is distributed in the hope that it will be usefull,
** but WITHOUT ANY WARRANTY; without even implied warranty of 
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
** General Public License for more details.
**
*) 


let version = "1.01"

open Amoeba
open Stderr
open Disk
open Unix
open Bytebuf

module String = StringLabels


(*
** DOS file system module
*)

let bit n = 1 lsl n 

(*
** Supported FAT types
*)

type fat_Type = Fat12 | Fat16 | Fat32

(*
** DOS Boot Block info structure
*)


type boot_info =
{
    doscap: capability;     (* disk drive capability *)

    oem_name: string;       (* OEM name and number *)

    vol_size:int;           (* Number of logical sectors in the vol *) 
    sector_size:int;        (* Bytes per sector    *)
    track_size:int;         (* Sectors per track   *)
    num_heads:int;          (* number of heads     *)           
      
    cluster_size:int;       (* Sector per cluster  *)
    fat_size:int;           (* Sectors per FAT     *)
    fat_type:fat_Type;

    reserved_sect:int;      (* reserved sectors    *)
    num_fats:int;           (* number of FATs      *)
    num_root_entr:int;      (* Number of root directory entries *)

    hidden_sect:int;        (* Number of hidden sectors *)

    media_type:string*int*int;         (* Medium descriptor byte 
                                       ** -> id string("Floppy"/"HardDisk", 
                                       **    number of tracks, 
                                       **    sect_per_tracks
                                       *)

    vol_serial:int;         (* Volume serial number     *)
    vol_label:string;       (* Volume label (11 bytes)  *)
    fs_id_str:string;       (* Filesystem ID string     *)



}


(*
** File attributes
*)

let attr_READ_ONLY  = bit 0
let attr_HIDDEN     = bit 1
let attr_SYSTEM     = bit 2
let attr_VOLUME_ID  = bit 3
let attr_DIRECTORY  = bit 4
let attr_ARCHIVE    = bit 5
let attr_LONG_NAME  = attr_READ_ONLY lor 
                      attr_HIDDEN    lor
                      attr_SYSTEM    lor
                      attr_VOLUME_ID    

exception Invalid_Fat

(*
** Directory entry: holds directories and files !
*)


let dir_ENTRY_SIZE = 32 

type dir_entry = {

    mutable dir_short_name:string;      (* Short name                   *)
    mutable dir_long_name:string;       (* Long VFAT name               *)

    mutable dir_attr:int;               (* File attributes              *)
    mutable dir_file_size:int;          (* File size                    *)

    mutable dir_cluster_num:int;        (* Size in clusters             *)
    mutable dir_first_cluster:int;      (* First cluster number of file *)
    mutable dir_cluster_list:int array; (* Complete cluster list        *)

    mutable dir_time_create:tm;         (* Creation time                *)
    mutable dir_time_lastacc:tm;        (* Last access date             *)
    mutable dir_time_write:tm;          (* Last Write time              *)

    (* For the case this is a directory: hold the sub directory list *)
    mutable dir_sub:(string,dir_entry) Hashtbl.t;

    mutable dir_sub_num:int;            (* number of entries in the hash *)

      
} 


(*
** some dummies
*)

let nilhash = Hashtbl.create 1

let nilbootinfo =
    {
        doscap          = nilcap;
        oem_name        = "" ;
        vol_size        = 0;
        sector_size     = 0;
        track_size      = 0;
        num_heads       = 0;

        cluster_size    = 0;
        fat_size        = 0;
        fat_type        = Fat16;
        reserved_sect   = 0;
        num_fats        = 0;
        num_root_entr   = 0;        

        hidden_sect     = 0;
        media_type      = ("",0,0);

        vol_serial      = 0;
        vol_label       = "";
        fs_id_str       = "";
    }

let niltm = 
    {
        tm_sec      = 0;
        tm_min      = 0;
        tm_hour     = 0;
        tm_mday     = 0;
        tm_mon      = 0;
        tm_year     = 0;
        tm_wday     = 0;
        tm_yday     = 0;
        tm_isdst    = false;
    }

let nildir = 
    
    {
        dir_short_name      = "";
        dir_long_name       = "";

        dir_attr            = 0;
        dir_file_size       = 0;

        dir_cluster_num     = 0;
        dir_first_cluster   = 0;
        dir_cluster_list    = [||];

        dir_time_create     = niltm;
        dir_time_lastacc    = niltm;
        dir_time_write      = niltm;
      
        dir_sub             = nilhash;
        dir_sub_num         = -1;
    } 

(*
** Read the boot sector of a DOS partition (logical block 0). Cap is the 
** partition capability.
*)

let read_dosboot pcap =
    let buf = buf_create 512 in     (* always 512 bytes block *)
    let err = disk_read pcap 
                        ~start:0
                        ~num:1
                        ~blksize:512
                        ~buf:buf    
                        ~pos:0 
    in

    if (err <> std_OK) then
        (err,nilbootinfo)
    else
    begin
        let bstr = string_of_buf buf  in
        
        let oemname = String.sub bstr 3 8 in

        let totsect16  = (
                        (int_of_char bstr.[0x13]) lor
                        ((int_of_char bstr.[0x14]) lsl 8)
                      ) in
        let totsect32  = (
                        (int_of_char bstr.[0x20]) lor
                        ((int_of_char bstr.[0x21]) lsl 8) lor
                        ((int_of_char bstr.[0x22]) lsl 16) lor
                        ((int_of_char bstr.[0x23]) lsl 24)
                      ) in

        let sectorsize  = (
                        (int_of_char bstr.[0xb]) lor
                        ((int_of_char bstr.[0xc]) lsl 8)
                        ) in

        let tracksize  = (
                        (int_of_char bstr.[0x18]) lor
                        ((int_of_char bstr.[0x19]) lsl 8)
                        ) in

        let numheads  = (
                        (int_of_char bstr.[0x1a]) lor
                        ((int_of_char bstr.[0x1b]) lsl 8)
                        ) in


        let clustersize = (int_of_char bstr.[0xd]) in

        let fatsize16  = (
                        (int_of_char bstr.[0x16]) lor
                        ((int_of_char bstr.[0x17]) lsl 8)
                      ) in
        let fatsize32  = (
                        (int_of_char bstr.[0x24]) lor
                        ((int_of_char bstr.[0x25]) lsl 8) lor
                        ((int_of_char bstr.[0x26]) lsl 16) lor
                        ((int_of_char bstr.[0x27]) lsl 24) 
                      ) in

        let reservedsect  = (
                        (int_of_char bstr.[0xe]) lor
                        ((int_of_char bstr.[0xf]) lsl 8)
                      ) in

        let numfats = (int_of_char bstr.[0x10]) in

        let numrootentr  = (
                        (int_of_char bstr.[0x11]) lor
                        ((int_of_char bstr.[0x12]) lsl 8)
                          ) in

        let hiddensect  = (
                        (int_of_char bstr.[0x1c]) lor
                        ((int_of_char bstr.[0x1d]) lsl 8)
                         ) in

        let mediatype = (
                            let md = int_of_char bstr.[0x15] in
                            match md with
                            | 0xf0 -> ("Floppy",80,18);
                            | 0xf8 -> ("HardDisk",0,0);
                            | 0xf9 -> ("Floppy",80,15);
                            | 0xfa -> ("Floppy",80,8);
                            | 0xfb -> ("Floppy",80,8);
                            | 0xfc -> ("Floppy",40,9);
                            | 0xfd -> ("Floppy",40,9);
                            | 0xfe -> ("Floppy",40,8);
                            | 0xff -> ("Floppy",40,8);
                            | _    -> ("Unknown",0,0);
                        ) in

        (*
        ** Determine NOW the FAT type -> follows MS specifications ! 
        *)

        let rootdirsectors = ( (numrootentr * dir_ENTRY_SIZE) + 
                               (sectorsize - 1)) /
                               sectorsize in

        let fatsize = if (fatsize16 <> 0) then
                        fatsize16
                      else 
                        fatsize32
        in
        let totsect = if (totsect16 <> 0) then
                        totsect16
                      else
                        totsect32
        in

        let datasect = totsect -
                       ( reservedsect + 
                         (numfats * fatsize) +
                         rootdirsectors 
                       ) in 
        let clustercnt = datasect / clustersize in
        
        let fattype    = if (clustercnt < 4085) then
                            Fat12
                       else if (clustercnt < 65525) then
                            Fat16
                       else
                            Fat32

        in


        let volserial = ( if (fattype = Fat12 || fattype = Fat16) then
                             (
                                (int_of_char bstr.[0x27]) lor
                                ((int_of_char bstr.[0x28]) lsl 8) lor
                                ((int_of_char bstr.[0x29]) lsl 16) lor
                                ((int_of_char bstr.[0x30]) lsl 24)
                             )                               
                           else
                             (
                                (int_of_char bstr.[0x43]) lor
                                ((int_of_char bstr.[0x44]) lsl 8) lor
                                ((int_of_char bstr.[0x45]) lsl 16) lor
                                ((int_of_char bstr.[0x46]) lsl 24)
                             ) 
                         ) in
        
        let vollabel = ( if (fattype = Fat12 || fattype = Fat16) then
                                String.sub bstr ~pos:0x2b
                                                ~len:11
                          else
                                String.sub bstr ~pos:0x47
                                                ~len:11
                        ) in
        let fsidstr  = ( if (fattype = Fat12 || fattype = Fat16) then
                                String.sub bstr ~pos:0x36
                                                ~len:8
                          else
                                String.sub bstr ~pos:0x52
                                                ~len:8
                        ) in
          
        let bi = {
            doscap          = pcap;
            oem_name        = oemname ;
            vol_size        = totsect;
            sector_size     = sectorsize;
            track_size      = tracksize;
            num_heads       = numheads;

            cluster_size    = clustersize;
            fat_size        = fatsize;
            fat_type        = fattype;
            reserved_sect   = reservedsect;
            num_fats        = numfats;
            num_root_entr   = numrootentr;        

            hidden_sect     = hiddensect;
            media_type      = mediatype;

            vol_serial      = volserial;
            vol_label       = vollabel;
            fs_id_str       = fsidstr;

        } in
        (std_OK,bi)
    end

(*
** Read the first FAT.
*)

let read_dosfat bs =
    let cap = bs.doscap in
    let sec = bs.reserved_sect  in
    let num = bs.fat_size in
    let buf = buf_create (num * bs.sector_size) in
    
    let err = disk_read cap
                        ~start:sec
                        ~num:num
                        ~blksize:bs.sector_size
                        ~buf:buf
                        ~pos:0
    in
    if (err <> std_OK) then
        (err,[| |])
    else
    begin
    
        let fatsize = (bs.fat_size * bs.sector_size * 2) /
                      ( if bs.fat_type = Fat12 then 3 else 4) 
        in 

        let fat = Array.create fatsize 0 in

        (*
        ** Convert the FAT buffer (either 12 Bit or 16 Bit for one
        ** cluster) to the fat array
        *)
        let bstr = string_of_buf buf in
        
        if (bs.fat_type = Fat12) then
        begin
            for i = 0 to fatsize -1  
            do
                let bpos = i + (i / 2) in
                let dw = (int_of_char bstr.[bpos]) lor
                         ((int_of_char bstr.[bpos+1]) lsl 8) in

                fat.(i) <- ( if ((i land 1) <> 0) then
                                (dw lsr 4)
                             else
                                (dw land 0xfff)
                           );
            done
        end
        else if (bs.fat_type = Fat16) then
        begin
            for i = 0 to fatsize - 1
            do
                fat.(i) <- (int_of_char bstr.[i*2]) lor
                           ((int_of_char bstr.[(i*2)+1]) lsl 8);
            done
        end;        
        (std_OK,fat)
    end


(*
** Convert unicode to ascii string
*)

let ascii_of_unicode str =
    let strlen = ( try ( ((String.index str '\255') ) / 2) with 
                   Not_found -> ((String.length str) / 2);
                 ) in

    (* remove string end marker: '\000\000' if any ! *)
    let endmark = ( try ( 
                            ignore(String.index str '\255');
                            1
                        ) 
                    with 
                        Not_found -> 0;
                  ) in


    let ascstr = String.create (strlen-endmark) in
    
    for i = 0 to strlen - 1 - endmark
    do
        ascstr.[i] <- str.[i*2];
    done;
    ascstr

(*
** Compact 8.3 name string
*)

let compact_83_name str =
    let len_main  = try (
                          let pos = (String.index str ' ') in
                          if (pos > 7) then
                                8
                          else
                                pos
                        ) with Not_found -> 8
    in
    let len_suff = try (
                          (String.index_from str 8 ' ') - 8 
                        ) with Not_found -> 3
    in    
    
    if (len_suff > 0) then
    (
        (String.sub str ~pos:0 ~len:len_main)^
        "."^
        (String.sub str ~pos:8 ~len:len_suff)
    )
    else
        (String.sub str ~pos:0 ~len:len_main)

(*
** Extract generic directory or longname slot sequence
** from buffer.
** Returns (nextentry,dir) 
*)

(*
** Directory inconsistency
*)

exception Invalid_Dir



let buf_get_dosdir buf pos n bs =
    let fattype = bs.fat_type in
    

    if ((buf_len buf) < pos + dir_ENTRY_SIZE) then
        failwith "buf_get_dosdir: out of bound buffer access";

    let bstr = buf_tostring buf pos dir_ENTRY_SIZE in

    let dirattr = (int_of_char bstr.[11]) in


    if (dirattr <> attr_LONG_NAME && bstr.[0] <> '\229') then
    begin    
        (*
        ** old 8.3 style entry; simple 
        *)

        let time_write = (
                            let ts =
                                (int_of_char bstr.[22]) lor
                                ((int_of_char bstr.[23]) lsl 8) in
                            let ds =
                                (int_of_char bstr.[24]) lor
                                ((int_of_char bstr.[25]) lsl 8) in

                            {
                                tm_sec  = ts land 0x1f;
                                tm_min  = (ts lsr 5) land 0x3f;
                                tm_hour = (ts lsr 11) land 0x1f;
                                tm_mday = (ds land 0x1f);
                                tm_mon  = (ds lsr 5) land 0xf;
                                tm_year = ((ds lsr 9) land 0x1f) + 80;
                                tm_wday = 0;    (* TODO *)
                                tm_yday = 0;    (* TODO *)
                                tm_isdst = false;
                            }
                     ) in

        let shortname = String.sub bstr ~pos:0 ~len:11 in
        let fsize =          (int_of_char bstr.[28]) lor
                            ((int_of_char bstr.[29]) lsl 8) lor
                            ((int_of_char bstr.[30]) lsl 16) lor
                            ((int_of_char bstr.[31]) lsl 24) 
        in

        Db.Pr.ss 2 "buf_get_dosdir [8.3]" shortname;

        (n+1),
        {
            dir_short_name = shortname;
            dir_long_name = compact_83_name shortname;

            dir_attr = dirattr;

            dir_file_size = fsize;

            dir_cluster_num = if (fsize > 0) then
                                    (fsize + bs.sector_size * bs.cluster_size) 
                                    / (bs.sector_size * bs.cluster_size)
                              else
                                    0;


            dir_first_cluster =  (
                                (int_of_char bstr.[26]) lor
                                ((int_of_char bstr.[27]) lsl 8) lor
                                ((int_of_char bstr.[20]) lsl 16) lor
                                ((int_of_char bstr.[21]) lsl 24) 
                             );

            (* read the cluster list later *)
            dir_cluster_list = [||];

            dir_time_write = time_write;

            dir_time_create = ( 
                            if (fattype <> Fat32) then
                                time_write
                            else
                            (
                                let ts =
                                    (int_of_char bstr.[14]) lor
                                    ((int_of_char bstr.[15]) lsl 8) in
                                let ds =
                                    (int_of_char bstr.[16]) lor
                                    ((int_of_char bstr.[17]) lsl 8) in

                                {
                                    tm_sec  = ts land 0x1f;
                                    tm_min  = (ts lsr 5) land 0x3f;
                                    tm_hour = (ts lsr 11) land 0x1f;
                                    tm_mday = ds land 0x3f;
                                    tm_mon  = (ds lsr 6) land 0xf;
                                    tm_year = (ds lsr 10) land 0x3f;
                                    tm_wday = ts;    (* TODO *)
                                    tm_yday = ds;    (* TODO *)
                                    tm_isdst = false;
                                }
                            );
                          );

            dir_time_lastacc = (
                            if (fattype <> Fat32) then
                                time_write
                            else
                            (
                                let ds =
                                    (int_of_char bstr.[16]) lor
                                    ((int_of_char bstr.[17]) lsl 8) in
                                let ts = 0 in
                                {
                                    tm_sec  = ts land 0x1f;
                                    tm_min  = (ts lsr 5) land 0x3f;
                                    tm_hour = (ts lsr 11) land 0x1f;
                                    tm_mday = ds land 0x3f;
                                    tm_mon  = (ds lsr 6) land 0xf;
                                    tm_year = (ds lsr 10) land 0x3f;
                                    tm_wday = 0;    (* TODO *)
                                    tm_yday = 0;    (* TODO *)
                                    tm_isdst = false;
                                }
                            );
                           );

                dir_sub = nilhash;
                dir_sub_num = -1;

            }
    end
    else if (bstr.[0] <> '\229') then
    begin
        (*
        ** It's a start of a long name slot sequence
        *)

        let longname    = ref ((String.sub bstr ~pos:1  ~len:10) ^
                               (String.sub bstr ~pos:14 ~len:12) ^
                               (String.sub bstr ~pos:28 ~len:4))
        in

        let id          = ref (int_of_char bstr.[0]) in


        (*
        ** Do we have only one longname dir slot ?
        *)

        Db.Pr.sd 2 "buf_get_dosdir (first id)" (!id);

        let single_dir_entry =
            if (!id = 0x41) then
                true
            else
                false
        in

        let checksum    = (int_of_char bstr.[13]) in

        if ((!id land 0x40) = 0) then
            raise Invalid_Dir
        else
            id := !id land 0x3f;

        let nl          = ref (n+1) in
        let last        = ref false in
        let bstr        = ref bstr in

        (*
        ** Loop through the list of all directory entry chains
        ** building the longname.
        *)

        if (single_dir_entry = false) then
        while ( 
                (* !nl < bs.num_root_entr && ???????????????? *) 
                !last = false
              )
        do
            let bpos = (!nl * dir_ENTRY_SIZE) in
            bstr := buf_tostring buf bpos dir_ENTRY_SIZE;

            let dirattr = (int_of_char !bstr.[11]) in

        
            if (dirattr = attr_LONG_NAME) then
            begin
                (* some consistency checks TODO *)
                let nextid      = (int_of_char !bstr.[0]) in
                let nextchksum  = (int_of_char !bstr.[13]) in

                if (!id <> nextid + 1 ) then
                    raise Invalid_Dir;

                if (checksum <> nextchksum) then
                    raise Invalid_Dir;

                id := nextid; 

                longname :=   ((String.sub !bstr ~pos:1  ~len:10) ^
                               (String.sub !bstr ~pos:14 ~len:12) ^
                               (String.sub !bstr ~pos:28 ~len:4)) ^
                              !longname;

                incr nl;
            end       
            else if (!bstr.[0] <> '\000' && !bstr.[0] <> '\229') then
            begin
                (* some consistency checks TODO *)
                last := true ;
            end
            else
                (* internally inconsistency *)
                raise Invalid_Dir;
        done
        else
        begin
            (*
            ** The next one is the generic 8.3 dir entry 
            *)

            let bpos = (!nl * dir_ENTRY_SIZE) in
            bstr := buf_tostring buf bpos dir_ENTRY_SIZE;
        end;
        

        let time_write = (
                            let ts =
                                (int_of_char !bstr.[22]) lor
                                ((int_of_char !bstr.[23]) lsl 8) in
                            let ds =
                                (int_of_char !bstr.[24]) lor
                                ((int_of_char !bstr.[25]) lsl 8) in

                            {
                                tm_sec  = ts land 0x1f;
                                tm_min  = (ts lsr 5) land 0x3f;
                                tm_hour = (ts lsr 11) land 0x1f;
                                tm_mday = (ds land 0x1f);
                                tm_mon  = (ds lsr 5) land 0xf;
                                tm_year = ((ds lsr 9) land 0x1f) + 80;
                                tm_wday = 0;    (* TODO *)
                                tm_yday = 0;    (* TODO *)
                                tm_isdst = false;
                            }
                     ) in

        let dirshortname = String.sub !bstr ~pos:0 ~len:11 in

        begin
            let last_checksum = 
            (
                let sum = ref 0 in
                for i = 0 to 10 
                do
                    sum := (((!sum land 0x1) lsl 7) lor 
                            ((!sum land 0xfe) lsr 1)) + 
                           (int_of_char dirshortname.[i]);
                done;
                (!sum land 0xff)
            ) in

            if (checksum <> last_checksum) then
            begin
                Printf.printf "buf_get_dosdir: %s long name -> chksum_slots=%d chksum=%d\n" 
                              (ascii_of_unicode !longname) checksum last_checksum;
                raise Invalid_Dir;
            end;
        end;

        let fsize =          (int_of_char !bstr.[28]) lor
                            ((int_of_char !bstr.[29]) lsl 8) lor
                            ((int_of_char !bstr.[30]) lsl 16) lor
                            ((int_of_char !bstr.[31]) lsl 24) 
        in
        
        Db.Pr.ss 50 "buf_get_dosdir: long name" (ascii_of_unicode !longname);
        
        (!nl+1),
        {
                dir_short_name = dirshortname;
                dir_long_name = ascii_of_unicode !longname;

                dir_attr = (int_of_char !bstr.[11]);

                dir_file_size = fsize;

                dir_cluster_num = if (fsize > 0) then
                                    (fsize + bs.sector_size * bs.cluster_size) 
                                    / (bs.sector_size * bs.cluster_size)
                                  else
                                        0;
    
                dir_first_cluster =  (
                                (int_of_char !bstr.[26]) lor
                                ((int_of_char !bstr.[27]) lsl 8) lor
                                ((int_of_char !bstr.[20]) lsl 16) lor
                                ((int_of_char !bstr.[21]) lsl 24) 
                             );

                (* read the cluster list later *)
                dir_cluster_list = [||];


                dir_time_write = time_write;
    
                dir_time_create = ( 
                            if (fattype <> Fat32) then
                                time_write
                            else
                            (
                                let ts =
                                    (int_of_char !bstr.[14]) lor
                                    ((int_of_char !bstr.[15]) lsl 8) in
                                let ds =
                                    (int_of_char !bstr.[16]) lor
                                    ((int_of_char !bstr.[17]) lsl 8) in

                                {
                                    tm_sec  = ts land 0x1f;
                                    tm_min  = (ts lsr 5) land 0x3f;
                                    tm_hour = (ts lsr 11) land 0x1f;
                                    tm_mday = ds land 0x3f;
                                    tm_mon  = (ds lsr 6) land 0xf;
                                    tm_year = (ds lsr 10) land 0x3f;
                                    tm_wday = ts;    (* TODO *)
                                    tm_yday = ds;    (* TODO *)
                                    tm_isdst = false;
                                }
                            );
                          );

                dir_time_lastacc = (
                            if (fattype <> Fat32) then
                                time_write
                            else
                            (
                                let ds =
                                    (int_of_char !bstr.[16]) lor
                                    ((int_of_char !bstr.[17]) lsl 8) in
                                let ts = 0 in
                                {
                                    tm_sec  = ts land 0x1f;
                                    tm_min  = (ts lsr 5) land 0x3f;
                                    tm_hour = (ts lsr 11) land 0x1f;
                                    tm_mday = ds land 0x3f;
                                    tm_mon  = (ds lsr 6) land 0xf;
                                    tm_year = (ds lsr 10) land 0x3f;
                                    tm_wday = 0;    (* TODO *)
                                    tm_yday = 0;    (* TODO *)
                                    tm_isdst = false;
                                }
                            );
                           );

            dir_sub = nilhash;
            dir_sub_num = -1;
        }
    end
    else
        (*
        ** empty dir entry 
        *)
        (n+1,nildir)


(*
** Return the clusters allocated by a file or directory. Start is
** the first cluster. Num ist the number of expected clusters calculated
** from the filesize. For directories, num must be 0. Returns
** (number_of_clusters * cluster_array) tuple.
*)

let cluster_chain ~fat:fat ~bs:bs ~start:start ~num:num =
    let fattype = bs.fat_type in

    Db.Pr.sdd 2 "cluser_chain (start,num)" start num;

    if (num > 0) then
    begin
        (* it's a file of known size *)
        
        let clus_arr = Array.create num 0 in
        let fatpos   = ref start in

        clus_arr.(0) <- start;

        Db.Pr.sd 2 "cluster_chain:cluster num" num;
        Db.Pr.sd 5 "cluster_chain:next(start)" (fat.(!fatpos));

        for i = 1 to num - 1
        do
            let next = fat.(!fatpos) in
            if (fattype = Fat12 && next < 0xFF0 ||
                fattype = Fat16 && next < 0xFFF0) then
            begin
                clus_arr.(i) <- next;
                fatpos := next;
            end
            (* ???
            else
                raise Invalid_Fat;                
            *)
        done;

        (* now the next cluster must be the last one *)
        let next = fat.(!fatpos) in


        if (fattype = Fat12 && next < 0xFF8 ||
            fattype = Fat16 && next < 0xFFF8) then
        begin
            Db.Pr.sd 1 "cluster_chain:invalid next ???" next;
            (* raise Invalid_Fat; ?????????????????? *)
        end;
        (num,clus_arr)
    end
    else
    begin
        (* it's a directory *)
        let clustnum = ref 0 in

        let rec iter_fat cluster =
            Db.Pr.sd 3 "cluster_chain (curr)" cluster;
            (* sanity check for invalid clusters/fats *)
            incr clustnum;
            if (!clustnum > 200) then
                raise Invalid_Fat;
 
            let next = fat.(cluster) in


            if (fattype = Fat12 && next < 0xFF8 ||
                fattype = Fat16 && next < 0xFFF8) then
                [cluster ] @ (iter_fat next)
            else
                [ cluster ]
        in
        let clus_list = iter_fat start in
        let clus_arr = Array.of_list clus_list in
        let clus_num = Array.length clus_arr in
        (clus_num,clus_arr)        
    end

 

(*
** Read the Root directory.
**
** Returns:
**
** [ errstat * volid * directory ]
*)

let read_dosroot ~bootinfo:bs ~fat:fat =
    let hash = Hashtbl.create (bs.num_root_entr) in

    let rootdir = {
        dir_short_name  = "/";
        dir_long_name   = "/";
        dir_attr        = attr_DIRECTORY;
        dir_file_size   = 0;
        dir_cluster_num = 0;
        dir_first_cluster = 0;
        dir_cluster_list  = [||];
        dir_time_create   = niltm;
        dir_time_lastacc  = niltm;
        dir_time_write    = niltm;
        dir_sub           = hash;
        dir_sub_num       = 0;
    } in


    let cap = bs.doscap in
    let sec = bs.reserved_sect + (bs.num_fats * bs.fat_size) in
    let num = (bs.num_root_entr * dir_ENTRY_SIZE) / bs.sector_size  in
    let buf = buf_create (bs.num_root_entr * dir_ENTRY_SIZE) in

    Db.Pr.ss 2 "read_dosroot" "disk_read...";
    
    let err = disk_read cap
                        ~start:sec
                        ~num:num
                        ~blksize:bs.sector_size
                        ~buf:buf
                        ~pos:0
    in
    if (err <> std_OK) then
        (err,nildir,nildir)
    else
    begin
        
        let n    = ref 0 in
        let last = ref false in
        let dl   = ref [] in
        let volid = ref nildir in

        while (!n < bs.num_root_entr && !last = false)
        do
            Db.Pr.ss 2 "read_dosroot" "buf_get_dosdir:";

            (*
            ** Extracts the dir entry from the buffer. Returns nildir
            ** if dir entry is empty.
            *)
            let (nextn,dir) = buf_get_dosdir buf 
                                    (!n * dir_ENTRY_SIZE) 
                                    !n
                                    bs
            in
            n := nextn;

            if (dir <> nildir && dir.dir_short_name.[0] = '\000') then
                last := true        (* last entry in root dir *)
            else if (dir <> nildir) then 
            begin
                (* 
                ** It's a valid entry 
                *)

                Db.Pr.sss 2 "read_dosroot (short,long)" 
                        dir.dir_short_name
                        dir.dir_long_name;

                if (dir.dir_attr <> attr_VOLUME_ID)  then
                begin
                    (* 
                    ** A generic or longname  directory entry. Add it
                    ** to the content hash table.
                    *)


                    Hashtbl.add hash dir.dir_long_name dir;

                    (* resolve the cluster chain *)
                    let num,clc = cluster_chain ~fat:fat ~bs:bs 
                                    ~start:dir.dir_first_cluster
                                    ~num:dir.dir_cluster_num
                    in
                    dir.dir_cluster_list <- clc;

                    rootdir.dir_sub_num <- rootdir.dir_sub_num + 1;
                end
                else 
                    (* volume id entry *)
                    volid :=  dir;
            end

        done;
        (std_OK,!volid,rootdir)
    end

(*
** Read the a directory dir. The content is stored in the dir_sub 
** hash table.
**
** Returns:
**
** [ errstat ]
*)

let read_dosdir ~bootinfo:bs ~fat:fat ~dir:parent =

    parent.dir_sub_num <- 0;
    parent.dir_sub <- Hashtbl.create 100;

    let cap = bs.doscap in


    let num = Array.length parent.dir_cluster_list in

    let buf = buf_create (bs.sector_size * bs.cluster_size * num) in
    
    for i = 0 to num-1 
    do
        let sec = (* bs.hidden_sect + *)
              bs.reserved_sect +
              bs.num_fats * bs.fat_size +
              (bs.num_root_entr * dir_ENTRY_SIZE) / bs.sector_size +
              (parent.dir_cluster_list.(i) - 2) * bs.cluster_size 
        in
        let err = disk_read cap
                        ~start:sec
                        ~num:bs.cluster_size
                        ~blksize:bs.sector_size
                        ~buf:buf
                        ~pos:(i * bs.sector_size * bs.cluster_size)
        in
        if (err <> std_OK) then
            raise Invalid_Dir;
    done;

    begin
        
        let n    = ref 0 in
        let last = ref false in

        while ( (* !n < bs.num_root_entr &&  ??????????????? *)
                !last = false)
        do
            (*
            ** extract the dir entry from the buffer 
            *)
            let (nextn,dir) = buf_get_dosdir buf 
                                    (!n * dir_ENTRY_SIZE) 
                                    !n
                                    bs
            in
            n := nextn;

            if (dir <> nildir && dir.dir_short_name.[0] = '\000') then
                last := true        (* last entry in root dir *)
            else if (dir <> nildir) then 
            begin
                (* a valid entry *)
                if (dir.dir_attr <> attr_VOLUME_ID)  then
                begin
                    (* 
                    ** A generic or longname  directory entry. Add it
                    ** to the content hash table.
                    *)

                    Hashtbl.add parent.dir_sub dir.dir_long_name dir;

                    (*
                    ** Resolve the cluster chain. Be aware of
                    ** dirs like '.' and '..' -> start cluster = 0 ! 
                    *)
                    if(dir.dir_first_cluster > 0) then
                    begin
                        let num,clc = cluster_chain ~fat:fat ~bs:bs 
                                    ~start:dir.dir_first_cluster
                                    ~num:dir.dir_cluster_num
                        in
                        Db.Pr.sd 10 "read_dosdir: numcluster" num;
                        dir.dir_cluster_list <- clc;
                    end;

                    parent.dir_sub_num <- parent.dir_sub_num + 1;

                end;
            end

        done;
        std_OK 
    end

    
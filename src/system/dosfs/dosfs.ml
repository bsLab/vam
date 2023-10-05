(*
** User interface routines for DOS FAT filesystem access.
*)

let version = "1.01"

open Amoeba
open Disk
open Stderr
open Fat
open Bytebuf

(*
** Volume structure
*)

type dos_volume =
{
    mutable dos_bootinfo:boot_info;     (* The boot sector informations *)
    mutable dos_volumeid:dir_entry;     (* The volume id                *)
    mutable dos_rootdir:dir_entry;      (* The root directory           *)
    mutable dos_fat:int array;          (* The FAT                      *)
}

(*
** Init routine:
**
**  Read the boot sector, the rootdir, the volume descriptor, and
**  the fat table.
**
**  [ dos_volume  ] = vol_init doscap
**
**
*)

let vol_init doscap =

    let stat,boot = read_dosboot doscap in


    if (stat <> std_OK) then
        failwith ("dos_init: read_dosboot failed: "^
                  (err_why stat));

    let stat,fat = read_dosfat boot in

    if (stat <> std_OK) then
        failwith ("dos_init: read_dosfat failed: "^
                  (err_why stat));


    let stat,volid,rootdir = read_dosroot boot fat in

    if (stat <> std_OK) then
        failwith ("dos_init: read_dosroot failed: "^
                  (err_why stat));


 
    {
        dos_bootinfo = boot;
        dos_volumeid = volid;
        dos_rootdir = rootdir;
        dos_fat = fat;
    }


(*
** Read a directory. Lookup first the hash table. Returns the dir_entry
** structure of the directory. The path string must be clean. That means:
** no relative directories.
*)

let read_dir ~dosvol:dosvol ~path:path =
    let root = dosvol.dos_rootdir in
    let bs   = dosvol.dos_bootinfo in
    let fat  = dosvol.dos_fat in

    let pathl = Str.split (Str.regexp "/") path in

    let rec iter pl dir =
        match pl with 
        | hd::tl ->
            let hash = dir.dir_sub in
            let dir' = Hashtbl.find hash hd in
            if (dir'.dir_attr land attr_DIRECTORY <> attr_DIRECTORY) then
                raise Invalid_Dir;

            if (dir'.dir_sub_num = -1) then
            begin
                let stat = read_dosdir bs fat dir' in
                if (stat <> std_OK) then
                    raise Invalid_Dir;

            end;
            iter tl dir'
        | [] -> dir
            
    in                         
    iter pathl root

(*
** Invalidate a directory and remove it from the hash table.
*)

let invalidate_dir ~dosvol:dosvol ~path:path =
    let root = dosvol.dos_rootdir in
    let bs   = dosvol.dos_bootinfo in
    let fat  = dosvol.dos_fat in

    let pathl = Str.split (Str.regexp "/") path in

    let rec iter pl dir =
        match pl with 
        | hd::tl ->
            let hash = dir.dir_sub in
            let dir' = Hashtbl.find hash hd in
            if (dir'.dir_attr land attr_DIRECTORY <> attr_DIRECTORY) then
                raise Invalid_Dir;

            if (dir'.dir_sub_num = -1) then
            begin
                raise Invalid_Dir;
            end;
            iter tl dir'
        | [] -> dir
            
    in                         
    let dir = iter pathl root in
    dir.dir_sub_num <- -1

   

(*
** Return the content of a directory.
**
**  [ dir_entry list ]
**
*)


let list_dir dir =
    let dirlist = ref [] in

    if (dir.dir_sub_num < 0) then
        raise Invalid_Dir;

    Hashtbl.iter (fun key dir ->
        dirlist := !dirlist @ [dir];
    ) dir.dir_sub;

    !dirlist


  


(*
** Read all file clusters and use the user defined function f to
** write the clusters to a specified target.
**
** val f : buf:buffer -> pos:int -> len:int -> totlen:int -> unit
** 
** 
*)

let read_file ~dosvol:dosvol ~file:file ~func:f =
    let bs = dosvol.dos_bootinfo in

    let cap = bs.doscap in
    let bufsize = (bs.sector_size * bs.cluster_size) in
    let buf = buf_create bufsize in

    let dirname = Filename.dirname file in
    let filename = Filename.basename file in

    let dir = read_dir dosvol dirname in


    let file = Hashtbl.find dir.dir_sub filename in
    
    if (file.dir_attr land attr_DIRECTORY = attr_DIRECTORY) then
        raise Invalid_Dir;

    let n = file.dir_cluster_num in
    let pos = ref 0 in
    let totlen = file.dir_file_size in


    for i = 0 to n-1 
    do
        let num = min (bs.cluster_size)
                      ((totlen - !pos + 511)/bs.sector_size) in

        let len = min (num * bs.sector_size)
                      (totlen - !pos) in

        let sec = (* bs.hidden_sect + *)
              bs.reserved_sect +
              bs.num_fats * bs.fat_size +
              (bs.num_root_entr * dir_ENTRY_SIZE) / bs.sector_size +
              (file.dir_cluster_list.(i) - 2) * bs.cluster_size
        in

        Db.Pr.sdd 200 "read_file: start,num" sec num;

        let err = disk_read cap
                            ~start:sec
                            ~num:num
                            ~blksize:bs.sector_size
                            ~buf:buf
                            ~pos:0
        in
        if (err <> std_OK) then 
        begin
            Db.Pr.ss 2 "read_file: disk_read failed" (err_why err);
            raise Invalid_Dir;
        end;
        f buf !pos len totlen;
        pos := !pos + len;
    done



(*
** Copy from DOS to base filesystem 
*)
open Unix

let copy_fromdos ~dosvol:dosvol ~dosfile:df ~homefile:uf =
    let oc = open_out_bin uf in

    let f abuf fpos len totlen =
        let buf = string_of_buf abuf in 
        output oc buf 0 len;
    in
    read_file dosvol df f;
    close_out oc;

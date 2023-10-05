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
**    $INITIAL:     (C) 20042-2005 BSSLAB
**    $CREATED:     
**    $VERSION:     0.69
**
**    $INFO:
**
**  Kernel boot directory implementation.
**
**    $ENDOFINFO
**
*)




open Amoeba 
open Machtype
open Bytebuf
open Afs_client
open Disk_client
open Stderr  
open Stdcom
open Dir
open Printf
open Ar
open Cap_env
open Name
open Ksys
open Bootdir

open Vash_io
open Vash_env 
open Unix


let mkhost args =
  try
  begin
    let usage err =
            out "usage: mkhost [-h] \"0:0:0...MAC-address\" <filename>"; nl ();
            raise (Error err);
            in
    let mac = ref "" in
    let file = ref "" in

    let rec iter args =
        match args with
        | hd::tl -> 
        begin
            match hd with
            | "-h" -> usage std_OK;
            | _ -> 
            begin
                if hd.[0] = '-' then
                begin
                    out ("unknown option: "^hd); nl ();
                    usage std_ARGBAD; 
                end;
                if (!mac = "") then
                    mac := hd
                else if (!file = "") then
                    file := hd
                else
                    usage std_ARGBAD;
            end;
            iter tl;
        end;
        | [] -> ()
        in
    iter args;

    if (!mac = "" || !file = "") then
        usage std_ARGBAD;

    if !file.[0] <> '/' then
    begin
        match !work_dir with
        | Amoeba_path wd ->
                file := (wd^"/"^ !file);
        | Unix_path wd ->
                file := ("/unix/"^wd^"/"^ !file);
    end;
    
    let path = path_resolve !file in
    begin
        match path with
        | Amoeba_path path ->
        begin
            let stat,parent = if (path.[0] = '/') then
                                        dir_lookup ~root:!root_cap
                                                   ~name:(
                                            Filename.dirname path
                                        )
                                      else 
                                      if (Filename.dirname path = ".") 
                                        then
                                        std_OK,!work_cap
                                      else
                                        dir_lookup ~root:!work_cap
                                                   ~name:(
                                            Filename.dirname path
                                        )
                in
            if (stat <> std_OK) then
                raise (Error stat);
            let newcap = sys_hostcap !mac in

            let stat = dir_append ~root:parent
                                  ~name:(Filename.basename path)
                                  ~obj:newcap
                in
            if (stat <> std_OK) then
                raise (Error stat);
        end;
        | Unix_path path ->
        begin   
            raise (Error std_NOTNOW);
        end;
    end;
    std_OK
  end
  with
    | Error err -> err
    | Failure str -> out ("failed:"^str); nl (); std_SYSERR   

let kmesg args =
    let usage err =
            out "usage: kmesg [-h] <host1> [<host2> ...]"; nl ();
            raise (Error err);
            in
    let envnames = ref [] in
    let rec iter args =
        match args with
        | hd::tl -> 
        begin
            match hd with
            | "-h" -> usage std_OK;
            | _ -> 
            begin
                if hd.[0] = '-' then
                begin
                    out ("unknown option: "^hd); nl ();
                    usage std_ARGBAD; 
                end;
                (*
                ** Three possibilities:
                ** 1. absolute host path
                ** 2. relative to work_dir
                ** 3. host name (/hosts/...)
                *)
                let hostpath,path = 
                    if hd.[0] <> '/' then
                    begin
                        "/hosts/"^hd,       (* ??? *)
                        match !work_dir with
                        | Amoeba_path wd ->
                            (wd^"/"^hd);
                        | Unix_path wd ->
                            ("/unix/"^wd^"/"^hd);
                    end
                    else
                        "",hd in

                let path = path_resolve path in
                begin
                    match path with
                    | Amoeba_path path ->
                    begin
                        let stat,cap = dir_pathlookup path in
                        let cap =
                            if (stat <> std_OK) then
                            begin
                                let stat,cap = dir_pathlookup hostpath in
                                if (stat <> std_OK) then
                                    raise (Error stat);
                                cap
                            end
                            else
                                cap in

                        let stat,str = sys_printbuf cap  in
                        if (stat <> std_OK) then
                            raise (Error stat);
                        out (str); nl ();
                    end;
                    | Unix_path path ->
                    begin
                        out ("- Unix FS not supported"); nl ();
                    end;
                end;

                iter tl;
            end;
        end;
        | [] -> ()
        in
    try
    begin
        iter args;
        std_OK
    end
    with
        | Error err -> err


let reboot args =
  try
  begin
    let usage err =
            out "usage: reboot [-h] [-c <-XX>] [-f <bf>] <host> <kernelimage>"; nl ();
            out "       -c : command line string"; nl (); 
            out "       -f : boot flags (int value)"; nl (); 
            raise (Error err);
            in
    let host = ref "" in
    let file = ref "" in
    let clin = ref "kernel " in
    let bfla = ref 0 in

    let rec iter args =
        match args with
        | hd::tl -> 
        begin
            match hd with
            | "-h" -> usage std_OK;
            | "-c" -> 
            begin
                match tl with
                | hd::tl -> clin := !clin ^ hd; iter tl;
                | [] -> usage std_ARGBAD;
            end;        
            | "-f" -> 
            begin
                match tl with
                | hd::tl -> bfla := int_of_string hd; iter tl;
                | [] -> usage std_ARGBAD;
            end;        
            | _ -> 
            begin
                if hd.[0] = '-' then
                begin
                    out ("unknown option: "^hd); nl ();
                    usage std_ARGBAD; 
                end;
                if (!host = "") then
                    host := hd
                else if (!file = "") then
                    file := hd
                else
                    usage std_ARGBAD;
            end;
            iter tl;
        end;
        | [] -> ()
        in

    iter args;

    if (!host = "" || !file = "") then
        usage std_ARGBAD;

    (*
    ** Three possibilities:
    ** 1. absolute host path
    ** 2. relative to work_dir
    ** 3. host name (/hosts/...)
    *)
    let hostpath1,hostpath2 = 
        if !host.[0] <> '/' then
        begin
            "/hosts/"^ !host,       (* ??? *)
            match !work_dir with
            | Amoeba_path wd ->
                (wd^"/"^ !host);
            | Unix_path wd ->
                ("/unix/"^wd^"/"^ !host);
        end
        else
            "", !host in

    let filepath = 
        if !file.[0] <> '/' then
        begin
            match !work_dir with
            | Amoeba_path wd ->
                (wd^"/"^ !file);
            | Unix_path wd ->
                ("/unix/"^wd^"/"^ !file);
        end
        else
            !file in
    
    let file = path_resolve filepath in
    let host = path_resolve hostpath2 in

    match host with
    | Amoeba_path path ->
    begin
        let stat,hcap = dir_pathlookup path in

        let hcap = 
            if (stat <> std_OK) then
            begin
                let stat,hcap = dir_pathlookup hostpath1 in
                if (stat <> std_OK) then
                begin
                    out "hostcap lookup failed"; nl ();
                    raise (Error stat);
                end;
                hcap
            end
            else hcap in

        match file with
        | Amoeba_path path ->
        begin
            let stat,fcap = dir_pathlookup path in            
            if (stat <> std_OK) then
            begin
                out "filecap lookup failed"; nl ();
                raise (Error stat);
            end;
            let stat,str = std_info fcap 10 in
            if (stat <> std_OK) then
            begin
                out "std_info on filecap failed"; nl ();
                raise (Error stat);
            end;
            if (str.[0] <> '-') then
            begin
                out "kernelimage path is not a fileserver object"; nl ();
                raise (Error stat);
            end;
            
            let stat,syscap = syscap_lookup hcap in
            if (stat <> std_OK) then
            begin
                out "syscap lookup failed"; nl ();
                raise (Error stat);
            end;
        
            out ("syscap: "^(ar_cap syscap)); nl ();
            out ("filecap: "^ (ar_cap fcap)); nl ();
            out ("commandline: "^(!clin)); nl();
            
            let stat = sys_boot ~syscap:syscap
                                ~kernelcap:fcap
                                ~commandline:!clin
                                ~flags:!bfla
                in
            if (stat <> std_OK) then
            begin
                out "sys_boot failed"; nl ();
                raise (Error stat);
            end;
            std_OK
        end;
        | Unix_path path ->
        begin   
            std_NOTNOW
        end;
    end;
    | Unix_path path ->
    begin
        std_NOTNOW
    end;
  end
  with
    | Error err -> err
    | Failure str -> out ("failed:"^str); nl (); std_SYSERR   


let kstat args =
    let usage err =
            out "usage: kstat [-h] <host> <-arg> [<-arg2>,...]"; nl ();
            raise (Error err);
            in
    let host = ref "" in
    let statargs = ref [] in

    let rec iter args =
        match args with
        | hd::tl -> 
        begin
            match hd with
            | "-h" -> usage std_OK;
            | _ -> 
            begin
                if (!host = "" &&
                    hd.[0] <> '-') then
                    host := hd
                else if (hd.[0] = '-') then
                    statargs := !statargs @ [hd]
                else
                    usage std_ARGBAD;

                iter tl;
            end;
        end;
        | [] -> ()
    in

    try
    begin
        iter args;
        if (!host = "" || !statargs = []) then
            usage std_ARGBAD;

        (*
        ** Three possibilities:
        ** 1. absolute host path
        ** 2. relative to work_dir
        ** 3. host name (/hosts/...)
        *)
        let hostpath1,hostpath2 = 
            if !host.[0] <> '/' then
            begin
                "/hosts/"^ !host,       (* ??? *)
                match !work_dir with
                | Amoeba_path wd ->
                    (wd^"/"^ !host);
                | Unix_path wd ->
                    ("/unix/"^wd^"/"^ !host);
            end
            else
                "", !host in

        let path = path_resolve hostpath2 in
        match path with
        | Amoeba_path path ->
        begin
            let stat,cap = dir_pathlookup path in
            let cap = 
                if (stat <> std_OK) then
                begin
                    let stat,cap = dir_pathlookup hostpath1 in
                    if stat <> std_OK then
                        raise (Error stat);
                    cap
                end
                else cap in

            let stat,str = sys_kstat cap !statargs in
            if (stat <> std_OK) then
                raise (Error stat);
            out (str); nl ();
            std_OK
        end;
        | Unix_path path ->
        begin
            out ("- Unix FS not supported"); nl ();
            std_OK
        end;
    end
    with
        | Error err -> err



let installk args =
  try
  begin
    let usage err =
            out "usage: installk [-h -f -i] <vdisk> <name>:<kernelimage> [...]"; nl ();
            out "       -f : floppy disk (and only floppy!)"; nl ();
            out "       -i : initial installation (no magic check)"; nl();
            raise (Error err);
            in
    let vdisk = ref "" in
    let kernels = ref [] in
    let floppy = ref false in
    let init = ref false in

    let rec iter args =
        match args with
        | hd::tl -> 
        begin
            match hd with
            | "-h" -> usage std_OK;
            | "-f" -> floppy := true; iter tl;
            | "-i" -> init := true; iter tl;
            | _ -> 
            begin
                if hd.[0] = '-' then
                begin
                    out ("unknown option: "^hd); nl ();
                    usage std_ARGBAD; 
                end;
                if (!vdisk = "") then
                    vdisk := hd
                else
                    kernels := !kernels @ [(
                                let name_path = Str.split (Str.regexp ":")
                                                    hd in
                                match name_path with
                                | hd::tl ->
                                begin
                                            hd,
                                            (match tl with
                                            | hd'::tl -> hd';
                                            | [] -> usage std_ARGBAD);
                                end;
                                | [] -> usage std_ARGBAD;
                            )];
            end;
            iter tl;
        end;
        | [] -> ()
        in

    iter args;

    if (!vdisk = "" || !kernels = []) then
        usage std_ARGBAD;

        (*
        ** Two possibilities:
        ** 1. absolute host path
        ** 2. relative to work_dir
        *)
        let vdiskpath = 
            if !vdisk.[0] <> '/' then
            begin
                match !work_dir with
                | Amoeba_path wd ->
                    (wd^"/"^ !vdisk);
                | Unix_path wd ->
                    ("/unix/"^wd^"/"^ !vdisk);
            end
            else
                !vdisk in
    
    let vdisk = path_resolve vdiskpath in

    match vdisk with
    | Amoeba_path path ->
    begin
        let off = if !floppy then 18 else 0 in
        let stat,vcap = dir_pathlookup path in

        if (stat <> std_OK) then
        begin
            out ("vdisk lookup for "^path^" failed"); nl ();
            raise (Error stat);
        end;
        let bd_entries = ref [||] in
        let buf = buf_create 512 in     (* kernel dir buffer / 1 block *)

        let stat = disk_read vcap ~start:off ~num:1 ~blksize:512
                                 ~buf:buf ~pos:0 
            in
        if (stat <> std_OK) then
        begin
            out ("disk_read for kerneldir "^path^" failed"); nl ();
            raise (Error stat);
        end;

        if (!init = false) then
        begin
            (*
            ** Is it really a kernel partition?
            *)
            let _,bd = buf_get_bd buf 0 in
            if (bd.bd_magic <> bd_MAGIC) then
            begin
                out ("not a kernel boot directory (use -i flag for cold install)"); 
                nl ();
                raise (Error std_ARGBAD);
            end;
        end;
    
        (*
        ** The next free block in kernel vdisk and total number
        ** of blocks occupied by kernel images.
        *)
        let nextfree = ref 1 in
        let totsize = ref 0 in
        let ikernels = ref [] in

        List.iter (fun k->
                let name,path = k in
                let path = 
                    if path.[0] <> '/' then
                    begin
                        match !work_dir with
                        | Amoeba_path wd ->
                            (wd^"/"^path);
                        | Unix_path wd ->
                            ("/unix/"^wd^"/"^path);
                    end
                    else path in

                match (path_resolve path) with
                | Amoeba_path path ->
                begin
                    let stat,kcap = name_lookup path in
                    if (stat <> std_OK) then
                    begin
                        out ("lookup of kernel image "^path^" failed"); nl();
                        raise (Error stat);
                    end;
                    let stat,size = afs_size kcap in
                    if (stat <> std_OK) then
                    begin
                        out ("afs_Size failed on "^path); nl ();
                        raise (Error stat);
                    end;
                    let blksize = (size+511)/512  in
                    
                    (*
                    ** Install the kernel
                    *)
                    let buf'' = buf_create (blksize*512) in
                    let stat,n = afs_read ~cap:kcap ~offset:0 ~buf:buf''
                                          ~size:size
                        in
                    if (stat <> std_OK) then
                    begin
                        out ("afs_read for "^path^" failed"); nl ();
                        raise (Error stat);
                    end;
                    let stat = disk_write vcap ~start:(off + !nextfree) 
                                           ~num:blksize 
                                           ~blksize:512 ~buf:buf'' ~pos:0
                        in
                    if (stat <> std_OK) then
                    begin
                        out ("disk_write for "^path^" failed"); nl ();
                        raise (Error stat);
                    end;
                    ikernels := !ikernels @ [!nextfree,blksize,name];
                    nextfree := !nextfree + blksize;
                end;
                | Unix_path path ->
                begin
                    let size = 
                        let size = ref 0 in
                        let found = protects (
                                let stats =  Unix.stat path in
                                size := stats.st_size;) in
                        if not found then raise (Error std_NOTFOUND);
                        size in

                    let fd = 
                        let fd = ref nilfd in
                        let found = protects (
                                fd := Unix.openfile path [O_RDONLY]
                                                         (256+128)) in
                        if not found then raise (Error std_NOTFOUND);
                        !fd in

                    let blksize = 512  in

                    (*
                    ** Install the kernel
                    *)
                    let buf'' = buf_create blksize in

                    let stat = ref std_OK in
                    let off = ref (off + !nextfree) in    
                
                    while (!stat = std_OK && !size > 0)
                    do
                        let bn = min !size blksize in
                        let n = Unix.readb ~fd:fd 
                                           ~buf:buf'' 
                                           ~boff:0 
                                           ~len:bn in
                        if n <> bn then raise (Error std_IOERR);

                        let stat = disk_write vcap ~start:!off
                                                   ~num:1
                                                   ~blksize:512 
                                                   ~buf:buf'' 
                                                   ~pos:0
                        in
                        if (stat <> std_OK) then
                        begin
                            out ("disk_write for "^path^" failed"); nl ();
                            raise (Error stat);
                        end;
                        size := !size - bn;
                        off := !off + bn;
                    done;
                    let blksize = (!size+511)/512  in
                    ikernels := !ikernels @ [!nextfree,blksize,name];
                    nextfree := !nextfree + blksize;
                end;
            ) !kernels;
        (*
        ** Build up the new kernel boot directory...
        *)
        let bd_entries = ref [||] in
        List.iter ( fun b ->
                let sec,size,name = b in
                bd_entries := Array.append !bd_entries 
                                [|{
                                    bde_start = word32 sec;
                                    bde_size = word32 size;
                                    bde_name = bde_toname name;
                                }|];
            ) !ikernels;
        let bd' = {
            bd_magic = bd_MAGIC;
            bd_entries = !bd_entries;
            bd_unused = word32 (bd_NENTRIES-
                                (Array.length !bd_entries));
            } in
        let buf' = buf_create 512 in
        let _ = buf_put_bd ~buf:buf' ~pos:0 ~bd:bd' in
        let stat = disk_write vcap ~start:off ~num:1 ~blksize:512
                                   ~buf:buf' ~pos:0 
            in
        if (stat <> std_OK) then
        begin
            out ("disk_write of kernel boot directory failed"); nl ();
            raise (Error stat);
        end;
        std_OK;
    end;
    | Unix_path path ->
    begin
        std_NOTNOW
    end;
  end
  with
    | Error err -> err
    | Failure str -> out ("failed:"^str); nl (); std_SYSERR;
    | _ -> out "failed: UNIX error"; nl (); std_IOERR


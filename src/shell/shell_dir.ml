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
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2005 BSSLAB
**    $CREATED:     1.5.2005
**    $VERSION:     1.07
**
**    $INFO:
**
**  Shell: directory and file management
**
**    $ENDOFINFO
**
*)


open Amoeba
open Dir
open Stderr
open Stdcom
open Unix
open Printf
open Cap_env
open Ar
open Buf
open Bytebuf
open Shell_common
open Capset
open Name
open Afs
open Dns
open Stdobjtypes

(*
** Show the content of a directory or one component.  Returns
** a string with directory informations specified with format.
**
** Example:
**
**  print (dir "/" "%h %2# %20rn %30cr %20lt") ;;
**  
** Path: either Amoeba path
**          => Directory
**             lookups are relative to environment ROOT capability.
**
**      or Unix path
**          => path must be preceeded with the "/unix" prefix
**
** Directory entry informations will be separated by newlines.
** Both Amoeba and Unix filesystems are supported.
**
** Format arguments must be separated by spaces.
**
** Paths can contain filter operations '*', for example:
**
**  '/system/*.ml'
**
*)

let dir ~path ~format =
  let old_timeout = Rpc.timeout !shell_dir_timeout in
  try
  begin
    let fmts = parse_format format in
    if fmts = [] then
        raise (Format_error format);


    let get_stat ss = match ss with
                      | Some s -> s;
                      | None -> raise (Shell_ex (std_SYSERR,
                                                "programming error"));
            in

    (*
    ** Determine type of a Unix object
    *)

    let get_unix_type path =
        let ustat = ref None in
        let exists = protects (ustat := Some (Unix.stat path)) in 
        if exists then
        begin
            let ustat = get_stat !ustat in
            match ustat.st_kind with            
            | S_REG -> if ustat.st_size <> cap_SIZE then
                            File_obj
                       else
                            Cap_obj;       (* sufficient criteria ? *)
            | S_DIR -> Dir_obj;
            | _ -> Nil_obj;            
        end
        else
            Nil_obj
        in

    (*
    ** Cache last dns lookup result - speed up operation!
    *)
    let last_dir_cs = ref nilcapset in

    (*
    ** Determine type of Amoeba object
    *)
    let get_amoeba_type root_cs path =
        let stat,path_cs = dns_lookup root_cs path in
        last_dir_cs := path_cs;
        if stat = std_OK then
        begin
            (* 
            ** Exist! Determine type of object...
            *)
            let _,cap = cs_to_cap path_cs in
            let stat,str = std_info cap 10 in
            if stat = std_OK then
            begin
                match str.[0] with
                | a when a = objsym_DIRECT -> Dir_obj;
                | a when a = objsym_FILE -> File_obj;
                | _ -> Cap_obj;
            end
            else
                Cap_obj
        end
        else
            Nil_obj
        in

    match (path_resolve path) with
    | Amoeba_path path -> 
    begin

        let stat,root = get_env_cap "ROOT" in
        if stat <> std_OK then
            raise (Shell_ex (stat,"get_env_cap of ROOT failed"));
        let root_cs = cs_singleton root in


        let path_type = get_amoeba_type root_cs path in
        let path',single,filter = 
                match path_type with
                | Dir_obj -> path,"","";
                | File_obj
                | Cap_obj -> 
                begin
                    let path' = Filename.dirname path in
                    let single = Filename.basename path in
                    path',single,"";
                end;
                | Nil_obj ->
                begin
                    (*
                    ** Check filter operation
                    *)
                    let path' = Filename.dirname path in
                    let filter = Filename.basename path in
                    let is_filter = String.contains filter '*' in
                    if not is_filter then
                        raise (Shell_ex (std_NOTFOUND,path));
                    path',"",filter;
                end;
            in

        let stat,cp = dir_lookup ~root:root ~name:path' in
        if stat <> std_OK then
            raise (Shell_ex (stat,("dir lookup failed: "^path)));
        let stat,dirdesc = dir_open cp in
        if stat <> std_OK then
            raise (Shell_ex (stat,("dir open failed: "^path)));


        let head = ref "" in
        let str = ref "" in
        let linelen = ref 0 in
        let sep = ref 1 in
        let spaces () = nchars !sep ' ' in

        (*
        ** Try to get pad space parameter...
        *)
        List.iter (fun fmt -> match fmt.fs_type with
                              | Format_pad -> sep:=fmt.fs_len;
                              | _ -> ()) fmts;
        (*
        ** Print header if specified
        *)
        List.iter (fun fmt ->
            match fmt.fs_type with
            | Format_header ->
            begin
              List.iter (fun fmt ->
                match fmt.fs_type with
                | Format_name ->
                    head := !head ^ 
                            (print_box 
                                fmt.fs_len 
                                fmt.fs_just 
                                "Name" true) ^ (spaces ());
                    linelen := !linelen + fmt.fs_len + !sep;
                | Format_time ->
                    head := !head ^ 
                            (print_box 
                                fmt.fs_len 
                                fmt.fs_just 
                                "Creation Time" true) ^ (spaces ());
                    linelen := !linelen + fmt.fs_len + !sep;
                | Format_cap ->
                    head := !head ^ 
                            (print_box 
                                fmt.fs_len 
                                fmt.fs_just 
                                "Row Capability" true) ^ (spaces ());
                    linelen := !linelen + fmt.fs_len + !sep;
                | Format_rights ->
                        let colstr = ref "" in
                        let coln = Array.length dirdesc.dir_colnames in
                        let lenc = fmt.fs_len / coln in
                        Array.iter (fun cn -> 
                                    colstr := !colstr ^ 
                                               (print_box 
                                                    lenc 
                                                    Just_center
                                                    cn
                                                    true);
                                ) dirdesc.dir_colnames;
                    head := !head ^ 
                                (print_box 
                                    fmt.fs_len 
                                    fmt.fs_just 
                                    !colstr false) ^ (spaces ());
                    linelen := !linelen + fmt.fs_len + !sep;
                | Format_info ->
                    head := !head ^ 
                            (print_box 
                                fmt.fs_len 
                                fmt.fs_just 
                                "Object info" true) ^ (spaces ());
                    linelen := !linelen + fmt.fs_len + !sep;
                | _ -> ();                    
              ) fmts;
            end;
            | _ -> ();
            ) fmts;

        if !head <> "" then 
        begin
            head := !head ^ "\n" ^ (nchars !linelen '=') ^ "\n";
        end;


        for i = 0 to (dirdesc.dir_nrows - 1)
        do
          let dr = dir_next dirdesc in
          let name = dr.dr_name in
          let cols = dr.dr_cols in
          let time = dr.dr_time in

          if (single = "" && filter = "") ||
             (dr.dr_name = single) ||
             (is_filter dr.dr_name filter) then
          begin
            List.iter (fun fmt ->
              begin
                match fmt.fs_type with
                | Format_name ->
                    str := !str ^ (print_box 
                                        fmt.fs_len 
                                        fmt.fs_just name true) ^ (spaces ());
                | Format_time ->
                    (*
                    ** Hack:
                    ** New servers count time in 10s units,
                    ** old one in 1s units!
                    *)
                    let tm = if (time > 800000000) then
                                localtime (float_of_int time)
                             else
                                 localtime ((float_of_int time)*.10.0) in
                    let tms = sprintf "%2.2d.%2.2d.%4.4d %2.2d:%2.2d"
                            tm.tm_mday
                            (tm.tm_mon+1)
                            (tm.tm_year+1900)
                            tm.tm_hour
                            tm.tm_min in
                    str := !str ^ (print_box 
                                        fmt.fs_len 
                                        fmt.fs_just 
                                        tms false) ^ (spaces ());
                | Format_rights ->
                    let colstr = ref "" in
                    let coln = Array.length cols in
                    let lenc = fmt.fs_len / coln in
                    Array.iter (fun ci -> 
                                    colstr := !colstr ^ 
                                               (print_box 
                                                    lenc 
                                                    Just_center
                                                    (sprintf "0x%2.2x" ci)
                                                    false);
                                ) cols;
                    str := !str ^ (print_box 
                                        fmt.fs_len 
                                        fmt.fs_just 
                                        !colstr false) ^ (spaces ());
                | Format_cap ->
                    let stat,cap = dir_lookup ~root:cp ~name:name in
                    let capstr = if stat = std_OK then
                                    (ar_cap cap)
                                 else
                                    (sprintf "failed: %s" (err_why stat)) in
                    str := !str ^ (print_box 
                                        fmt.fs_len 
                                        fmt.fs_just 
                                        capstr false) ^ (spaces ());
                | Format_info ->
                    let stat,cap = dir_lookup ~root:cp ~name:name in
                    let infostr = if stat <> std_OK then
                                    (sprintf "failed: %s" (err_why stat))
                                  else
                                  begin
                                    let stat,str = std_info cap fmt.fs_len in
                                    if stat = std_OK then
                                        str
                                    else
                                        (sprintf "failed: %s" (err_why stat));
                                  end 
                        in

                    str := !str ^ (print_box 
                                        fmt.fs_len 
                                        fmt.fs_just 
                                        infostr false) ^ (spaces ());
                | _ -> ();
              end;
              ) fmts;
            str := !str ^ "\n";
          end;
        done;
        __(Rpc.timeout old_timeout);
        (!head ^ !str)
    end;
    | Unix_path path ->
    begin
        let path_type = get_unix_type path in

        let head = ref "" in
        let str = ref "" in
        let linelen = ref 0 in
        let sep = ref 1 in
        let spaces () = nchars !sep ' ' in
        let colnames = [|"Owner";"Group";"Other"|] in

        (*
        ** Try to get pad space parameter...
        *)
        List.iter (fun fmt -> match fmt.fs_type with
                              | Format_pad -> sep:=fmt.fs_len;
                              | _ -> ()) fmts;
        (*
        ** Print header if specified
        *)
        List.iter (fun fmt ->
            match fmt.fs_type with
            | Format_header ->
            begin
              List.iter (fun fmt ->
                match fmt.fs_type with
                | Format_name ->
                    head := !head ^ 
                            (print_box 
                                fmt.fs_len 
                                fmt.fs_just 
                                "Name" true) ^ (spaces ());
                    linelen := !linelen + fmt.fs_len + !sep;
                | Format_time ->
                    head := !head ^ 
                            (print_box 
                                fmt.fs_len 
                                fmt.fs_just 
                                "Creation Time" true) ^ (spaces ());
                    linelen := !linelen + fmt.fs_len + !sep;
                | Format_cap ->
                    head := !head ^ 
                            (print_box 
                                fmt.fs_len 
                                fmt.fs_just 
                                "Row Capability" true) ^ (spaces ());
                    linelen := !linelen + fmt.fs_len + !sep;
                | Format_rights ->
                        let colstr = ref "" in
                        let coln = Array.length colnames in
                        let lenc = fmt.fs_len / coln in
                        Array.iter (fun cn -> 
                                    colstr := !colstr ^ 
                                               (print_box 
                                                    lenc 
                                                    Just_center
                                                    cn
                                                    true);
                                ) colnames;
                    head := !head ^ 
                                (print_box 
                                    fmt.fs_len 
                                    fmt.fs_just 
                                    !colstr false) ^ (spaces ());
                    linelen := !linelen + fmt.fs_len + 1;
                | Format_info ->
                    head := !head ^ 
                            (print_box 
                                fmt.fs_len 
                                fmt.fs_just 
                                "Object info" true) ^ (spaces ());
                    linelen := !linelen + fmt.fs_len + !sep;
                | _ -> ();                    
              ) fmts;
            end;
            | _ -> ();
            ) fmts;

        if !head <> "" then 
        begin
            head := !head ^ "\n" ^ (nchars !linelen '=') ^ "\n";
        end;

        let curname = ref "" in

        (*
        ** Convert UNIX rights mask to Amoeba column rights style.
        *)
        let cols_of_mask mask =
            let mask = ref mask in
            let cols = Array.init 3 (fun _ -> String.create 3 ) in 
                                        
            let chars = [|'r';'w';'x'|] in
            for i = 1 to 3
            do
                let bits = ref (!mask land 0x7) in
                for j = 1 to 3
                do
                    if (!bits land 1) = 1 
                        then cols.(3-i).[3-j] <- chars.(3-j)
                        else cols.(3-i).[3-j] <- '-';
                    bits := !bits lsr 1;
                done;
                mask := !mask lsr 3;
            done;
            cols
            in

        

        let path',single,filter = 
                match path_type with
                | Dir_obj -> path,"","";
                | File_obj
                | Cap_obj -> 
                begin
                    let path' = Filename.dirname path in
                    let single = Filename.basename path in
                    path',single,"";
                end;
                | Nil_obj ->
                begin
                    (*
                    ** Check filter operation
                    *)
                    let path' = Filename.dirname path in
                    let filter = Filename.basename path in
                    let is_filter = String.contains filter '*' in
                    if not is_filter then
                        raise (Shell_ex (std_NOTFOUND,path));
                    path',"",filter;
                end;
            in


        let dirdesc = ref None in
        protect (dirdesc := Some(Unix.opendir path')); 
        let dirdesc = match !dirdesc with
                      | None ->
                            raise (Shell_ex (std_CAPBAD,"opendir failed"));
                      | Some dh -> dh;
                in

        while (protects (curname := readdir dirdesc)) <> false
        do
          if (single = "" && filter = "") ||
             (!curname = single) ||
             (is_filter !curname filter) then
          begin
            let dr = Unix.stat (path' ^ "/" ^ !curname) in
            let name = !curname in
            let cols = cols_of_mask dr.st_perm in
            let time = dr.st_ctime in

            List.iter (fun fmt ->
              begin
                match fmt.fs_type with
                | Format_name ->
                    str := !str ^ (print_box 
                                        fmt.fs_len 
                                        fmt.fs_just name true) ^ (spaces ());
                | Format_time ->
                    (*
                    ** Hack:
                    ** New servers count time in 10s units,
                    ** old one in 1s units!
                    *)
                    let tm = localtime (time) in
                    let tms = sprintf "%2.2d.%2.2d.%4.4d %2.2d:%2.2d"
                            tm.tm_mday
                            (tm.tm_mon+1)
                            (tm.tm_year+1900)
                            tm.tm_hour
                            tm.tm_min in
                    str := !str ^ (print_box 
                                        fmt.fs_len 
                                        fmt.fs_just 
                                        tms false) ^ (spaces ());
                | Format_rights ->
                    let colstr = ref "" in
                    let coln = Array.length cols in
                    let lenc = fmt.fs_len / coln in
                    Array.iter (fun ci -> 
                                    colstr := !colstr ^ 
                                               (print_box 
                                                    lenc 
                                                    Just_center
                                                    ci
                                                    false);
                                ) cols;
                    str := !str ^ (print_box 
                                        fmt.fs_len 
                                        fmt.fs_just 
                                        !colstr false) ^ (spaces ());
                | Format_cap ->
                    (*
                    ** Only files with cap size are treated
                    ** as capability files.
                    *)
                    if dr.st_size = cap_SIZE then
                    begin
                        let stat,cap = read_cap (path' ^ "/" ^ name) in
                        let capstr = if stat = std_OK then
                                    (ar_cap cap)
                                 else
                                    (sprintf "failed: %s" (err_why stat)) in
                        str := !str ^ (print_box 
                                        fmt.fs_len 
                                        fmt.fs_just 
                                        capstr false) ^ (spaces ());
                    end
                    else
                        str := !str ^ (print_box 
                                        fmt.fs_len 
                                        fmt.fs_just 
                                        "not a capability" true) ^ (spaces ());

                | Format_info ->
                    (*
                    ** Only files with cap size are treated
                    ** as capability files.
                    *)
                    if dr.st_size = cap_SIZE then
                    begin
                        let stat,cap = read_cap (path' ^ "/" ^ name) in
                        let infostr = 
                                if stat = std_OK then
                                begin
                                    let stat,str = std_info cap fmt.fs_len in
                                    if stat = std_OK then
                                        str
                                    else
                                        (sprintf "failed: %s" (err_why stat))
                                end
                                else
                                    (sprintf "failed: %s" (err_why stat)) in
                        str := !str ^ (print_box 
                                        fmt.fs_len 
                                        fmt.fs_just 
                                        infostr false) ^ (spaces ());
                    end
                    else
                        str := !str ^ (print_box 
                                        fmt.fs_len 
                                        fmt.fs_just 
                                        "not a capability" true) ^ (spaces ());

                | _ -> ();
              end;
              ) fmts;
            str := !str ^ "\n";
          end;
        done;
        __(Rpc.timeout old_timeout);
        (!head ^ !str)
    end
  end
  with
    | Shell_ex (err,msg) -> __(Rpc.timeout old_timeout);
                            sprintf "%s: %s\n" msg (err_why err);
    | Format_error str -> __(Rpc.timeout old_timeout);
                          sprintf "Invalid format: %s\n" str;
    | _ -> __(Rpc.timeout old_timeout);
           "Internal Error\n"
 

(*
** Copy generic objects (not a file or directory), 
** files and directories. Both the UNIX and Amoeba
** filesystems are supported. The operations to be performed are
** specified in the args string:
**
**  o      : delete and copy  -> already existing dst directory entries
**                                are replaced 
**  f      : destroy and copy -> destroy files to be deleted during
**                                overwrite (std_DESTROY request).
**
**  r      : copy recursive, both files, capabilities
**
**  s      : create and return a log string (will then be printed
**           with user supplied function shell_log_fun, too)
**
** Arguments must be divided by spaces.
**
** Example:
**      copy "/unix/etc" "/tmp/etc" "r"
**
** Note on directory copy:
**
**      The last path component of the source diretory is NOT created
**      as a new directory in the destination path! That means in
**      the above example, the target directory '/tmp/etc' must already
**      exist.
**
**
*)

let copy ~src ~dst ~args =
  let old_timeout = Rpc.timeout (2 * !shell_dir_timeout) in
  let msg = ref "" in
  let do_log = ref false in
  let log str = 
      if !do_log then
      begin
        msg := !msg ^ str;
        !shell_log_fun str;
      end
      in

  let fatal str = 
      msg := !msg ^ str;
      !shell_log_fun str;
      in

  try
  begin
    let src = path_resolve src in
    let dst = path_resolve dst in

    (* -rw-r--r-- *)
    let unix_perm = ref 420 in
    (* drwxr-xr-x *)
    let unix_dir_perm = ref 493 in

    let overwrite,file_destroy,recursive,status =
        let o,f,r,s=ref false,ref false, ref false, ref false in
        let argl = Str.split (Str.regexp " ") args in
        List.iter (fun arg ->
            match arg with
            | "o" -> o:= true;
            | "f" -> f:= true;
            | "r" -> r:= true;
            | "s" -> s:= true;
            | _ -> raise (Shell_ex (std_ARGBAD,arg)); 
            ) argl;
        !o,!f,!r,!s
        in
    if status then do_log := true;

    (*
    ** Copy one file:
    **
    **  1. from Unix to Unix
    **  2. from Unix to Amoeba
    **  3. from Amoeba to Unix
    **  4. from Amoeba to Amoeba
    *)
    let get_stat ss = match ss with
                      | Some s -> s;
                      | None -> raise (Shell_ex (std_SYSERR,
                                                "programming error"));
            in

    
    (*
    ** Copy a file from Unix to Unix.
    ** Src and Dst are full paths!
    *)
    let copy_file_u2u src dst =

        let bufsize = 10*512 in
        let buf = buf_create bufsize in
        (*
        ** Get informations about the source file (file size...)
        *)
        let stat = ref None in
        let exists = protects (stat := Some (Unix.stat src)) in 
        if not exists then raise (Shell_ex (std_NOTFOUND,src));
        let src_stat = get_stat !stat in
        let src_size = src_stat.st_size in
        
        (*
        ** Maybe an already existing destination ?
        *)
        let exists = protects (stat := Some (Unix.stat dst)) in 
        if exists && not overwrite 
                then raise (Shell_ex (std_EXISTS,dst))
        else if exists then
        begin
            (*
            ** Delete the file.
            *)
            let failed = not (protects (Unix.unlink dst)) in
            if failed 
                then raise (Shell_ex (std_IOERR,("delete failed: "^dst)));
        end;

        let srcfd = ref nilfd in
        let dstfd = ref nilfd in

        let failed = not (protects 
                            (srcfd := Unix.openfile
                                           ~name:src
                                           ~flags:[O_RDONLY]
                                           ~perm:0;
                            )) in        
        if failed then raise (Shell_ex (std_IOERR,src));
        let failed = not (protects 
                            (dstfd := Unix.openfile
                                           ~name:dst
                                           ~flags:[O_WRONLY;
                                                   O_CREAT]
                                           ~perm:src_stat.st_perm;
                            )) in        
        if failed then raise (Shell_ex (std_IOERR,dst));
       
        (*
        ** Now copy the data
        *)
        let left = ref src_size in
        while (!left > 0)
        do
            let size = min !left bufsize in

            let len = Unix.readb ~fd: !srcfd
                                 ~buf: buf
                                 ~boff: 0
                                 ~len: size in
            
            let len' = Unix.writeb ~fd: !dstfd
                                   ~buf: buf
                                   ~boff: 0
                                   ~len: len in
            if len <> len' then
                raise (Shell_ex (std_IOERR,"src read does not match dst write"));
            left := !left - len;
        done;
        log (sprintf "copied %s -> %s\n" src dst);
        in

    (*
    ** Copy a file from Unix to Amoeba.
    ** Src and Dst are full paths, Dst_file is only the dst filename.
    *)
    let copy_file_u2a root_cs dst_dir_cs src dst dst_file =

        let bufsize = 10*512 in
        let buf = buf_create bufsize in
        (*
        ** Get informations about the source file (file size...)
        *)
        let stat = ref None in
        let exists = protects (stat := Some (Unix.stat src)) in 
        if not exists then raise (Shell_ex (std_NOTFOUND,src));
        let src_stat = get_stat !stat in
        let src_size = src_stat.st_size in
        
        (*
        ** Maybe an already existing destination ?
        *)

        let stat,file_cs = dns_lookup ~root: dst_dir_cs 
                                      ~path: dst_file in
        let exists = stat = std_OK in
        if exists && not overwrite 
                then raise (Shell_ex (std_EXISTS,dst))
        else if exists then
        begin
            (*
            ** Destroy the file if file_destroyd mode is selected and 
            ** remove object from directory.
            *)
            if file_destroy then
            begin
                let _,cap = cs_to_cap file_cs in
                let stat = std_destroy cap in
                if stat <> std_OK then
                    raise (Shell_ex (stat,("destroy failed: "^dst)));
            end;
            let stat = dns_delete dst_dir_cs dst_file in
            let failed = stat <> std_OK in
            if failed 
                then raise (Shell_ex (stat,("delete failed: "^dst)));
        end;

        let srcfd = ref nilfd in

        let failed = not (protects 
                            (srcfd := Unix.openfile
                                           ~name:src
                                           ~flags:[O_RDONLY]
                                           ~perm:0;
                            )) in        
        if failed then raise (Shell_ex (std_IOERR,src));

        (*
        ** Try to get the AFS server capability 
        **  1. from the DNS server
        **  2. from environment variable AFS
        *)

        let afs_cap = ref nilcap in
        
        let stat,cap = dns_getdefafs dst_dir_cs in
        if stat = std_OK then
        begin
            afs_cap := cap;
        end
        else
        begin
            let stat,cap = get_env_cap "AFS" in
            if stat = std_OK then
                afs_cap := cap
            else
                raise (Shell_ex (stat,"can't get AFS environment"));
        end;


        let stat,filecap = afs_create ~cap: !afs_cap 
                                      ~buf:nilbuf
                                      ~size:0
                                      ~commit:0 in

        if stat <> std_OK then 
            raise (Shell_ex (stat,"afs create failed: "^dst));

        let filecap = ref filecap in
       
        (*
        ** Now copy the data
        *)
        let left = ref src_size in
        let off = ref 0 in

        while (!left > 0)
        do
            let size = min !left bufsize in

            let len = Unix.readb ~fd: !srcfd
                                 ~buf: buf
                                 ~boff: 0
                                 ~len: size in
            
            let stat,cap = afs_modify ~cap: !filecap
                                      ~buf: buf
                                      ~size: len
                                      ~offset: !off
                                      ~commit: 0 in
            filecap := cap;
            if stat <> std_OK then
                raise (Shell_ex (stat,"afs modify failed: "^dst));
            left := !left - len;
            off := !off + len;
        done;
        protect(Unix.close !srcfd);
        (*
        ** Commit the AFS file
        *)
        let stat,cap = afs_modify !filecap nilbuf 0 0 afs_COMMIT in
        if stat <> std_OK then
            raise (Shell_ex (stat,"afs commit failed: "^dst));

        (*
        ** Install filecap in directory
        *)
        let _,cols = default_colmask () in
        let filecs = cs_singleton !filecap in
        let stat = dns_append ~dir:  dst_dir_cs 
                              ~name: dst_file 
                              ~obj:  filecs
                              ~cols: cols in
        if stat <> std_OK then
            raise (Shell_ex (stat,"dns append failed: "^dst));
    
        log (sprintf "copied %s -> %s\n" src dst);
        in




    (*
    ** Copy a file from Amoeba to Unix
    *)
    let copy_file_a2u root_cs src src_dir_cs src_file dst =

        let bufsize = 10*512 in
        let buf = buf_create bufsize in
        (*
        ** Get informations about the source file (file size...)
        *)
        let stat,file_cs = dns_lookup src_dir_cs src_file in
        if stat <> std_OK then raise (Shell_ex 
                                        (stat,"dns lookup failed: "^src));
        let _,src_file_cap = cs_to_cap file_cs in
        let stat,src_size = afs_size src_file_cap in
        if stat <> std_OK then raise (Shell_ex 
                                        (stat,"afs size: "^src));

        (*
        ** Maybe an already existing destination ?
        *)
        let exists = protects (__(Unix.stat dst)) in 
        if exists && not overwrite 
                then raise (Shell_ex (std_EXISTS,dst))
        else if exists then
        begin
            (*
            ** Delete the file.
            *)
            let failed = not (protects (Unix.unlink dst)) in
            if failed 
                then raise (Shell_ex (std_IOERR,("delete failed: "^dst)));
        end;

        let dstfd = ref nilfd in

        let failed = not (protects 
                            (dstfd := Unix.openfile
                                           ~name:dst
                                           ~flags:[O_WRONLY;
                                                   O_CREAT]
                                           ~perm: !unix_perm;   
                            )) in        
        if failed then raise (Shell_ex (std_IOERR,
                                       ("open file failed: "^dst)));
       
        (*
        ** Now copy the data
        *)
        let left = ref src_size in
        let off = ref 0 in

        while (!left > 0)
        do
            let size = min !left bufsize in

            let stat,got = afs_read ~cap: src_file_cap 
                                    ~offset: !off
                                    ~buf: buf
                                    ~size: size in
            
            if stat <> std_OK then
                raise (Shell_ex (stat,("afs read failed: "^src)));

            let len' = Unix.writeb ~fd: !dstfd
                                   ~buf: buf
                                   ~boff: 0
                                   ~len: got in

            left := !left - len';
            off := !off + len';
        done;
        protect(Unix.close !dstfd);

        log (sprintf "copied %s -> %s\n" src dst);
        in

    (*
    ** Copy a file from Amoeba to Amoeba
    *)
    let copy_file_a2a root_cs src_dir_cs src_file 
                              dst_dir_cs dst_file 
                              src dst =

        let bufsize = 10*512 in
        let buf = buf_create bufsize in
        (*
        ** Get informations about the source file (file size...)
        *)
        let stat,file_cs = dns_lookup src_dir_cs src_file in
        if stat <> std_OK then raise (Shell_ex 
                                        (stat,"dns lookup failed: "^src));
        let _,src_file_cap = cs_to_cap file_cs in
        let stat,src_size = afs_size src_file_cap in
        if stat <> std_OK then raise (Shell_ex 
                                        (stat,"afs size: "^src));


        (*
        ** Maybe an already existing destination ?
        *)

        let stat,file_cs = dns_lookup ~root: dst_dir_cs
                                      ~path: dst_file in
        let exists = stat = std_OK in
        if exists && not overwrite 
                then raise (Shell_ex (std_EXISTS,dst))
        else if exists then
        begin
            (*
            ** Destroy the file if file_destroyd mode is selected and 
            ** remove object from directory.
            *)
            if file_destroy then
            begin
                let _,cap = cs_to_cap file_cs in
                let stat = std_destroy cap in
                if stat <> std_OK then
                    raise (Shell_ex (stat,("destroy failed: "^dst)));
            end;
            let stat = dns_delete dst_dir_cs dst_file in
            let failed = stat <> std_OK in
            if failed 
                then raise (Shell_ex (stat,("delete failed: "^dst)));
        end;


        (*
        ** Try to get the AFS server capability 
        **  1. from the DNS server
        **  2. from environment variable AFS
        *)

        let afs_cap = ref nilcap in
        
        let stat,cap = dns_getdefafs dst_dir_cs in
        if stat = std_OK then
        begin
            afs_cap := cap;
        end
        else
        begin
            let stat,cap = get_env_cap "AFS" in
            if stat = std_OK then
                afs_cap := cap
            else
                raise (Shell_ex (stat,"can't get AFS environment"));
        end;


        let stat,filecap = afs_create ~cap: !afs_cap 
                                      ~buf:nilbuf
                                      ~size:0
                                      ~commit:0 in

        if stat <> std_OK then 
            raise (Shell_ex (stat,"afs create failed: "^dst));

        let filecap = ref filecap in


        (*
        ** Now copy the data
        *)
        let left = ref src_size in
        let off = ref 0 in

        while (!left > 0)
        do
            let size = min !left bufsize in

            let stat,len = afs_read ~cap: src_file_cap 
                                    ~offset: !off
                                    ~buf: buf
                                    ~size: size in
            
            if stat <> std_OK then
                raise (Shell_ex (stat,("afs read failed: "^src)));

            let stat,cap = afs_modify ~cap: !filecap
                                      ~buf: buf
                                      ~size: len
                                      ~offset: !off
                                      ~commit: 0 in
            filecap := cap;
            if stat <> std_OK then
                raise (Shell_ex (stat,"afs modify failed: "^dst));

            left := !left - len;
            off := !off + len;
        done;
        (*
        ** Commit the AFS file
        *)
        let stat,cap = afs_modify !filecap nilbuf 0 0 afs_COMMIT in
        if stat <> std_OK then
            raise (Shell_ex (stat,"afs commit failed: "^dst));

        (*
        ** Install filecap in directory
        *)
        let _,cols = default_colmask () in
        let filecs = cs_singleton !filecap in
        let stat = dns_append ~dir:  dst_dir_cs
                              ~name: dst_file 
                              ~obj:  filecs
                              ~cols: cols in
        if stat <> std_OK then
            raise (Shell_ex (stat,"dns append failed: "^dst));
        log (sprintf "copied %s -> %s\n" src dst);
        in
 

    (*
    ** Copy capability
    *)
    let copy_cap_a2a  root_cs 
                      obj_cs
                      dst_dir_cs
                      obj_name
                      src
                      dst =

        (*
        ** Maybe an already existing destination ?
        *)

        let stat,file_cs = dns_lookup ~root: dst_dir_cs
                                      ~path: obj_name in
        let exists = stat = std_OK in
        if exists && not overwrite 
                then raise (Shell_ex (std_EXISTS,dst))
        else if exists then
        begin
            (*
            ** Delete only directory entry.
            *)
            let stat = dns_delete dst_dir_cs obj_name in
            let failed = stat <> std_OK in
            if failed 
                then raise (Shell_ex (stat,("delete failed: "^dst)));
        end;

        (*
        ** Install cap in directory
        *)
        let _,cols = default_colmask () in
        let stat = dns_append ~dir:  dst_dir_cs
                              ~name: obj_name 
                              ~obj:  obj_cs
                              ~cols: cols in
        if stat <> std_OK then
            raise (Shell_ex (stat,"dns append failed: "^dst));

        log (sprintf "copied %s -> %s\n" src dst);
        in


    let copy_cap_a2u  root_cs 
                      obj_cs
                      src
                      dst =
        (*
        ** Maybe an already existing destination ?
        *)
        let exists = protects (__(Unix.stat dst)) in 
        if exists && not overwrite 
                then raise (Shell_ex (std_EXISTS,dst))
        else if exists then
        begin
            (*
            ** Delete the file.
            *)
            let failed = not (protects (Unix.unlink dst)) in
            if failed 
                then raise (Shell_ex (std_IOERR,("delete failed: "^dst)));
        end;

       
        let _,obj_cap = cs_to_cap obj_cs in
        let stat = write_cap dst obj_cap in
        
        if stat <> std_OK then
            raise (Shell_ex (stat,"write cap failed: "^dst));

        log (sprintf "copied %s -> %s\n" src dst);
        in


    (*
    ** Copy capability
    *)
    let copy_cap_u2a  root_cs 
                      dst_dir_cs
                      obj_name
                      src
                      dst =

        (*
        ** Maybe an already existing destination ?
        *)

        let stat,file_cs = dns_lookup ~root: dst_dir_cs
                                      ~path: obj_name in
        let exists = stat = std_OK in
        if exists && not overwrite 
                then raise (Shell_ex (std_EXISTS,dst))
        else if exists then
        begin
            (*
            ** Delete only directory entry.
            *)
            let stat = dns_delete dst_dir_cs obj_name in
            let failed = stat <> std_OK in
            if failed 
                then raise (Shell_ex (stat,("delete failed: "^dst)));
        end;
        (*
        ** Get the capability
        *)
        let stat,cap = read_cap src in
        if stat <> std_OK then
            raise (Shell_ex (stat,("read cap failed: "^src)));

        let obj_cs = cs_singleton cap in

        (*
        ** Install cap in directory
        *)
        let _,cols = default_colmask () in
        let stat = dns_append ~dir:  dst_dir_cs
                              ~name: obj_name 
                              ~obj:  obj_cs
                              ~cols: cols in
        if stat <> std_OK then
            raise (Shell_ex (stat,"dns append failed: "^dst));

        log (sprintf "copied %s -> %s\n" src dst);
        in

    let copy_cap_u2u  
                      src
                      dst =
        (*
        ** Maybe an already existing destination ?
        *)
        let exists = protects (__(Unix.stat dst)) in 
        if exists && not overwrite 
                then raise (Shell_ex (std_EXISTS,dst))
        else if exists then
        begin
            (*
            ** Delete the file.
            *)
            let failed = not (protects (Unix.unlink dst)) in
            if failed 
                then raise (Shell_ex (std_IOERR,("delete failed: "^dst)));
        end;

        let stat,obj_cap = read_cap src in       
        if stat <> std_OK then
            raise (Shell_ex (stat,"read cap failed: "^src));

        let stat = write_cap dst obj_cap in
        
        if stat <> std_OK then
            raise (Shell_ex (stat,"write cap failed: "^dst));

        log (sprintf "copied %s -> %s\n" src dst);
        in


    (*
    ** Determine type of a Unix object
    *)

    let get_unix_type path =
        let ustat = ref None in
        let exists = protects (ustat := Some (Unix.stat path)) in 
        if exists then
        begin
            let ustat = get_stat !ustat in
            match ustat.st_kind with            
            | S_REG -> if ustat.st_size <> cap_SIZE then
                            File_obj
                       else
                            Cap_obj;       (* sufficient criteria ? *)
            | S_DIR -> Dir_obj;
            | _ -> Nil_obj;            
        end
        else
            Nil_obj
        in

    (*
    ** Cache last dns lookup result - speed up operation!
    *)
    let last_dir_cs = ref nilcapset in

    (*
    ** Determine type of Amoeba object
    *)
    let get_amoeba_type root_cs path =
        let stat,path_cs = dns_lookup root_cs path in
        last_dir_cs := path_cs;
        if stat = std_OK then
        begin
            (* 
            ** Exist! Determine type of object...
            *)
            let _,cap = cs_to_cap path_cs in
            let stat,str = std_info cap 10 in
            if stat = std_OK then
            begin
                match str.[0] with
                | a when a = objsym_DIRECT -> Dir_obj;
                | a when a = objsym_FILE -> File_obj;
                | _ -> Cap_obj;
            end
            else
                Cap_obj
        end
        else
            Nil_obj
        in

    (*
    ** Copy from directory to directory
    *)

    let copy_dir_u2u src_dir dst_dir filter =
        let rec copy_dir src dst =
            (*
            ** Get all entries from src and copy them to dst.
            ** Distinguish files, caps and directories.
            *)
            let dirdesc = ref None in
            protect (dirdesc := Some(Unix.opendir src)); 
            let dirdesc = match !dirdesc with
                          | None ->
                            raise (Shell_ex (std_IOERR,
                                                ("opendir failed: "^src)));
                          | Some dh -> dh;
                in
            let curname = ref "" in
            while (protects (curname := Unix.readdir dirdesc)) <> false
            do
                let src_path =  src ^ "/" ^ !curname in
                let src_type = get_unix_type src_path in
                if (!curname <> "." &&
                    !curname <> ".." &&
                    (filter = "" || 
                     (is_filter !curname filter))) then
                match src_type with
                | File_obj ->
                begin
                    (*
                    ** Copy file to dst directory.
                    *)
                    let dst_path = dst ^ "/" ^ !curname in
                    copy_file_u2u src_path dst_path;
                end;
                | Cap_obj ->
                begin
                    let dst_path = dst ^ "/" ^ !curname in
                    copy_cap_u2u src_path dst_path;
                end;
                | Dir_obj -> 
                begin
                    let dst_path  = dst ^ "/" ^ !curname in
                    let src_path' = src ^ "/" ^ !curname in
                    let stat = ref None in
                    let failed = not (protects 
                                        (stat := Some (Unix.stat src_path'))) in 
                    if failed then
                        raise (Shell_ex (std_IOERR,
                                            ("invalid source: "^src_path)));

                    let exists = protects (__(Unix.stat dst_path)) in
                    if not exists then
                    begin
                        log (sprintf "created directory %s\n"
                                                   dst_path);
                        let perm = (get_stat !stat).st_perm in
                        let failed = not (protects (Unix.mkdir dst_path perm))
                        in
                        if failed then
                            raise (Shell_ex (std_IOERR,
                                            ("can't create dir: "^dst_path)));
                    end;
    
                    copy_dir src_path' dst_path;
                end;
                | Nil_obj -> raise (Shell_ex (std_IOERR,
                                            ("invalid source: "^src_path)));
            done;
            in

        copy_dir src_dir dst_dir;
        in

    let copy_dir_u2a root_cs src_dir dst_dir_cs dst_dir filter =
        let rec copy_dir src dst_cs dst =

            (*
            ** Get all entries from src and copy them to dst.
            ** Distinguish files, caps and directories.
            *)
            let dirdesc = ref None in
            protect (dirdesc := Some(Unix.opendir src)); 
            let dirdesc = match !dirdesc with
                          | None ->
                            raise (Shell_ex (std_IOERR,
                                                ("opendir failed: "^src)));
                          | Some dh -> dh;
                in

            let curname = ref "" in
            while (protects (curname := Unix.readdir dirdesc)) <> false
            do
                let src_path =  src ^ "/" ^ !curname in
                let src_type = get_unix_type src_path in
                if (!curname <> "." &&
                    !curname <> ".." &&
                    (filter = "" || 
                     (is_filter !curname filter))) then
                match src_type with
                | File_obj ->
                begin
                    (*
                    ** Copy file to dst directory.
                    *)
                    let dst_path = dst ^ "/" ^ !curname in
                    copy_file_u2a root_cs dst_cs src_path dst_path !curname;
                end;
                | Cap_obj ->
                begin
                    let dst_path = dst ^ "/" ^ !curname in
                    copy_cap_u2a root_cs dst_cs !curname src_path dst_path;
                end;
                | Dir_obj -> 
                begin
                    let dst_path  = dst ^ "/" ^ !curname in
                    let src_path' = src ^ "/" ^ !curname in
                    let stat = ref None in
                    let failed = not (protects 
                                        (stat := Some (Unix.stat src_path'))) in 
                    if failed then
                        raise (Shell_ex (std_IOERR,
                                            ("invalid source: "^src_path)));

                    let stat,dst_cs' = dns_lookup dst_cs !curname in 
                    if stat <> std_OK then
                    begin
                        log (sprintf "created directory %s\n"
                                                   dst_path);
                        (*
                        ** Create and append a new directory.
                        *)
                        let col_names = dns_DEFAULT_COLS in
                        let stat,dst_cs' = dns_create dst_cs col_names in

                        if stat <> std_OK then
                            raise (Shell_ex (stat,
                                            ("can't create dir: "^dst_path)));

                        let _,col_rights = default_colmask () in

                        let stat = dns_append dst_cs !curname
                                              dst_cs' col_rights in

                        if stat <> std_OK then
                            raise (Shell_ex (stat,
                                            ("can't append dir: "^dst_path)));
                        copy_dir src_path' dst_cs' dst_path;
                    end
                    else
                        copy_dir src_path' dst_cs' dst_path;
                end;
                | Nil_obj -> raise (Shell_ex (std_IOERR,
                                            ("invalid source: "^src_path)));
            done;
            in

        copy_dir src_dir dst_dir_cs dst_dir;
        in

    let copy_dir_a2u root_cs src_dir src_dir_cs dst_dir filter =

        let rec copy_dir src src_cs dst =

            (*
            ** Get all entries from src and copy them to dst.
            ** Distinguish files, caps and directories.
            *)

            let _,src_cap = cs_to_cap src_cs in
            let stat,dirdesc = dir_open src_cap in
            if stat <> std_OK then
                raise (Shell_ex (stat,("dir open failed: "^src)));

            
            let curname = ref "" in

            for i = 0 to (dirdesc.dir_nrows - 1)
            do
                let dr = dir_next dirdesc in
                curname := dr.dr_name;

                let src_path =  src ^ "/" ^ !curname in
                let src_type = get_amoeba_type root_cs src_path in
                let last_src_cs = !last_dir_cs in

                
                if (filter = "" || 
                    (is_filter !curname filter)) then
                match src_type with
                | File_obj ->
                begin
                    (*
                    ** Copy file to dst directory.
                    *)
                    let dst_path = dst ^ "/" ^ !curname in
                    copy_file_a2u root_cs src_path 
                                          src_cs 
                                          !curname dst_path; 
                end;
                | Cap_obj ->
                begin
                    let dst_path = dst ^ "/" ^ !curname in
                    copy_cap_a2u root_cs 
                                 last_src_cs 
                                 src_path 
                                 dst_path;
                end;
                | Dir_obj -> 
                begin
                    let dst_path  = dst ^ "/" ^ !curname in
                    let src_path' = src ^ "/" ^ !curname in

                    let exists = (protects (__(Unix.stat dst_path))) in

                    if not exists then
                    begin
                       log (sprintf "created directory %s\n"
                                                   dst_path);

                        let failed = not (protects (Unix.mkdir 
                                                    dst_path
                                                    !unix_dir_perm)) in
                        if failed then
                            raise (Shell_ex (std_IOERR,
                                            ("can't create dir:"^dst_path)));

                    end;
                    copy_dir src_path' last_src_cs dst_path;
                end;
                | Nil_obj -> raise (Shell_ex (std_IOERR,
                                            ("invalid source: "^src_path)));
            done;
            in

        copy_dir src_dir src_dir_cs dst_dir;
        in

    let copy_dir_a2a root_cs src_dir src_dir_cs dst_dir dst_dir_cs filter =

        let rec copy_dir src src_cs dst dst_cs =

            (*
            ** Get all entries from src and copy them to dst.
            ** Distinguish files, caps and directories.
            *)

            let _,src_cap = cs_to_cap src_cs in
            let stat,dirdesc = dir_open src_cap in
            if stat <> std_OK then
                raise (Shell_ex (stat,("dir open failed: "^src)));

            
            let curname = ref "" in

            for i = 0 to (dirdesc.dir_nrows - 1)
            do
                let dr = dir_next dirdesc in
                curname := dr.dr_name;

                let src_path =  src ^ "/" ^ !curname in
                let src_type = get_amoeba_type root_cs src_path in
                let last_src_cs = !last_dir_cs in

                
                if (filter = "" || 
                    (is_filter !curname filter)) then
                match src_type with
                | File_obj ->
                begin
                    (*
                    ** Copy file to dst directory.
                    *)
                    let dst_path = dst ^ "/" ^ !curname in
                    copy_file_a2a root_cs src_cs
                                          !curname
                                          dst_cs 
                                          !curname 
                                          src_path
                                          dst_path; 
                end;
                | Cap_obj ->
                begin
                    let dst_path = dst ^ "/" ^ !curname in
                    copy_cap_a2a root_cs 
                                 last_src_cs 
                                 dst_cs
                                 !curname
                                 src_path 
                                 dst_path;
                end;
                | Dir_obj -> 
                begin
                    let dst_path  = dst ^ "/" ^ !curname in
                    let src_path' = src ^ "/" ^ !curname in

                    let stat,dst_cs' = dns_lookup dst_cs !curname in 
                    if stat <> std_OK then
                    begin
                        log (sprintf "created directory %s\n"
                                                   dst_path);
                        (*
                        ** Create and append a new directory.
                        *)
                        let col_names = dns_DEFAULT_COLS in
                        let stat,dst_cs' = dns_create dst_cs col_names in

                        if stat <> std_OK then
                            raise (Shell_ex (stat,
                                            ("can't create dir: "^dst_path)));

                        let _,col_rights = default_colmask () in

                        let stat = dns_append dst_cs !curname
                                              dst_cs' col_rights in

                        if stat <> std_OK then
                            raise (Shell_ex (stat,
                                            ("can't append dir: "^dst_path)));

                        copy_dir src_path' last_src_cs dst_path dst_cs';
                    end
                    else
                        copy_dir src_path' last_src_cs dst_path dst_cs';
                end;
                | Nil_obj -> raise (Shell_ex (std_IOERR,
                                            ("invalid source: "^src_path)));
            done;
            in

        copy_dir src_dir src_dir_cs dst_dir dst_dir_cs;
        in


    (
      match src with
      | Unix_path src ->
      begin
        match dst with
        | Unix_path dst ->
        begin
            (*
            ** Get informations about src and dst
            *)

            (*
            ** Either dst is a directory (must exist) or a destination
            ** file (may or may not exist).
            *)
            let file' = Filename.basename dst in

            let src_type = get_unix_type src in

            let dst_type = get_unix_type dst in

            match src_type with
            | File_obj ->
            begin
                let file = Filename.basename src in
                match dst_type with
                | File_obj ->
                    copy_file_u2u src dst
                | Dir_obj ->
                    copy_file_u2u src (dst^"/"^file);
                | Nil_obj -> 
                begin
                    (*
                    ** Check parent directory.
                    *)
                    let dir' = Filename.dirname dst in
                    let dst_type' = get_unix_type dir' in
                    match dst_type' with
                    | Dir_obj -> copy_file_u2u src dst;
                    | _ -> raise (Shell_ex (std_IOERR,
                                    ("Invalid destination: "^dst)));
                end;
                | _ ->  
                    raise (Shell_ex (std_IOERR,
                                    ("Invalid source: "^src)));
            end
            | Dir_obj ->
            begin
                copy_dir_u2u src dst "";
            end;
            | Cap_obj ->
            begin
                let file = Filename.basename src in
                match dst_type with
                | File_obj ->
                    copy_cap_u2u src dst
                | Dir_obj ->
                    copy_cap_u2u src (dst^"/"^file);
                | Nil_obj -> 
                begin
                    (*
                    ** Check parent directory.
                    *)
                    let dir' = Filename.dirname dst in
                    let dst_type' = get_unix_type dir' in
                    match dst_type' with
                    | Dir_obj -> copy_cap_u2u src dst;
                    | _ -> raise (Shell_ex (std_IOERR,
                                    ("Invalid destination: "^dst)));
                end;
                | _ ->  
                    raise (Shell_ex (std_IOERR,
                                    ("Invalid source: "^src)));

            end;
            
            | Nil_obj ->
            begin
                (*
                ** Check for filter operation
                *)
                let src_dir',filter = 
                    let path' = Filename.dirname src in
                    let filter = Filename.basename src in
                    let is_filter = String.contains filter '*' in
                    if not is_filter then
                        raise (Shell_ex (std_NOTFOUND,src));
                    path',filter;
                    in
                match dst_type with
                | File_obj ->
                    raise (Shell_ex (std_IOERR,
                            ("Invalid source: "^src)));
                | Dir_obj ->
                    copy_dir_u2u src_dir' dst filter;
                | _ ->
                    raise (Shell_ex (std_IOERR,
                            ("Invalid source: "^src)));
            end;
        end;
        | Amoeba_path dst -> 
            (*
            ** Get informations of src and dst
            *)

            (*
            ** Either dst is a directory (must exist) or a destination
            ** file (may or may not exist).
            *)
            let file' = Filename.basename dst in

            let src_type = get_unix_type src in

            let stat,root = get_env_cap "ROOT" in
            if stat <> std_OK then
                raise (Shell_ex (stat,"get_env_cap of ROOT failed"));
            let root_cs = cs_singleton root in

            let dst_type = get_amoeba_type root_cs dst in
            let last_dst_cs = !last_dir_cs in

            match src_type with
            | File_obj ->
            begin
                let file = Filename.basename src in
                (*
                ** A regular src file.
                *)

                match dst_type with
                | File_obj ->
                    let dir' = Filename.dirname dst in
                    let _,dst_dir_cs = dns_lookup root_cs dir' in
                    copy_file_u2a root_cs 
                                  dst_dir_cs 
                                  src 
                                  dst
                                  (Filename.basename dst);
                | Dir_obj ->
                    copy_file_u2a root_cs
                                  last_dst_cs
                                  src 
                                  (dst^"/"^file)
                                  file;
                | _ ->
                begin
                    (*
                    ** Check parent directory.
                    *)
                    let dir' = Filename.dirname dst in
                    let file' = Filename.basename dst in
                    let dst_type' = get_amoeba_type root_cs dir' in
                    let last_dst_cs = !last_dir_cs in

                    match dst_type' with
                    | Dir_obj -> 
                        copy_file_u2a root_cs
                                      last_dst_cs
                                      src 
                                      dst
                                      file';
                    | _ -> raise (Shell_ex (std_IOERR,
                                    ("Invalid destination: "^dst)));
                    
                end;
            end;
            | Dir_obj ->
            begin
                copy_dir_u2a root_cs src last_dst_cs dst "";
            end;

            | Cap_obj ->
            begin
                let obj_name = Filename.basename src in
                match dst_type with
                | File_obj -> raise (Shell_ex (std_IOERR,
                                    ("Invalid destination: "^dst)));
                | Dir_obj ->
                    copy_cap_u2a root_cs
                                 last_dst_cs
                                 obj_name
                                 src
                                 dst;
                | _ ->
                    let dst_dir' = Filename.dirname dst in
                    let dst_name = Filename.basename dst in
                    let dst_type' = get_amoeba_type root_cs
                                                    dst_dir' in
                    if dst_type' <> Dir_obj then
                            raise (Shell_ex (std_IOERR,
                                    ("Invalid destination: "^dst)));
                    copy_cap_u2a root_cs
                                 !last_dir_cs
                                 dst_name
                                 src
                                 dst;
            end;                             
            | Nil_obj ->
            begin
                (*
                ** Check for filter operation
                *)
                let src_dir',filter = 
                    let path' = Filename.dirname src in
                    let filter = Filename.basename src in
                    let is_filter = String.contains filter '*' in
                    if not is_filter then
                        raise (Shell_ex (std_NOTFOUND,src));
                    path',filter;
                    in
                match dst_type with
                | File_obj ->
                    raise (Shell_ex (std_IOERR,
                            ("Invalid source: "^src)));
                | Dir_obj ->
                    copy_dir_u2a root_cs src_dir' last_dst_cs dst filter;
                | _ ->
                    raise (Shell_ex (std_IOERR,
                            ("Invalid source: "^src)));
            end;
      end;
      | Amoeba_path src ->
      begin
        match dst with
        | Unix_path dst ->
        begin
            (*
            ** Get informations about src and dst
            *)

            let stat,root = get_env_cap "ROOT" in
            if stat <> std_OK then
                raise (Shell_ex (stat,"get_env_cap of ROOT failed"));
            let root_cs = cs_singleton root in

            let src_type = get_amoeba_type root_cs src in

            let last_src_cs = !last_dir_cs in

            (*
            ** Either dst is a directory (must exist) or a destination
            ** file.
            *)

            let dst_type = get_unix_type dst in
            
            match src_type with
            | File_obj ->
            begin
                match dst_type with
                | Nil_obj 
                | File_obj ->
                    let src_dir' = Filename.dirname src in
                    let src_file = Filename.basename src in
                    let _,src_dir_cs = dns_lookup root_cs src_dir' in
                    copy_file_a2u root_cs 
                                  src
                                  src_dir_cs
                                  src_file
                                  dst;
                | Dir_obj ->
                    let src_dir' = Filename.dirname src in
                    let src_file = Filename.basename src in
                    let _,src_dir_cs = dns_lookup root_cs src_dir' in
                    copy_file_a2u root_cs 
                                  src
                                  src_dir_cs
                                  src_file 
                                  (dst^"/"^src_file);
                | _ -> raise (Shell_ex (std_IOERR,
                                   ("Invalid destination: "^dst)));
            end
            | Dir_obj ->
            begin
                (*
                ** A directory.
                *)
                match dst_type with
                | Dir_obj ->
                    copy_dir_a2u  root_cs
                                  src
                                  last_src_cs
                                  dst
                                  "";
                | _ -> raise (Shell_ex (std_IOERR,
                                   ("Invalid destination: "^dst)));
            

            end;
            | Cap_obj ->
            begin
                match dst_type with
                | Nil_obj 
                | File_obj ->
                    copy_cap_a2u  root_cs 
                                  last_src_cs
                                  src
                                  dst;
                | Dir_obj ->
                    let obj_name = Filename.basename src in
                    copy_cap_a2u  root_cs 
                                  last_src_cs
                                  src
                                  (dst^"/"^obj_name);
                | _ -> raise (Shell_ex (std_IOERR,
                                   ("Invalid destination: "^dst)));

            end;
            | _ -> raise (Shell_ex (std_IOERR,
                                   ("Invalid source: "^src)));
        end;


        | Amoeba_path dst -> 
        begin
            let stat,root = get_env_cap "ROOT" in
            if stat <> std_OK then
                raise (Shell_ex (stat,"get_env_cap of ROOT failed"));
            let root_cs = cs_singleton root in

            let src_type = get_amoeba_type root_cs src in
            let last_src_cs = !last_dir_cs in

            (*
            ** Either dst is a directory (must exist) or a destination
            ** file (may or may not exist).
            *)

            let dst_type = get_amoeba_type root_cs dst in
            let last_dst_cs = !last_dir_cs in
            
            match src_type with
            | File_obj ->
            begin
                match dst_type with
                | File_obj ->
                    let src_dir' = Filename.dirname src in
                    let src_file = Filename.basename src in
                    let dst_dir' = Filename.dirname dst in
                    let dst_file = Filename.basename dst in
                    let _,src_dir_cs = dns_lookup root_cs src_dir' in
                    let _,dst_dir_cs = dns_lookup root_cs dst_dir' in
                    copy_file_a2a root_cs 
                                  src_dir_cs
                                  src_file
                                  dst_dir_cs
                                  dst_file
                                  src
                                  dst;
                | Dir_obj ->
                    let src_dir' = Filename.dirname src in
                    let src_file = Filename.basename src in
                    let _,src_dir_cs = dns_lookup root_cs src_dir' in
                    copy_file_a2a root_cs 
                                  src_dir_cs
                                  src_file 
                                  last_dst_cs
                                  src_file
                                  src
                                  (dst^"/"^src_file);
                | Nil_obj ->
                    let src_dir' = Filename.dirname src in
                    let src_file = Filename.basename src in
                    let dst_dir' = Filename.dirname dst in
                    let dst_file = Filename.basename dst in
                    let _,src_dir_cs = dns_lookup root_cs src_dir' in
                    let stat,dst_dir_cs = dns_lookup root_cs dst_dir' in
                    if stat <> std_OK then
                        raise (Shell_ex (stat,
                                   ("Invalid destination: "^dst)));
                    copy_file_a2a root_cs 
                                  src_dir_cs
                                  src_file
                                  dst_dir_cs
                                  dst_file
                                  src
                                  dst;
                | _ -> raise (Shell_ex (std_IOERR,
                                   ("Invalid destination: "^dst)));
            end;
            | Dir_obj ->
            begin
                (*
                ** A directory.
                *)
                copy_dir_a2a root_cs src last_src_cs
                                     dst last_dst_cs "";
            end;
            | Cap_obj ->
            begin
                match dst_type with
                | Dir_obj ->
                begin
                    let obj_cs = last_src_cs in
                    let obj_name = Filename.basename src in
                    copy_cap_a2a  root_cs 
                                  obj_cs
                                  last_dst_cs
                                  obj_name
                                  src
                                  (dst^"/"^obj_name);
                    
                end;
                | _ ->
                begin
                    let obj_cs = last_src_cs in

                    let obj_name = Filename.basename dst in
                    let dst_dir' = Filename.dirname dst in

                    let stat,dst_dir_cs = dns_lookup root_cs dst_dir' in
                    if stat <> std_OK then
                        raise (Shell_ex (stat,
                                   ("Invalid destination: "^dst)));
                    copy_cap_a2a  root_cs 
                                  obj_cs
                                  dst_dir_cs
                                  obj_name
                                  src
                                  dst;
                    
                end;
            end;
            | _ -> raise (Shell_ex (std_IOERR,
                                   ("Invalid source: "^src)));
        end;
      end;
    );
    __(Rpc.timeout old_timeout);
    !msg
  end
  with
    | Shell_ex (err,str) -> __(Rpc.timeout old_timeout);
                            fatal (sprintf "%s: %s\n" str (err_why err));
                            !msg
    | Format_error str -> __(Rpc.timeout old_timeout);
                          fatal (sprintf "Invalid format: %s\n" str);
                          !msg
    | _ -> __(Rpc.timeout old_timeout);
           fatal ("Internal Error\n");
           !msg


(*
** Delete files, generic capabilities and directories.
**
** Both the UNIX and Amoeba
** filesystems are supported. The operations to be performed are
** specified in the args string:
**
**
**  r      : delete a specified directory (recursive) with
**           all subdirectories contained.
**
**  f      : destroy found files before deleting the directory
**
**  d      : delete with check of object type
**
**  s      : return status string
**
** Arguments must be divided by spaces.
**
** Example (;-(:
**      del "/unix/etc" "r"
**
** Note: The last component of a path is always deleted, too, even in the
**       case of directory! That means in the above example, etc is 
**       removed itself.
**
*)

let del path args =
  let msg = ref "" in
  let do_log = ref false in
  let log str = 
      if !do_log then
      begin
        msg := !msg ^ str;
        !shell_log_fun str;
      end
      in
  let fatal str = 
      msg := !msg ^ str;
      !shell_log_fun str;
      in

  let old_timeout = Rpc.timeout 5000 in
  try
  begin
    let get_stat ss = match ss with
                      | Some s -> s;
                      | None -> raise (Shell_ex (std_SYSERR,
                                                "programming error"));
            in

    (*
    ** Determine type of a Unix object
    *)

    let get_unix_type path =
        let ustat = ref None in
        let exists = protects (ustat := Some (Unix.stat path)) in 
        if exists then
        begin
            let ustat = get_stat !ustat in
            match ustat.st_kind with            
            | S_REG -> if ustat.st_size <> cap_SIZE then
                            File_obj
                       else
                            Cap_obj;       (* sufficient criteria ? *)
            | S_DIR -> Dir_obj;
            | _ -> Nil_obj;            
        end
        else
            Nil_obj
        in

    (*
    ** Cache last dns lookup result - speed up operation!
    *)
    let last_dir_cs = ref nilcapset in

    (*
    ** Determine type of Amoeba object
    *)
    let get_amoeba_type root_cs path =
        let stat,path_cs = dns_lookup root_cs path in
        last_dir_cs := path_cs;
        if stat = std_OK then
        begin
            (* 
            ** Exist! Determine type of object...
            *)
            let _,cap = cs_to_cap path_cs in
            let stat,str = std_info cap 10 in
            if stat = std_OK then
            begin
                match str.[0] with
                | a when a = objsym_DIRECT -> Dir_obj;
                | a when a = objsym_FILE -> File_obj;
                | _ -> Cap_obj;
            end
            else
                Cap_obj
        end
        else
            Nil_obj
        in

    let file_destroy,recursive,destroy,status =
        let f,r,d,s=ref false,ref false, ref false, ref false in
        let argl = Str.split (Str.regexp " ") args in
        List.iter (fun arg ->
            match arg with
            | "d" -> d:= true;
            | "f" -> f:= true;
            | "r" -> r:= true;
            | "s" -> s:= true;
            | _ -> raise (Shell_ex (std_ARGBAD,arg)); 
            ) argl;
        !f,!r,!d,!s
        in
    if status then do_log := true;

    if path = "" || path = "/" then
        raise (Shell_ex (std_ARGBAD,("this / is not what you want !?")));

    (*
    ** Delete a directory component by component with all its
    ** sub directories...
    *)
    let del_dir_a root_cs dir dir_cs filter =
        let rec del_dir src src_cs =

            (*
            ** Get all entries from src.
            ** Distinguish files, caps and sub directories.
            *)
            let _,src_cap = cs_to_cap src_cs in
            let stat,dirdesc = dir_open src_cap in
            if stat <> std_OK then
                raise (Shell_ex (stat,("dir open failed: "^src)));

            for i = 0 to (dirdesc.dir_nrows - 1)
            do
                let dr = dir_next dirdesc in
                let name  = dr.dr_name in
                let src_path =  src ^ "/" ^ name in
                let src_type = get_amoeba_type root_cs src_path in
                let last_src_cs = !last_dir_cs in

                if (filter = "" || 
                    (is_filter name filter)) then
                match src_type with
                | File_obj ->
                begin
                    if file_destroy then
                    begin
                        let _,cap = cs_to_cap last_src_cs in
                        let stat = std_destroy cap in
                        if stat <> std_OK then
                            raise (Shell_ex (stat,
                                        ("std destroy failed: "^src_path)));
                    end; 

                    let stat = dns_delete src_cs name in
                    if stat <> std_OK then
                        raise (Shell_ex (stat,("delete failed: "^path)));
                    if file_destroy then
                        log (sprintf "deleted : %s\n" src_path)
                    else
                        log (sprintf "removed : %s\n" src_path);
                end;
                | Cap_obj ->
                begin
                    let stat = dns_delete src_cs name in
                    if stat <> std_OK then
                        raise (Shell_ex (stat,("delete failed: "^path)));
                    log (sprintf "removed : %s\n" src_path);
                end;
                | Dir_obj ->
                begin
                    let src_path = src ^ "/" ^ name in

                    if not recursive then
                        raise (Shell_ex (stat,("recursive mode needed for: "^src_path)));

                    (*
                    ** First goto subdirectories and delete all components...
                    *)
                    del_dir src_path last_src_cs;

                    (*
                    ** Finally remove directory itself.
                    *)
                    let stat = dns_delete src_cs name in
                    if stat <> std_OK then
                        raise (Shell_ex (stat,("delete failed: "^src_path)));
                    log (sprintf "removed dir: %s\n" src_path);                    
                end;
                | Nil_obj -> raise (Shell_ex (stat,("invalid source: "^src_path)));
            done;
            in
        del_dir dir dir_cs;

        if filter = "" then
        begin
            (*
            ** Now remove the directory itself.
            *)
            let par_dir = Filename.dirname dir in
            let name = Filename.basename dir in

            let stat,par_cs = dns_lookup root_cs par_dir in
            if stat <> std_OK then
                raise (Shell_ex (stat,("can't lookup parent dir of: "^dir)));
        
            let stat = dns_delete par_cs name in
            if stat <> std_OK then
                raise (Shell_ex (stat,("delete failed: "^dir)));
            log (sprintf "removed dir: %s\n" dir);        
        end;
        in  

    let del_dir_u dir filter =
        let rec del_dir src =

            (*
            ** Get all entries from src.
            ** Distinguish files, caps and sub directories.
            *)
            let dirdesc = ref None in
            protect (dirdesc := Some(Unix.opendir src)); 
            let dirdesc = match !dirdesc with
                          | None ->
                            raise (Shell_ex (std_IOERR,
                                                ("opendir failed: "^src)));
                          | Some dh -> dh;
                in
            let curname = ref "" in
            while (protects (curname := Unix.readdir dirdesc)) <> false
            do
                let src_path =  src ^ "/" ^ !curname in
                let src_type = get_unix_type src_path in

                if (filter = "" || 
                    (is_filter !curname filter)) then
                match src_type with
                | File_obj ->
                begin
                    let failed = not (protects (Unix.unlink src_path)) in
                    if failed then
                        raise (Shell_ex (std_IOERR,("delete failed: "^src_path)));
                    log (sprintf "deleted : %s\n" src_path);
                end;
                | Cap_obj ->
                begin
                    let failed = not (protects (Unix.unlink src_path)) in
                    if failed then
                    raise (Shell_ex (std_IOERR,("delete failed: "^src_path)));
                    log (sprintf "deleted : %s\n" src_path);
                end;
                | Dir_obj ->
                begin
                    let src_path = src ^ "/" ^ !curname in

                    if not recursive then
                        raise (Shell_ex (std_IOERR,
                                        ("recursive mode needed for: "^src_path)));

                    (*
                    ** First goto subdirectories and delete all components...
                    *)
                    del_dir src_path;

                    (*
                    ** Finally remove directory itself.
                    *)
                    let failed = not (protects (Unix.unlink src_path)) in
                    if failed then
                        raise (Shell_ex (std_IOERR,
                                            ("delete failed: "^src_path)));
                    log (sprintf "deleted dir: %s\n" src_path);
                end;
                | Nil_obj -> 
                    raise (Shell_ex (std_IOERR,("invalid source: "^src_path)));
            done;
            in
        del_dir dir;

        if filter = "" then
        begin
            (*
            ** Now remove the directory itself.
            *)
            let par_dir = Filename.dirname dir in

            let failed = not (protects (Unix.unlink par_dir)) in
            if failed then
                raise (Shell_ex (std_IOERR,("delete failed: "^par_dir)));
            log (sprintf "deleted dir: %s\n" par_dir);
        end;
        in  

    (
      match (path_resolve path) with
      | Amoeba_path path ->
      begin
        let stat,root_cap = get_env_cap "ROOT" in
        if stat <> std_OK then
            raise (Shell_ex (stat,"get_env_cap of ROOT failed"));
        let root_cs = cs_singleton root_cap in


        if not destroy then
        begin
            let path_type = get_amoeba_type root_cs path in
            let last_path_cs = !last_dir_cs in

            let path',filter = 
                match path_type with
                | File_obj
                | Cap_obj 
                | Dir_obj -> path,"";
                | Nil_obj ->
                begin
                    (*
                    ** Check filter operation
                    *)
                    let path' = Filename.dirname path in
                    let filter = Filename.basename path in
                    let is_filter = String.contains filter '*' in
                    if not is_filter then
                        raise (Shell_ex (std_NOTFOUND,path));
                    path',filter;
                end;
                in

            match path_type with
            | Cap_obj ->
            begin
                let stat = dir_delete root_cap path' in
                if stat <> std_OK then
                    raise (Shell_ex (stat,("delete failed: "^path)));
                log (sprintf "removed: %s\n" path);
            end;
            | File_obj ->
            begin
                if file_destroy then
                begin
                    let _,cap = cs_to_cap last_path_cs in
                    let stat = std_destroy cap in
                    if stat <> std_OK then
                        raise (Shell_ex (stat,("std destroy failed: "^path)));
                end; 
                let stat = dir_delete root_cap path' in
                if stat <> std_OK then
                    raise (Shell_ex (stat,("delete failed: "^path)));

                if status && file_destroy then
                    log (sprintf "file destroyed: %s\n" path)
                else if status then
                    log (sprintf "file removed: %s\n" path);
            end;
            | Nil_obj ->
            begin
                (*
                ** Filter mode
                *)
                let stat,dir_cs = dns_lookup root_cs path' in
                if stat <> std_OK then
                    raise (Shell_ex (stat,("dns lookup failed: "^path')));
                del_dir_a root_cs path' dir_cs filter;
            end;
            | Dir_obj  ->
                if not recursive then
                    raise (Shell_ex (std_SYSERR,
                                    ("recursive mode needed for: "^path)));
                del_dir_a root_cs path last_path_cs filter;
        end
        else 
        begin
            (*
            ** Simply remove specified object from parent directory.
            *)
            let stat = dir_delete root_cap path in
            if stat <> std_OK then
                raise (Shell_ex (stat,("delete failed: "^path)));

            log (sprintf "directory removed: %s\n" path);
        end;
      end;
      | Unix_path path ->
      begin

        let path_type = get_unix_type path in

        let path',filter = 
                match path_type with
                | File_obj
                | Cap_obj 
                | Dir_obj -> path,"";
                | Nil_obj ->
                begin
                    (*
                    ** Check filter operation
                    *)
                    let path' = Filename.dirname path in
                    let filter = Filename.basename path in
                    let is_filter = String.contains filter '*' in
                    if not is_filter then
                        raise (Shell_ex (std_NOTFOUND,path));
                    path',filter;
                end;
                in

        match path_type with
        | Cap_obj ->
        begin

                let failed = not (protects (Unix.unlink path')) in
                if failed then
                    raise (Shell_ex (std_IOERR,("delete failed: "^path')));
                log (sprintf "deleted : %s\n" path');
        end;
        | File_obj ->
        begin
                let failed = not (protects (Unix.unlink path')) in
                if failed then
                    raise (Shell_ex (std_IOERR,("delete failed: "^path')));
                log (sprintf "deleted : %s\n" path');
        end;
        | Nil_obj ->
        begin
                (*
                ** Filter mode
                *)
                del_dir_u path' filter;
        end;
        | Dir_obj  ->
                if not recursive then
                    raise (Shell_ex (std_SYSERR,
                                    ("recursive mode needed for: "^path)));
                del_dir_u path' "";
      end;
    );
    __(Rpc.timeout old_timeout);
    !msg
  end
  with
    | Shell_ex (err,str) -> __(Rpc.timeout old_timeout);
                            fatal (sprintf "%s: %s\n" str (err_why err));
                            !msg
    | Format_error str ->   __(Rpc.timeout old_timeout);
                            fatal (sprintf "Invalid format: %s\n" str);
                            !msg
    | _ -> __(Rpc.timeout old_timeout);
           fatal "Internal Error\n";
           !msg

(*
** Create a new directory (and if needed some more upper level directories)
*)

let mkdir path args =
  let msg = ref "" in
  let do_log = ref false in
  let log str = 
      if !do_log then
      begin
        msg := !msg ^ str;
        !shell_log_fun str;
      end
      in
  let fatal str = 
      msg := !msg ^ str;
      !shell_log_fun str;
      in

  let old_timeout = Rpc.timeout (2 * !shell_dir_timeout) in
  try
  begin

    let status =
        let s=ref false in
        let argl = Str.split (Str.regexp " ") args in
        List.iter (fun arg ->
            match arg with
            | "s" -> s:= true;
            | _ -> raise (Shell_ex (std_ARGBAD,arg)); 
            ) argl;
        !s
        in
    if status then do_log := true;

    (
      match (path_resolve path) with
      | Amoeba_path path -> 
      begin
        let pathl = Str.split (Str.regexp "/") path in
        let stat,root_cap = get_env_cap "ROOT" in
        if stat <> std_OK then
            raise (Shell_ex (stat,"get_env_cap of ROOT failed"));
        let root_cs = cs_singleton root_cap in
    
        let par_dir_cs = ref root_cs in
        let path' = ref "" in
        List.iter (fun pe ->
                path' := !path' ^ "/" ^  pe;

                let stat,dir_cs = dns_lookup !par_dir_cs pe in

                if stat <> std_OK then
                begin
                    (*
                    ** It seems there is a missing directory - create it
                    *)
                    let col_names = dns_DEFAULT_COLS in
                    let stat,dir_cs = dns_create !par_dir_cs col_names in

                    if stat <> std_OK then
                            raise (Shell_ex (stat,
                                            ("can't create dir: " ^ !path')));

                    let _,cols = default_colmask () in

                    let stat = dns_append ~dir:  !par_dir_cs 
                                          ~name: pe
                                          ~obj:  dir_cs
                                          ~cols: cols in

                    if stat <> std_OK then
                        raise (Shell_ex (stat,"dns append failed: " ^ !path'));

                    par_dir_cs := dir_cs;

                    log (sprintf "created directory %s\n" !path');
                end 
                else
                    par_dir_cs := dir_cs;
                
            ) pathl;
      end;
      | Unix_path path -> 
      begin
        (* drwxr-xr-x *)
        let unix_dir_perm = ref 493 in
        let pathl = Str.split (Str.regexp "/") path in

        let path' = ref "" in
        List.iter (fun pe ->
                path' := !path' ^ "/" ^  pe;

                let exists = protects (__(Unix.stat !path')) in

                if not exists then
                begin
                    (*
                    ** It seems there is a missing directory - create it
                    *)
                    let failed = not (protects (Unix.mkdir !path'
                                                      !unix_dir_perm)) in
                    
                    if failed then
                            raise (Shell_ex (std_IOERR,
                                            ("can't create dir: " ^ !path')));

                    log (sprintf "created directory %s\n" !path');
                end; 
                
            ) pathl;
      end;
    );
    __(Rpc.timeout old_timeout);
    !msg
  end
  with
    | Shell_ex (err,str) -> __(Rpc.timeout old_timeout);
                            fatal (sprintf "%s: %s\n" str (err_why err));
                            !msg
    | Format_error str ->   __(Rpc.timeout old_timeout);
                            fatal (sprintf "Invalid format: %s\n" str);
                            !msg
    | _ -> __(Rpc.timeout old_timeout);
           fatal "Internal Error\n";
           !msg

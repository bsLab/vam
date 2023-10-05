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
**    $INITIAL:     (C) 2003-2005 BSSLAB
**    $CREATED:     ??
**    $VERSION:     1.02
**
**    $INFO:
**
**
**
**    $ENDOFINFO
**
*)



open Amoeba
open Bytebuf
open Buf
open Stderr
open Stdcom
open Stdcom2
open Name
open Dir
open Cap_env
open Capset

open Vash_io
open Vash_env

open Dns_common
open Dns_client
open Afs_common
open Afs_client
open Ar

open Unix
open Printf


(*
** Get default file server capability for current parent directory dir!
*)
let init_file_cap parentdir =
    let stat,dircap = name_lookup parentdir in
    if stat = std_OK then
    begin
        let dircs = cs_singleton dircap in
        let stat,cap = dns_getdefafs dircs in
        if stat = std_OK then
            file_cap := cap
    end
    else if (!file_cap = nilcap) then
    begin
        let stat,cap = get_env_cap "AFS" in 
        if (stat <> std_OK) then
        begin
            let stat,cap = name_lookup "/server/afs" in
            if (stat = std_OK) then
                file_cap := cap
            else
            begin
                let stat,cap = name_lookup "/profile/cap/afs/server" in
                if (stat = std_OK) then
                    file_cap := cap
                else
                begin
                    out "no way to get the fileserver cap!";  nl();
                    raise (Error std_NOTFOUND); (* no idea anymore ! *)
                end;
            end; 
        end
        else
            file_cap := cap;
    end




let cp args =
  try
  begin

    let usage err =
            out "usage: cp [-h -o -f] <source> <destination> "; nl ();
            out "       -o: overwrite mode"; nl ();
            out "       -f: destroy object while in overwrite mode"; nl ();
            raise (Error err);
        in

    let opt_ovw = ref false in
    let opt_for = ref false in

    let src = ref "" in
    let dst = ref "" in

    let rec iter args =
        match args with
        | hd::tl -> 
        begin
            match hd with
            | "-h" -> usage std_OK;
            | "-o" -> opt_ovw := true; iter tl;
            | "-f" -> opt_for := true; iter tl;
            | _ -> 
            begin
                if hd.[0] = '-' then
                begin
                    out ("unknown option: "^hd); nl ();
                    usage std_ARGBAD; 
                end;
                if (!src = "") then
                begin
                    src := hd
                end
                else if (!dst = "") then
                    dst := hd 
                else
                    usage std_ARGBAD;

                iter tl;
            end;
        end;
        | [] -> ()
    in                    
    iter args;

    if (!src = "" || 
        !dst = "") then
        usage std_ARGBAD;

    
    let src_file = Filename.basename !src in

    let src = 
        if (!src.[0] = '/') then
            path_resolve !src
        else
        match !work_dir with
        | Amoeba_path wd ->
            Amoeba_path (wd^"/"^(!src));
        | Unix_path wd ->
            Unix_path (wd^"/"^(!src));
            
    in

    (*
    ** Maybe a destination directory ?
    *)
    let dst = 
        let path = 
          if (!dst.[0] = '/') then
              path_resolve !dst
          else
          match !work_dir with
          | Amoeba_path wd ->
              Amoeba_path (wd^"/"^(!dst));
          | Unix_path wd ->
              Unix_path (wd^"/"^(!dst));
          in    
        match path with
        | Amoeba_path path ->
          let stat,cap = name_lookup path in
          if stat <> std_OK then
            Amoeba_path path
          else
          begin
            let stat,str = std_info cap 50 in
            if stat <> std_OK then 
            begin
                out "std_info of dst failed"; nl ();
                raise (Error stat);
            end;
            if str.[0] = '/' then	
              (*
              ** must be a directory - append src file 
              *)
              Amoeba_path (path ^ "/" ^ src_file)
            else
              Amoeba_path path;
          end;
        | Unix_path path -> 
          try
          begin
            let stat = Unix.stat path in
            if stat.st_kind = S_DIR then
              Unix_path (path ^ "/" ^ src_file)
            else
              Unix_path path
          end
          with _ -> Unix_path path;
    in	

    match src with
    | Amoeba_path f1 ->
    begin
        match dst with 
        | Amoeba_path f2 ->
        begin
            std_OK
        end;
        | Unix_path f2 ->
        begin
            (*
            ** Lookup the afs file object 
            *)
            let stat,cap1 = name_lookup f1 in
            if (stat <> std_OK) then
            begin
                out "name_lookup of src failed"; nl ();
                raise (Error stat);
            end;
            (*
            ** determine afs file size 
            *)
            let stat,size = afs_size cap1 in
            if (stat <> std_OK) then
            begin
                out "afs_size of src failed"; nl ();
                raise (Error stat);
            end;
            (*
            ** Open unix file for writing 
            *)
            let fd = Unix.openfile f2
                                   ([O_RDWR;O_CREAT]@
                                        (if !opt_ovw = true then
                                            []
                                         else
                                            [O_EXCL]))
                                    (256+128) in
            
            let buf_size = if size > 10000 then 10000 else size in
            let buf = buf_create buf_size in
            
            let frags = size/buf_size in
            let off = ref 0 in
            let fsize = ref (if (frags = 0) then
                                 size
                             else
                                 buf_size) 
                in

            for i = 0 to frags 
            do
                (*
                ** read from afs server
                *)
                let stat,n = afs_read ~cap:cap1 
                                      ~offset:!off
                                      ~buf:buf
                                      ~size:!fsize
                in
                if (stat <> std_OK) then
                begin
                    out "afs_read of src failed"; nl ();
                    raise (Error stat);
                end;

                (*
                ** and write to unix file
                *)

                let n = Unix.writeb fd buf 0 !fsize in
                
                if (n <> !fsize) then
                    raise (Error std_IOERR);
                off := !off + !fsize;
                fsize := min buf_size (size - !off);
            done;
            Unix.close fd;
            std_OK
        end;
    end;
    | Unix_path f1 ->
    begin
        match dst with 
        | Amoeba_path f2 ->
        begin
            (*
            ** Lookup the dns parent object 
            *)
            let parent = Filename.dirname f2 in
            let name = Filename.basename f2 in

            init_file_cap parent;

            let stat,dir = name_lookup parent in
            if (stat <> std_OK) then
            begin
                out "name_lookup of dst failed"; nl ();
                raise (Error stat);
            end;
            (*
            ** determine afs file size 
            *)
            let fstat = Unix.stat f1 in
            let size = fstat.st_size in

            (*
            ** Open unix file for reading 
            *)
            let fd = Unix.openfile f1
                                   [O_RDONLY]
                                   (256+128) in
            
            let buf_size = if size > 10000 then 10000 else size in
            let buf = buf_create buf_size in
            (*
            ** Create the afs file 
            *)

            let stat,cap2 = afs_create ~cap:!file_cap
                                       ~buf:nilbuf
                                       ~size:0
                                       ~commit:0
                in
            if (stat <> std_OK) then
            begin
                out "afs_create of dst failed"; nl ();
                raise (Error stat);
            end;

            let frags = size/buf_size in
            let off = ref 0 in
            let fsize = ref (if (frags = 0) then
                                 size
                             else
                                 buf_size) 
                in

            for i = 0 to frags 
            do
                (*
                ** read from unix file
                *)

                let n = Unix.readb fd buf 0 !fsize in
                if (n <> !fsize) then
                    raise (Error std_IOERR);

                (*
                ** modify afs file
                *)
                let stat,_ = afs_modify ~cap:cap2 
                                        ~offset:!off
                                        ~buf:buf
                                        ~size:!fsize
                                        ~commit:0
                    in
                if (stat <> std_OK) then
                begin
                    out "afs_modify of dst failed"; nl ();
                    raise (Error stat);
                end;

                off := !off + !fsize;
                fsize := min buf_size (size - !off);
            done;
            Unix.close fd;
            let stat,_ =    afs_modify ~cap:cap2
                                       ~offset:0
                                       ~buf:nilbuf
                                       ~size:0
                                       ~commit:afs_COMMIT
                in
            if (stat <> std_OK) then
            begin
                out "afs_modify of dst failed"; nl ();
                raise (Error stat);
            end;
            if (!opt_ovw = true) then
            begin
                let stat,cap = dir_lookup ~root:dir
                                         ~name:name
                    in
                if (stat = std_OK) then
                begin
                    let stat = dir_delete ~root:dir
                                          ~name:name
                        in
                    if (stat <> std_OK) then
                    begin
                        out "dir_delete failed."; nl ();
                        raise (Error stat);
                    end;
                    if (!opt_for = true) then
                        ignore(std_destroy cap);
                 end;
            end;
            (*
            ** Append the new file cap to the directory
            *)

            let stat = dir_append ~root:dir
                                  ~name:name
                                  ~obj:cap2
                in

            if (stat <> std_OK) then
            begin
                out (sprintf "dir_append of dst failed (dir=%s)"
                     (ar_cap dir)); nl ();
                raise (Error stat);
            end;
      
            std_OK
        end;
        | Unix_path f2 ->
        begin
            std_OK
        end;
    end;
  end
  with
     | Error err -> err
     | _ -> std_IOERR

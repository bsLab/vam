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
**      BSSLAB, Dr. Stefan Bosse www.bsslab.de
**
**    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF: 
**    Free Software Foundation-Europe, GNU GPL License, Version 2
**
**    $MODIFIEDBY:  BSSLAB
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2003 BSSLAB
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.06
**
**    $INFO:
**
** Main module of the Atomic Filesystem Server (AFS). Unix version.
**
**
**    $ENDOFINFO
**
*)


open Amoeba
open Bytebuf
open Stderr
open Stdcom
open Thread
open Afs_common
open Afs_server
open Afs_server_rpc
open Afs_cache
open Afs_unix
open Buf
open Unix
open Printf
open Cap_env
open Name
open Syslog

let nl = print_newline

let print_version () =
    print_string ("AFS: Atomic Filesystem Server: Version "^
                    (server_version));
    nl ();
    print_string          "     (C) 2003-2005 BSSLAB Dr. Stefan Bosse";
    nl ();
    exit 0


let lpath = ref def_lpath 
let cappath = ref def_lpath
let capfile () = (!cappath^"/.servercap") 
let part_inode_file ()  = (!lpath^"/"^def_part_inode)
let part_data_file ()  = (!lpath^"/"^def_part_data)
let nthr    = ref 4 
let ninodes = ref 10000 
let label   = ref "Filesystem"
let blksize = ref def_block_size
let nblocks = ref 10000
let docreate = ref false 
let dostart  = ref false
let overwrite = ref false 
let doshowx   = ref false
let showl     = ref false
let dodefrag  = ref false
let maxdefrag = ref 10 

let cache_param = { 
        c_inode_buffers = 1000;
        c_inode_size = 1;
        c_data_buffers = 100;
        c_data_size = 30;
    } 

let usage_str = "
Program Arguments:

  -c : Create a filesystem

      -n : Number of blocks ["^(string_of_int !nblocks)^"]"^"
      -b : Block size [in bytes, "^(string_of_int !blksize)^"]"^"
      -i : Number of inodes [=maximal number of files, "^
      (string_of_int !ninodes)^"]"^"

      -P : Partition path [\n            "^(!lpath)^"]"^"
      -C : Path name for the super capability [write, \n            "^
      (!cappath)^"]"^"

      -o : Overwrite mode
 
  -s : Start the filesystem (already created)

      -P : Partition path [\n            "^(!lpath)^"]"^"
      -C : Path name for the super capability [write, \n            "^
      (!cappath)^"]"^"

      -Nd : Number of data cache buffers ["^
      (string_of_int cache_param.c_data_buffers)^"]"^" 
      -Sd : Size of each data buffer [in blocks, "^
      (string_of_int cache_param.c_data_size)^"]"^"
      -Ni : Number of inode cache buffers ["^
      (string_of_int cache_param.c_inode_buffers)^"]"^"
      -Si : Size of each data buffer [in blocks, "^
      (string_of_int cache_param.c_inode_size) ^"]"^"
      -t : Number of service threads ["^
      (string_of_int !nthr)^"]"^"

  -X : Show the status of all blocks of the filesystem
      -L : show cluster list [graphical default]

  -D : Try a defragmentation of the filesystem. All files
       with a filesuze greater a threshold will moved up
       to the end of the filesystem, if possible.

      -M : maximal filesize [default "^(string_of_int !maxdefrag)^" blocks]

  -d : enable builtin debugger
  -syslog : enable system logging
  -h : Print this help message.
  -V : Print the server version

    Path convention:
    /unix/... -> located on local UNIX filesystem
    else      -> located on Amoeba filesystem, specified with ROOTCAP
"

let usage () =
    print_string usage_str;
    nl ();
    exit (-1)

let create () =
    let part_inode = match (path_resolve (part_inode_file())) with
                     | Unix_path p -> p;
                     | Amoeba_path _ -> 
                     print_string "part_inode must be a UNIX file!"; nl();
                     raise (Error std_ARGBAD) 
        in
    let part_data  = match (path_resolve (part_data_file())) with
                     | Unix_path p -> p;
                     | Amoeba_path _ -> 
                     print_string "part_data must be a UNIX file!"; nl();
                     raise (Error std_ARGBAD) 
        in
                    
    let stat,super = create_unix_fs 
                    ~label:!label
                    ~ninodes:!ninodes
                    ~blocksize:!blksize
                    ~nblocks:!nblocks
                    ~part_inode:part_inode
                    ~part_data:part_data
                    ~overwrite:!overwrite
    in

    if (stat = std_OK) then
    begin
        (*
        ** Create the public super capability file.
        *)
        let rootcap = { cap_port = super.afs_putport;
                    cap_priv = prv_encode ~obj:(Objnum 0)
                                          ~rights:prv_all_rights 
                                          ~rand:super.afs_checkfield
        } in
    
        match (path_resolve (capfile ())) with
        | Unix_path p ->
            let buf = buf_create cap_SIZE in
            let pos = buf_put_cap ~buf:buf ~pos:0 ~cap:rootcap in
            let fd  = Unix.openfile p
                            [O_CREAT;O_RDWR]
                            384
                in
            let wrn = Unix.writeb fd buf 0 cap_SIZE in
            if (wrn <> cap_SIZE) then
            begin
                print_string "AFS: Creation of super cap failed!";
                nl ();
            end;
        | Amoeba_path p ->
            let stat,_ = name_lookup p in
            if stat = std_OK then
            begin
                let stat = name_delete p in
                if stat <> std_OK then
                begin
                    print_string "AFS: can't delete old supercap"; nl();
                    raise (Error stat);
                end;
            end; 
            let stat = name_append p rootcap in
            if stat <> std_OK then
            begin
                print_string "AFS: can't append new supercap"; nl();
                raise (Error stat);
            end;
    end;

    print_string ("AFS: Status -> "^(err_why stat));
    nl ()
    
    

let start () =
    let sema = Sema.sema_create 0 in
    let part_inode = match (path_resolve (part_inode_file())) with
                     | Unix_path p -> p;
                     | Amoeba_path _ -> 
                     print_string "part_inode must be a UNIX file!"; nl();
                     raise (Error std_ARGBAD) 
        in
    let part_data  = match (path_resolve (part_data_file())) with
                     | Unix_path p -> p;
                     | Amoeba_path _ -> 
                     print_string "part_data must be a UNIX file!"; nl();
                     raise (Error std_ARGBAD) 
        in

    let stat,server =
        start_unix_fs 
                    ~part_inode:part_inode
                    ~part_data:part_data
                    ~cache:cache_param
    in
    if (stat <> std_OK) then
        failwith ("start_unix_fs failed: "^(err_why stat));

    let super = server.afs_super in

    (*
    ** Publish the public super capability.
    *)
    let rootcap = { cap_port = super.afs_putport;
                    cap_priv = prv_encode ~obj:(Objnum 0)
                                          ~rights:prv_all_rights 
                                          ~rand:super.afs_checkfield
        } in

    if !cappath = "" then
    begin
            print_string "AFS: Warning: no name specified for super capability";
            print_newline ();
    end
    else
    begin
        print_string ("AFS: publishing super cap "^(capfile ())^"... ");
        match (path_resolve (capfile ())) with
        | Amoeba_path p ->
            (*
            ** Look for old remains.
            *)
            let stat,_ = name_lookup p in
            if stat = std_OK then
            begin
                let stat = name_delete p in
                if stat <> std_OK then
                begin
                    print_string "can't delete old supercap! ";
                end;
            end;
            let stat = name_append p rootcap in
    
            if stat <> std_OK then
                print_string "failed."
            else    
                print_string "Ok.";
            print_newline ();
        | Unix_path p ->
            let buf = buf_create cap_SIZE in
            let pos = buf_put_cap ~buf:buf ~pos:0 ~cap:rootcap in
            let fd  = Unix.openfile p
                                [O_CREAT;O_RDWR]
                                384
                in
            let wrn = Unix.writeb fd buf 0 cap_SIZE in
            if (wrn <> cap_SIZE) then
            begin
                    print_string "AFS: Creation of super cap failed!";
                    nl ();
            end;
    end;
    print_string "Done."; nl();
    print_string ("AFS: starting "^(string_of_int !nthr)^" server threads...");
    nl () ;

    for i = 1 to !nthr
    do
        ignore(thread_create (fun () -> 
                server_loop ~server:server
                    ~sema:sema
                    ~nthreads:!nthr
                    ~inbuf_size:afs_REQBUFSZ
                    ~outbuf_size:afs_REQBUFSZ
               ) ());
    done;

    print_string "AFS: Ready.";
    nl ();

    for i = 1 to !nthr
    do
        Sema.sema_down sema;
    done;

    let stat = server.afs_exit () in
    if (stat <> std_OK) then
    begin
        print_string ("AFS: live table write failed:"^(err_why stat));
        nl ();
    end;
    print_string "AFS: Exit.";
    nl ()

let showx () =
    admin_fs ~part_inode:(part_inode_file ())
             ~part_data:(part_data_file ())
             ~maxsize:0
             ~mode:(if !showl = true then 0 else 1)

let defrag () =
    admin_fs ~part_inode:(part_inode_file ())
             ~part_data:(part_data_file ())
             ~maxsize:!maxdefrag
             ~mode:10

let _ =
    ignore(get_env_cap "ROOT");

    let args = Array.to_list (Sys.argv) in
    let rec iter al =
        match al with
        | hd::tl ->
        begin
            match hd with 
            | "-c" -> docreate := true; iter tl;
            | "-s" -> dostart  := true; iter tl;
            | "-X" -> doshowx := true; iter tl;
            | "-D" -> dodefrag := true; iter tl;
            | "-L" -> showl := true; iter tl;
            | "-h" | "-help" -> usage ();
            | "-o" -> overwrite := true; iter tl;
            | "-V" -> print_version ();
            | "-n" -> 
            begin
                match tl with
                | hd::tl -> 
                begin
                    nblocks := int_of_string hd;
                    iter tl;
                end;
                | [] -> usage ();
            end;
            | "-b" -> 
            begin
                match tl with
                | hd::tl -> 
                begin
                    blksize := int_of_string hd;
                    iter tl;
                end;
                | [] -> usage ();
            end;
            | "-i" -> 
            begin
                match tl with
                | hd::tl -> 
                begin
                    ninodes := int_of_string hd;
                    iter tl;
                end;
                | [] -> usage ();
            end;
            | "-P" -> 
            begin
                match tl with
                | hd::tl -> 
                begin
                    lpath := hd;
                    if (!cappath = def_lpath) then
                        cappath := !lpath;
                    iter tl;
                end;
                | [] -> usage ();
            end;
            | "-C" -> 
            begin
                match tl with
                | hd::tl -> 
                begin
                    cappath := hd;
                    iter tl;
                end;
                | [] -> usage ();
            end;
            | "-Nd" -> 
            begin
                match tl with
                | hd::tl -> 
                begin
                    cache_param.c_data_buffers <- (int_of_string hd);
                    iter tl;
                end;
                | [] -> usage ();
            end;
            | "-Sd" -> 
            begin
                match tl with
                | hd::tl -> 
                begin
                    cache_param.c_data_size <- (int_of_string hd);
                    iter tl;
                end;
                | [] -> usage ();
            end;
            | "-Ni" -> 
            begin
                match tl with
                | hd::tl -> 
                begin
                    cache_param.c_inode_buffers <- (int_of_string hd);
                    iter tl;
                end;
                | [] -> usage ();
            end;
            | "-Si" -> 
            begin
                match tl with
                | hd::tl -> 
                begin
                    cache_param.c_inode_size <- (int_of_string hd);
                    iter tl;
                end;
                | [] -> usage ();
            end;
            | "-t" -> 
            begin
                match tl with
                | hd::tl -> 
                begin
                    nthr := (int_of_string hd);
                    iter tl;
                end;
                | [] -> usage ();
            end;
            | "-M" -> 
            begin
                match tl with
                | hd::tl -> 
                begin
                    maxdefrag := (int_of_string hd);
                    iter tl;
                end;
                | [] -> usage ();
            end;
            | "-d" ->
            begin
                Debugger.init_debugger ();
                iter tl;
            end;
            | "-syslog" ->
            begin
                syslog_init "";
                iter tl;
            end;

            | _ -> usage ();
        end;
        | [] -> ()
    in
    iter (List.tl args);
    (*
    ** Maybe combined with start operation
    *)
    if (!dodefrag = true) then
        defrag ();

    if (!docreate = true) then
        create ()
    else if (!dostart = true) then
        start ()
    else if (!doshowx = true) then
        showx ()
    else if !dodefrag = false then
        usage ()

     
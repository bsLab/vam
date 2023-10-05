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
**    $INITIAL:     (C)
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.10
**
**    $INFO:
**
**  UNIX version of the Directory and Name server.
**
**    $ENDOFINFO
**
*)




(*
** Main module of the Atomic Filesystem Server (AFS). Unix version.
*)

open Amoeba
open Bytebuf
open Stderr
open Stdcom
open Thread
open Afs_cache
open Dns_common
open Dns_server
open Dns_server_rpc
open Dns_unix
open Buf
open Unix
open Printf
open Ar
open Syslog

let _ = Db.set_level 10

let print_version () =
    print_string ("DNS:  Directory and Name Server: Version "^
                    (server_version));
    print_newline ();

    print_string          "     (C) 2003 BSSLAB, Dr. Stefan Bosse";
    print_newline ();
    exit 0


let lpath = ref def_lpath 
let cappath = ref def_lpath
let servercap () = (!cappath^"/.servercap") 
let rootcap () = (!cappath^"/.rootcap") 
let part_dir ()   = (!lpath^"/"^def_dns_part)
let nthr    = ref 4 
let ninodes = ref 10000 
let label   = ref "Filesystem"
let nblocks = ref 10000
let docreate = ref false 
let dostart  = ref false
let overwrite = ref false 
let fs_server = ref [|nilcap;nilcap|]
let cols = ref [|"Owner";"Group";"Other"|]
let colmasks = ref [|0xff;0x4;0x2|]
let mode = ref Dnsmode_ONECOPY 

let nl = print_newline


let cache_param = { 
        c_inode_buffers = 100;
        c_inode_size = 1;
        c_dir_buffers = 30;
    } 

let usage_str = "
Program Arguments:

  -c : Create a filesystem
     -i : Number of inodes [=maximal number of files, "^
     (string_of_int !ninodes)^"]"^"
     -P : Partition path [\n            "^(!lpath)^"]"^"
     -C : Path for the server and root capability [write, \n            "^
     (!cappath)^"]"^"
     -F1 : File server 1 capability [format: x:x:x:x:x:x/o(r)/x:x:x:x:x:x]
     -F2 : File server 2 capability,opt. [format: x:x:x:x:x:x/o(r)/x:x:x:x:x:x]
     -f1 : File server 1 capability [file name]
     -f2 : File server 2 capability,opt. [file name]
     -m : Server mode [1:one (default), 2:two copy mode]
     -N : Column names [ "^
    (
        let str = ref "" in
        Array.iter (fun c -> str := !str^c^" ") !cols;
        !str
    )^"]"^"
     -R : Column rights [ "^
    (
        let str = ref "" in
        Array.iter (fun c -> str := !str^(sprintf "0x%x" c)^" ") !colmasks;
        !str
    )^"]"^"
     -o : Overwrite mode
 
  -s : Start the filesystem (already created)
      -P : Partition path [\n            "^(!lpath)^"]"^"
      -C : Path name for the server and root capability [read,\n            "^
      (servercap ())^"]"^"
      -Nd : Number of directory cache buffers ["^
      (string_of_int cache_param.c_dir_buffers)^"]"^" 
      -Ni : Number of inode cache buffers ["^
      (string_of_int cache_param.c_inode_buffers)^"]"^"
      -Si : Size of each data buffer [in blocks, "^
      (string_of_int cache_param.c_inode_size) ^"]"^"
      -t : Number of service threads ["^
      (string_of_int !nthr)^"]"^"

  -d : enable builtin debugger
  -syslog : enable system logging
  -h : Print this help message.
  -V : Print the server version
"

let usage () =
    print_string usage_str;
    print_newline ();
    exit (-1)

let create () =
    let ncols = Array.length !colmasks in
    let colrights = Array.create ncols (Rights_bits 0) in
    for i = 0 to ncols-1
    do
        colrights.(i) <- Rights_bits (!colmasks.(i));
    done;

    print_string ("DNS: file server 1 capability: "^
                    (ar_cap !fs_server.(0)));
    print_newline ();
    print_string ("DNS: file server 2 capability: "^
                    (ar_cap !fs_server.(1)));
    print_newline ();

    let stat,super,cap' = create_unix_fs 
                    ~label:!label
                    ~ninodes:!ninodes
                    ~cols:!cols
                    ~colmasks:colrights
                    ~dir_part:(part_dir ())
                    ~fs_server:!fs_server
                    ~overwrite:!overwrite
    in

    if (stat = std_OK) then
    begin
        (*
        ** Create the public server capability file.
        *)
        let cap = { cap_port = super.dns_putport;
                    cap_priv = prv_encode ~obj:(Objnum 0)
                                          ~rights:prv_all_rights 
                                          ~rand:super.dns_checkfield
        } in

        let stat = write_cap (servercap ()) cap in
        if (stat <> std_OK) then
        begin
            print_string "DNS: Creation of server capability failed!";
            print_newline ();
        end;

        (*
        ** Publish the root capability.
        *)

        let stat = write_cap (rootcap ()) cap' in
        if (stat <> std_OK) then
        begin
            print_string "DNS: Creation of root capability failed!";
            print_newline ();
        end;
    end;

    print_string ("DNS: Status -> "^(err_why stat));
    print_newline ()
    
    

let start () =
    let sema = Sema.sema_create 0 in
    let stat,server =
    start_unix_fs 
                    ~dir_part:(part_dir ())
                    ~cache:cache_param
                    ~mode:!mode
    in
    if (stat <> std_OK) then
        failwith ("DNS: start_unix_fs failed: "^(err_why stat));

    let super = server.dns_super in

    print_string ("DNS: starting "^(string_of_int !nthr)^" server threads...");
    print_newline () ;

    for i = 1 to !nthr
    do
        ignore(thread_create (fun () -> 
                server_loop ~server:server
                    ~sema:sema
                    ~nthreads:!nthr
                    ~inbuf_size:dns_REQBUFSZ
                    ~outbuf_size:dns_REQBUFSZ
               ) ());
    done;

    print_string "DNS: Ready.";
    print_newline ();

    for i = 1 to !nthr
    do
        Sema.sema_down sema;
    done;

    let stat = server.dns_exit () in
    if (stat <> std_OK) then
    begin    
        print_string ("DNS: live table write failed:"^(err_why stat));
        nl ();
    end;

    print_string "DNS: Exit.";
    print_newline ()

    
let _ =
    let args = Array.to_list (Sys.argv) in
    let rec iter al =
        match al with
        | hd::tl ->
        begin
            match hd with 
            | "-c" -> docreate := true; iter tl;
            | "-s" -> dostart  := true; iter tl;
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
            | "-F1" -> 
            begin
                match tl with
                | hd::tl -> 
                begin
                    !fs_server.(0) <- ar_tocap hd;
                    iter tl;
                end;
                | [] -> usage ();
            end;
            | "-F2" -> 
            begin
                match tl with
                | hd::tl -> 
                begin
                    !fs_server.(1) <- ar_tocap hd;
                    iter tl;
                end;
                | [] -> usage ();
            end;
            | "-f1" -> 
            begin
                match tl with
                | hd::tl -> 
                begin
                    let fd = Unix.openfile hd [O_RDONLY] 384 in
                    let buf = buf_create cap_SIZE in
                    let n = Unix.readb fd buf 0 cap_SIZE in 
                    let _,cap = try buf_get_cap ~buf:buf ~pos:0 
                                with Buf_overflow -> usage (); 
                        in
                    Unix.close fd;
                    !fs_server.(0) <- cap;
                    iter tl;
                end;
                | [] -> usage ();
            end;
            | "-f2" -> 
            begin
                match tl with
                | hd::tl -> 
                begin
                    let fd = Unix.openfile hd [O_RDONLY] 384 in
                    let buf = buf_create cap_SIZE in
                    let n = Unix.readb fd buf 0 cap_SIZE in 
                    let _,cap = try buf_get_cap ~buf:buf ~pos:0 
                                with Buf_overflow -> usage (); 
                        in
                    Unix.close fd;
                    !fs_server.(1) <- cap;
                    iter tl;
                end;
                | [] -> usage ();
            end;
            | "-m" -> 
            begin
                match tl with
                | hd::tl -> 
                begin
                  (
                    match hd with
                    | "1" -> mode := Dnsmode_ONECOPY
                    | "2" -> mode := Dnsmode_TWOCOPY;
                    | _ -> usage ();
                  );
                  iter tl;
                end;
                | [] -> usage ();
            end;
            | "-Nd" -> 
            begin
                match tl with
                | hd::tl -> 
                begin
                    cache_param.c_dir_buffers <- (int_of_string hd);
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
    if (!docreate = true) then
        create ()
    else if (!dostart = true) then
        start ()
    else usage ()

     
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
**    $AUTHORS:     Steafn Bosse
**    $INITIAL:     (C) 2004-2005 BSSLAB
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.06
**
**    $INFO:
**
**  Boot Disk Server (external version)
**
**    $ENDOFINFO
**
*)

let server_version = "1.05"

(*
** Main module of the BDISK server
*)

open Amoeba
open Bytebuf
open Stderr
open Stdcom
open Ar
open Sema
open Server
open Cap_env
open Printf
open Disk_client
open Disk_common
open Bootdisk_common

let _ = Db.set_level 10

let print_version () =
    print_string ("BDISK:  Boot Disk Server: Version "^
                    (server_version));
    print_newline ();

    print_string  "        Copyright: BSSLAB, Stefan Bosse (2004-2005)";
    print_newline ()

let hostname = ref (let hp = Unix.gethostname () in
                    (*
                    ** Maybe name is develop.bsslab.de, we need
                    ** only the first item.
                    *)
                    let hpdot = Str.split (Str.regexp "\.") hp in
                    List.hd hpdot)
                    

let vdiskpath = ref ""
let hostpath = ref (sprintf "/hosts/%s" !hostname)
let bootdiskname = ref "bootdisk@unknown"
let nthr    = ref 2
let blocksize = ref d_PHYSBLKSZ
let verbose = ref false 
let infomode = ref false 
let create = ref false
let overwrite = ref false

let usage_str = "
bdisk [options] <vdisk>
Program Arguments:
  <vdisk>: The Amoeva Virtual Disk holding the boot disk filesystem
  -H <path>: Host path directory (Amoeba filesystem), where to publish
             the bootdisk directory.
             Default: "^(!hostpath)^"
  -b : Blocksize [bytes]. Default: "^(sprintf "%d" !blocksize)^"
  -n : Number of service threads. Default: "^(sprintf "%d" !nthr)^"
  -h : Print this help message
  -V : Print the server version
  -c : create a bootdisk
  -o : overwrite mode
  -v : Verbose mode
"

let usage str =
    print_string (str^"\n"^usage_str);
    print_newline ();
    exit (-1)

let nl = print_newline 
let info str =
    print_string ("BDISK: "^str); nl ()

    
let _ =
    (* make sure we got our Amoeba root cap ! *)
    let _,rootcap = get_env_cap "ROOTCAP" in
    (*
    ** Scan program arguments...
    *)
    let args = Array.to_list (Sys.argv) in
    let rec iter al =
        match al with
        | hd::tl ->
        begin
            match hd with 
            | "-h" | "-help" -> usage "";
            | "-V" -> print_version (); exit 0;
            | "-v" -> verbose := true; iter tl;
            | "-c" -> create := true; iter tl;
            | "-o" -> overwrite := true; iter tl;
            | "-b" -> 
            begin
                match tl with
                | hd::tl -> 
                begin
                    blocksize := int_of_string hd;
                    iter tl;
                end;
                | [] -> usage "Invalid -b argument";
            end;
            | "-n" -> 
            begin
                match tl with
                | hd::tl -> 
                begin
                    nthr := int_of_string hd;
                    iter tl;
                end;
                | [] -> usage "Invalid -n argument";
            end;
            | "-H" -> 
            begin
                match tl with
                | hd::tl -> 
                begin
                    hostpath := hd;
                    iter tl;
                end;
                | [] -> usage "Invalid -H argument";
            end;
            | _ -> vdiskpath := hd;
        end;
        | [] -> ()
    in
    iter (List.tl args);
    if !vdiskpath = "" then
        usage "No virtual disk specified!";
    print_version ();

    (*
    ** Try to get a valid vdisk host name
    *)
    let vdisk_dir = Filename.dirname !vdiskpath in
    let vdisk_dira = Array.of_list (Str.split (Str.regexp "/") vdisk_dir) in
    let vdisk_dirlen = Array.length vdisk_dira in

    if vdisk_dirlen > 0 then
    begin
        let name = vdisk_dira.(vdisk_dirlen-1) in
        bootdiskname := "bootdisk@"^name;
    end;

    if not !create then
    begin
        let disk = init !blocksize 
                         !vdiskpath 
                         !hostpath 
                         !bootdiskname
                         !verbose 
                         in
        info (sprintf "published bootdisk cap in %s/%s" !hostpath !bootdiskname);

        if !verbose then
        begin
            info (sprintf "%s cap: %s" !vdiskpath (ar_cap disk.bdisk_vcap));
            info (sprintf "DNS port: %s" (ar_port disk.bdisk_prvport)); 
            info (sprintf "AFS port: %s" (ar_port disk.bdisk_file_prvport)); 
            info "Boot directory rows:";
            List.iter (fun bde ->
                print_string (sprintf "%16s [obj=%s startblk=%d numblks=%d]"
                       bde.boot_name
                       (ar_priv bde.boot_obj)
                       bde.boot_start
                       bde.boot_size);
                print_newline ();
                ) disk.bdisk_table;
        end;
        start !nthr disk !verbose;        
        stop disk; 
        info "bye."
    end
    else
    begin
        let stat = Create.create !vdiskpath !overwrite in
        info (sprintf "finsihed: %s" (err_why stat));
    end;
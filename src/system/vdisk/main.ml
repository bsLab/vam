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
**    $INITIAL:     (C) 2004
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.04
**
**    $INFO:
**
**  Virtual Disk Server (UNIX version)
**
**    $ENDOFINFO
**
*)

let server_version = "1.03"

(*
** Main module of the VDISK server
*)

open Amoeba
open Bytebuf
open Stderr
open Stdcom
open Ar
open Sema
open Name
open Thread
open Disk_common
open Disk_server
open Disk_server_rpc
open Buf
open Unix
open Printf
open Server
open Cap_env

let _ = Db.set_level 10

let print_version () =
    print_string ("VDISK:  Virtual Disk Server: Version "^
                    (server_version));
    print_newline ();

    print_string  "        Copyright: BSSLAB, Stefan Bosse (2004)";
    print_newline ()

let hostname = ref (let hp = Unix.gethostname () in
                    (*
                    ** Maybe name is develop.bsslab.de, we need
                    ** only the first item.
                    *)
                    let hpdot = Str.split (Str.regexp "\.") hp in
                    List.hd hpdot)
                    
let geom = {dev_numcyl=0;
            dev_numhead=0;
            dev_numsect=0}

let devpath = ref ""
let hostpath = ref (sprintf "/hosts/%s" !hostname)
let nthr    = ref 4 
let blocksize = ref d_PHYSBLKSZ
let verbose = ref false 
let infomode = ref false 

let usage_str = "
vdisk [options] <device>
Program Arguments:
  <device>: The UNIX device name and path, for ex. /dev/da0
  -H <path>: Host path directory (Amoeba filesystem)
             Default: "^(!hostpath)^"
  -b : Blocksize [bytes]. Default: "^(sprintf "%d" !blocksize)^"
  -n : Number of service threads. Default: "^(sprintf "%d" !nthr)^"
  -chs #c,#h,#s : Number of cylinders, heads and sectors of the disk.
  -h : Print this help message
  -V : Print the server version
  -i : Print disklabel and partition informations of the disk
  -v : Verbose mode
"

let usage str =
    print_string (str^"\n"^usage_str);
    print_newline ();
    exit (-1)

let nl = print_newline 
let info str =
    print_string ("VDISK: "^str); nl ()

    
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
            | "-i" -> infomode := true; iter tl;
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
            | "-chs" ->
            begin
                match tl with
                | hd::tl -> 
                begin
                    let chs = hd in
                    let chsl = Str.split (Str.regexp ",") chs in
                    if (List.length chsl) <> 3 
                        then usage "Invalid CHS value";
                    geom.dev_numcyl <- int_of_string
                                        (List.nth chsl 0);
                    geom.dev_numhead <- int_of_string
                                        (List.nth chsl 1);
                    geom.dev_numsect <- int_of_string
                                        (List.nth chsl 2);
                    iter tl;
                end;
                | [] -> usage "Invalid -chs argument";

            end;
            | _ -> devpath := hd;
        end;
        | [] -> ()
    in
    iter (List.tl args);
    if !devpath = "" then
        usage "No device specified!";
    print_version ();

    let dev,disk_fd,disks = init !blocksize 
                                 !devpath 
                                 !hostpath 
                                 !verbose 
                                 geom in

    let vt = vdisk_table disks in

    if !infomode then
    begin
        print_string 
"===========================================================================";
        nl ();
        let str = print_vdisk_table vt in
        print_string str;
        print_string 
"===========================================================================";
        nl ();
        let str = info_disklabels dev in
        print_string str;                
        print_string 
"===========================================================================";
        nl ();
    end
    else
    begin
        start !blocksize !nthr !hostpath vt !verbose;        
        (*
        ** Remove published caps
        *)
        stop !hostpath vt;
    end;

    info "bye.";
    Unix.close disk_fd 

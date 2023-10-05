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
**    $INITIAL:     (C) 2004-2006 BSSLAB
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.11
**
**    $INFO:
**
**  VAM Amoeba Executioner: native Amoeba process and kernel
**      booting
**
**    $ENDOFINFO
**
*)



open Amoeba
open Unix
open Vax_io
open Stderr
open Name
open Vax
open Vax_common
open Cap_env

let version = "1.11"

let help = "
VAX: VAM Amoeba Executioner! Version "^version^"
     (C) 2004-2006 BSSLAB Dr. Stefan Bosse (www.bsslab.de)

usage: vax [-h -v -k] [-E NAME=PATH] [-S NAME=STR] [-vm <vmpath>] <host> <prog> <progargs>
    -h : show this help
    -k : Boot new kernel on host
    -d : load symbol table for debugging (if any)
    -E : add environment cap NAME lookuped from PATH
         NAME=% resolves the default cap. from the Amoeba host directory
         (only standard caps: TOD,RANDOM,TTY,PROC,...)
    -S : add string environment variable
    -v : verbose

    -vm: path to the target system vamrun machine
         Alternatively use the VAMRUN evironment variable to specify

    -vs: path to the target system vam toplevel system 
         Alternatively use the VAMSYS evironment variable to specify

    <host>: either absolute Amoeba host root path or host name, found in: 
            default host path: "^(!hostpath)^"
    <prog>: Local UNIX or Amoeba path for Amoeba native executable 
            file (or kernel)

    Path convention:
    /unix/... -> located on local UNIX filesystem
    else      -> located on AMoeba filesystem, specified with ROOTCAP

"

let usage err =
    out help; nl ();
    raise (Error err)

let prog = ref ""
let host = ref ""
let args = ref []
let capenv = ref []
let strenv = ref []
let inargs = ref false
let kernelboot = ref false 
let vamrun = ref ""
let vamsys = ref ""

let _ =
    try
    begin
        let caplist = ref [] in
        let comline = Array.to_list Sys.argv in
        let rec eval li =
            match li with
            | hd::tl ->
            begin
                match hd with
                | "-h" when !inargs = false -> usage std_OK;
                | "-v" when !inargs = false -> incr verbose; eval tl;
                | "-d" when !inargs = false -> debug := true; eval tl;
                | "-k" when !inargs = false -> kernelboot := true; eval tl;
                | "-E" when !inargs = false->
                begin
                    match tl with
                    | hd::tl ->
                    begin
                        caplist := !caplist @ [hd];
                        eval tl;
                    end;
                    | [] -> usage std_ARGBAD;
                end;
                | "-S" when !inargs = false->
                begin
                    match tl with
                    | hd::tl ->
                    begin
                        strenv := ! strenv @ [hd];
                        eval tl;
                    end;
                    | [] -> usage std_ARGBAD;
                end;
                | "-vm" when !inargs = false->
                begin
                    match tl with
                    | hd::tl ->
                    begin
                        vamrun := hd;
                        eval tl;
                    end;
                    | [] -> usage std_ARGBAD;
                end;
                | "-vs" when !inargs = false->
                begin
                    match tl with
                    | hd::tl ->
                    begin
                        vamsys := hd;
                        eval tl;
                    end;
                    | [] -> usage std_ARGBAD;
                end;
                | _ -> 
                begin
                    if (!host = "") then
                        host := hd
                    else if (!prog = "") then
                        prog := hd 
                    else 
                        args := !args @ [hd];
                    inargs := true;
                    eval tl;
                end;
            end;
            | [] -> ();
            in
        eval (List.tl comline);
        if (!host = "" || !prog = "") then
            usage std_ARGBAD;

        (*
        ** Lookup env. caps
        *)
        List.iter (fun namepath ->
                        (* TODO: UNIX cap path *)
                        let name,path = 
                                let l = Str.split (Str.regexp "=") 
                                                   namepath in
                                (List.nth l 0),(List.nth l 1)
                            in
                        
                        if (path.[0] <> '%') then
                        begin
                            let stat,cap = name_lookup path in
                            if (stat <> std_OK) then
                            begin
                                Printf.printf "Environment cap %s not found\n" 
                                        namepath;
                                raise (Error stat);
                            end;
                            capenv := ! capenv @ [name,cap];
                        end
                        else
                        begin
                            (*
                            ** Standard host capability
                            *)
                            let path = 
                                match name with
                                | "RANDOM" -> "random";
                                | "TOD" -> "tod";
                                | "PROC" -> "proc";
                                | "CMOS" -> "cmos";
                                | "SER:00" -> "ser:00";
                                | "SER:01" -> "ser:01"; 
                                | "SER:02" -> "ser:02";
                                | "SER:03" -> "ser:03"; 
                                | "PAR:00" -> "par:00";
                                | "PAR:01" -> "par:01"; 
                                | "PAR:02" -> "par:02";
                                | "PAR:03" -> "par:03"; 
                                | "SYS" -> "sys";
                                | "IPC" -> "ipc";
                                | "TTY" -> "tty:00";
                                | "TTY:00" -> "tty:00";
                                | "TTY:01" -> "tty:01";
                                | "TTY:02" -> "tty:02";
                                | "TTY:03" -> "tty:03";
                                | _ -> 
                                        out ("Invalid def. cap "^name);
                                        nl ();
                                        usage std_ARGBAD;
                                in
                            let path = if (!host.[0] = '/') then
                                        !host ^ "/" ^ path 
                                       else
                                        !hostpath ^ "/" ^ !host ^ "/" ^ path 
                                in
                                    
                            let stat,cap = name_lookup path in
                            if (stat <> std_OK) then
                            begin
                                Printf.printf "Environment cap %s not found\n" 
                                    namepath;
                                raise (Error stat);
                            end;
                            capenv := ! capenv @ [name,cap];
                        end;
            ) !caplist;

        let stat = if !kernelboot = false then
                    exec_start !host 
                               (path_resolve !vamsys)
                               (path_resolve !vamrun)
                               (path_resolve !prog)
                               !args 
                               !strenv 
                               !capenv 
                   else
                    boot_kernel !host !prog ([(Filename.basename !prog)]@
                                             !args) in
        if (stat <> std_OK) then
            raise (Error stat); 
    end
    with
        | Error err -> if err <> std_OK then 
                        out ("failed: "^(err_why err)); nl();

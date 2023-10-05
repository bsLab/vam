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
**    $VERSION:     1.09
**
**    $INFO:
**
**
**
**    $ENDOFINFO
**
*)


open Amoeba
open Unix
open Vax_io
open Stderr
open Vax_tty
open Vax_exec
open Thread
open Name
open Ksys
open Cap_env
open Printf
open Afs_common
open Afu_server

open Vax_common

let __ = ignore

(*
** Determine file type of programm executable file
*)
let exec_file prog =
    let loc =
        match prog with
        | Unix_path prog -> Unix_loc;
        | Amoeba_path prog ->  Amoeba_loc;
        in
    let filename = 
        match prog with
        | Unix_path prog -> prog;
        | Amoeba_path prog ->  prog;
        in
    match loc with
    | Unix_loc ->
    begin
        try
        begin
            let fd = Unix.openfile filename [O_RDONLY] 0 in
            let str = String.create 100 in
            let n = Unix.read fd str 0 100 in
            Unix.close fd;
            match (str.[0],str.[1]) with
            | ('#','!') ->
            begin
                (*
                ** Examine string untill newline was found
                *)
                let pos = ref 0 in
                while (!pos < 100 && str.[!pos] != '\n') 
                do
                    incr pos;
                done;
                if !pos = 100 then
                    std_OBJBAD,Binary,loc,filename
                else
                begin
                    let str' = String.sub str 0 !pos in
                    let basename = Filename.basename str' in
                    match basename with
                    | "vamrun"  -> std_OK,Bytecode,loc,filename;
                    | "vam"     -> std_OK,Script,loc,filename;
                    | "vamsys"  -> std_OK,Script,loc,filename;
                    | _ -> std_OBJBAD,Binary,loc,filename;
                end;
            end;
            | _ -> std_OK,Binary,loc,filename
        end
        with | _ -> std_IOERR,Binary,loc,filename
    end;
    | Amoeba_loc -> std_OK,Binary,loc,filename
    

(*
** Execute a program on the specified target host. In the case, the binary
** is loacted on a UNIX host, a simple AFS interface to this file must be 
** provided.
** Different kinds of programs are supported:
**  1. Native binaries
**  2. Bytecode programs (needs vamrun path to be specified)
**  3. ML scripts
*)

let exec_start host vamsys vamrun prog args strenv capenv =
  try
  begin
    (*
    ** The capability of the file to be executed - not necessary
    ** the prog argument! (vamrun instead for example).
    *)

    let execfile = ref nilcap in

    let args = ref args in
    let cleanup = ref [] in

    let stat,kind,src_loc,prog = exec_file prog in
    if stat <> std_OK then
    begin
        out ("invalid program source "^prog^"\n");
        raise (Error stat);
    end;

    if !verbose > 0 then
    begin
        out (
        match kind with
        | Binary -> "native Binary file";
        | Bytecode -> "Bytecode file";
        | Script -> "VAM-ML script file";);
        out "\n";
    end;

    (
        match src_loc with
        | Unix_loc ->
        begin
            (*
            ** Under UNIX, we must emulate the AFS interface for the
            ** UNIX program file!
            *)
            let afu_server = afu_init afs_REQBUFSZ max_files in
            afu_server.afu_verbose <- if !verbose > 2 then 10 else 0;
            (*
            ** Open executable file 
            *)
            if (!verbose > 2) then
            begin
                out "Opening UNIX program file..."; nl();
            end;

            (
                match kind with
                | Binary ->  
                    let stat,new_afu = afu_open_file afu_server prog in
                    cleanup := !cleanup @ [
                                 (fun () -> 
                                    afu_close_file afu_server 
                                                   new_afu)];
                    execfile := afu_file_cap afu_server new_afu;
                | Bytecode -> 
                begin
                    (*
                    ** We have perhaps two files to handle here:
                    ** 1. The vamrun binary (located on UNIX).
                    ** 2. The bytecode file
                    *)

                    let vampath = ref (
                        match vamrun with
                        | Unix_path p   -> p;
                        | Amoeba_path "/" -> "";
                        | Amoeba_path path -> 
                        out 
                        ("Bytecode file and vamrun must"^
                         " be on same filesystem: "^path^"\n");
                        raise (Error std_ARGBAD);) in

                    if !vampath = "" then
                    begin
                        (*
                        ** Try to resolve environment variable VAMRUN
                        *)
                        vampath := (
                        try 
                         (match (path_resolve (Sys.getenv "VAMRUN")) with
                          | Unix_path p -> p;
                          | Amoeba_path "" -> raise Not_found;
                          | Amoeba_path _ -> 
                          out "Bytecode file and vamrun must be on same filesystem\n";
                          raise (Error std_ARGBAD)
                         );
                         with Not_found ->
                            out "no path to vamrun specified\n";
                            raise (Error std_ARGBAD));
                    end;

                    let stat,vam_afu = afu_open_file afu_server !vampath in
                    let stat,prog_afu = afu_open_file afu_server prog in
                    (*
                    ** Publish file cap in a temporary Amoeba directory
                    *)
                    let tmpfile =  
                        sprintf "/tmp/%s_%d.vax"
                            (Filename.basename prog)                   
                            (Random.bits ()) in
                    __(afu_publ_file afu_server prog_afu tmpfile);
                    args := ["-b";tmpfile] @ !args;
 
                    cleanup := !cleanup @ 
                                [(fun () -> __(afu_unpubl_file afu_server
                                                               tmpfile));
                                 (fun () -> afu_close_file afu_server vam_afu);
                                 (fun () -> afu_close_file afu_server prog_afu)];
                    execfile := afu_file_cap afu_server vam_afu;
                end;
                | Script -> failwith "TODO";
            );
            (*
            ** Start AFS service thread
            *) 
            if (!verbose > 2) then
            begin
                out "Starting AFS server..."; nl();
            end;

            afu_server.afu_verbose <- !verbose;

            for i = 1 to 2
            do
              ignore (thread_create 
                        (fun () -> afu_server_loop afu_server) ());
            done;
        end;
        | Amoeba_loc ->
        begin
            (
                match kind with
                | Binary ->  
                    let stat,fcap = name_lookup prog in
                    if stat <> std_OK then
                    begin
                        out "Can't lookup file capability"; nl();
                        raise (Error stat);
                    end;
                    execfile := fcap;
                | Bytecode -> 
                begin
                    failwith "TODO";
                end;
                | Script -> failwith "TODO";
            );
        end;
    );

    (*
    ** Start TTY service thread
    *)
    if (!verbose > 2) then
    begin
        out "Starting TTY server..."; nl();
    end;

    tty_start ();

    if (!verbose > 2) then
    begin
        out "Looking up proc server..."; nl();
    end;

    let stat,procsrv =
        if (host.[0] = '/') then
        begin
            if ((Filename.basename host) = "proc") then
                name_lookup host
            else
                name_lookup (host^"/proc")    
        end
        else
            name_lookup (!hostpath^"/"^host^"/proc")
        in

    if (stat <> std_OK) then
    begin
        List.iter (fun f -> f ()) !cleanup;
        out ("Can't lookup process server "^(host)); nl();
        raise (Error stat);
    end;

    (*
    ** Put some more standard capabilities in front of capenv list.
    ** The can be overriden by the current content of capenv.
    *)
    let fullpath =
        if (host.[0] = '/') 
            then host
            else (!hostpath^"/"^host)
        in

    let evs = [
            "TOD","tod";
            "RANDOM","random"; ] in
    let add = ref [] in

    List.iter (fun (name2,name) ->
        let stat,cap = name_lookup (fullpath^"/"^name) in
        if stat <> std_OK then
        begin
            out (sprintf "Warning: can't lookup stdcap %s:%s" 
                    (fullpath^"/"^name)
                    (err_why stat)); nl();
        end
        else
        begin
            add := !add @ [name2,cap];
        end;
      ) evs;

    let stat = ax procsrv !execfile
                  ([Filename.basename prog] @ !args) 
                   strenv
                   (!add@capenv)
        in

    List.iter (fun f -> f ()) !cleanup;
    tty_exit ();
    stat
  end
  with
    | Error stat -> stat;
    | Failure str -> out str; nl (); std_IOERR;
    | _ -> out "Unknown Failure"; nl(); std_SYSERR

(*
** Boot a new kernel on host
*)
let boot_kernel host kernel args =
  try
  begin
    let afu_server = afu_init afs_REQBUFSZ 4 in

    (*
    ** Open kernel file (UNIX FS)
    *)
    if (!verbose > 0) then
    begin
        out "Opening UNIX file..."; nl();
    end;

    let stat,afu_kernel = afu_open_file afu_server kernel in

    (*
    ** Start AFS service thread
    *)
    if (!verbose > 0) then
    begin
        out "Starting AFS-UNIX map server..."; nl();
    end;

    for i = 1 to 2
    do
      ignore (thread_create (fun () -> afu_server_loop afu_server) ());
    done;

    if (!verbose > 0) then
    begin
        out "Looking up system server..."; nl();
    end;

    let stat,syssrv =
        if (host.[0] = '/') then
        begin
            if ((Filename.basename host) = "sys") then
                name_lookup host
            else
                name_lookup (host^"/sys")    
        end
        else
            name_lookup (!hostpath^"/"^host^"/sys")
        in

    if (stat <> std_OK) then
    begin
        afu_close_all_files afu_server;
        out ("Can't lookup system server "^(host)); nl();
        raise (Error stat);
    end;

    let argstr = 
        let str = ref "" in 
        List.iter ( fun a ->
            str := !str ^ a ^ " ";
            )args;
        !str
        in

    out ("VAX: booting new kernel <" ^ kernel ^ "> [" ^ argstr ^ 
         "] on host " ^ host); nl();
    let kcap = {cap_port = afu_server.afu_putport;
                cap_priv = prv_encode (Objnum afu_kernel.afu_obj)
                                      prv_all_rights
                                      afu_kernel.afu_rand} in

    let stat = sys_boot ~syscap:syssrv 
                        ~kernelcap:kcap
                        ~commandline:argstr
                        ~flags:0 
        in
    out ("Status: "^(err_why stat)); nl();
    afu_close_all_files afu_server;
    stat
  end
  with
    | Error stat -> stat;
    | Failure str -> out str; nl (); std_IOERR;
    | _ -> out "Unknown Failure"; nl(); std_SYSERR

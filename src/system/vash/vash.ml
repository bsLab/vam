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
**    $INITIAL:     (C) 2003-2005 BSSLAB
**    $CREATED:     
**    $VERSION:     1.02
**
**    $INFO:
**
** The VAM shell. The Unix-Amoeba integrator.
**
**
**    $ENDOFINFO
**
*)





let server_version = "1.16"

open Amoeba
open Stdcom
open Stderr
open Stdcom2

open Vash_io
open Vash_env
open Cap_env

external terminal_readline: string -> string = "terminal_readline"

let print_version () =
    out ("     VASH - The VAMNET-Amoeba-Shell Ver. "^(server_version));
    nl ();
    out  "            (C) BSSLAB, Stefan Bosse (2003-2006)"; nl ();
    out  "            Use the 'help' command for more informations."; 
    nl ()



(*
** The following builtin commands are supported:
*)

type vash_com = 
    | Noop of string list
    | Help of string list
    | Exit_shell
    (*
    ** Standard commands
    *)
    | Std_info of string list         (* Object Info request              *) 
    | Std_status of string list       (* Status request                   *)
    | Std_params of string list       (* Server parameter request         *)
    | Std_exit of string list         (* Server exit request              *)
    | Std_destroy of string list      (* Object destroy request           *)
    | Std_age of string list          (* Server age request               *)
    | Std_touch of string list        (* Object touch live time request   *)
    | Std_exec of string list         (* Standard exec request            *)

    (*
    ** DNS interface
    *)

    | Dir of string list              (* Directory listing                *)
    | Print of string list            (* Print the content of a file      *)
    | Mkdir of string list
    | Del of string list
    | Cd of string list
    | Pwd of string list
    | C2a of string list
    | A2c of string list    
    | Mkhost of string list
    | Mv of string list

    (*
    ** AFS interface
    *)

    | Cp of string list               

    
    (*
    ** VASH environment (capabilities)
    *)
    | Set_env of string list
    | Get_env of string list
    | Print_env of string list 

    (*
    ** Exececution of VAM-ML scripts and programs
    *)
    | Exec of string list
    | Ax of string list
    | Stun of string list

    (*
    ** Systems services
    *)
    | Reboot of string list
    | Kmesg of string list
    | Kstat of string list
    | Installk of string list

    (*
    ** Buffer Stream Interface
    *)
    | Stream_read of string list
    | Stream_write of string list

let to_command com args =
    match com with
    | "std_info" -> Std_info args;
    | "std_status" -> Std_status args;
    | "std_params" -> Std_params args;
    | "std_exit" -> Std_exit args;
    | "std_destroy" -> Std_destroy args;
    | "std_age" -> Std_age args;
    | "std_touch" -> Std_touch args;
    | "std_exec" -> Std_exec args;

    | "dir" | "ls" -> Dir args;
    | "mkdir" | "mkd" -> Mkdir args;
    | "print" | "prn" | "cat" -> Print args;
    | "rm" | "del" -> Del args;
    | "cd" -> Cd args;
    | "pwd" -> Pwd args;
    | "c2a" -> C2a args;
    | "a2c" -> A2c args;
    | "mkhost" | "kernelcap" -> Mkhost args;
    | "mv" -> Mv args;

    | "cp" -> Cp args;

    | "set_env" | "set" | "put" -> Set_env args;
    | "get_env" | "get" -> Get_env args;
    | "print_env" -> Print_env args;

    | "exec" | "use" -> Exec args;
    | "ax" -> Ax args;
    | "stun" -> Stun args;

    | "reboot" -> Reboot args;
    | "printbuf" | "kmesg" -> Kmesg args;
    | "kstat" -> Kstat args;
    | "installk" -> Installk args;

    | "stream_read" -> Stream_read args;
    | "stream_write" -> Stream_write args;

    | "help" -> Help args;
    | "exit" -> Exit_shell;
    | _ -> raise (Error std_COMBAD)

let help  = "
======================= Builtin commands ========================
Standard requests:
std_info, std_status, std_params, std_exit, std_destroy, std_age,
std_touch, std_exec
-----------------------------------------------------------------
Directory and file requests:
dir, ls, mkdir, mkd, print, prn, cat, cp, mv, rm, del, cd,
pwd, c2a, a2c, mkhost, kernelcap
-----------------------------------------------------------------
Environment variable management:
set_env, set, put, get_env, get, print_env
-----------------------------------------------------------------
Script and program execution:
exec, use, ax, stun
-----------------------------------------------------------------
System requests:
reboot, kstat, kmesg, printbuf, installk
-----------------------------------------------------------------
-----------------------------------------------------------------
Buffer stream requests:
stream_read, stream_write
-----------------------------------------------------------------
Misc.:
help, exit
"

(*
** Parse the command line. Either blank spaces are seperators for
** command line arguments, or quotes "" and '' enclose arguments.
** Returns the list of arguments and the command.
*)

let parse_command_line line =
    try
    begin
        let args = ref [] in
        let line_len = String.length line in
        let curarg = ref "" in
        let inquotes = ref 0 in (* 1='' 2="" *)

        let add_arg () =
            if (!curarg <> "") then
                args := !args @ [!curarg];
            curarg := "";
        in

        for i = 0 to line_len-1
        do
            match line.[i] with
            | ' ' when !inquotes = 0 -> 
            begin
                add_arg ();
                curarg := "";
            end;
            | '\'' -> 
            begin
                if (!inquotes = 0) then
                begin
                    inquotes := 1;
                    add_arg ();
                end
                else if (!inquotes = 1) then
                begin 
                    add_arg ();
                    inquotes := 0;
                end
                else
                    raise (Error std_ARGBAD);
            end;
            | '"' -> 
            begin
                if (!inquotes = 0) then
                begin
                    inquotes := 2;
                    add_arg ();
                end
                else if (!inquotes = 2) then
                begin 
                    add_arg ();
                    inquotes := 0;
                end
                else
                    raise (Error std_ARGBAD);
            end;
            | _ -> curarg := !curarg ^ (String.sub line i 1); 
        done;
        add_arg ();
        match !args with
        | hd::tl ->
        begin
            std_OK,(to_command hd tl)
        end;
        | [] -> raise (Error std_ARGBAD);
    end
    with
        | Error err -> err,(Noop [])

(*
** Execute a command with arguments
*)


let exec_com com =
  try
  begin
    match com with

    | Help args ->
    begin
        out help; nl ();
        std_OK
    end;

    | Print_env args -> Vash_env.print_env args;
    | Set_env args -> Vash_env.put_env args;
    | Get_env args -> Vash_env.get_env args;

    | Std_info args -> Vash_std.info args;
    | Std_status args -> Vash_std.status args;
    | Std_exit args -> Vash_std.exit args;
    | Std_exec args -> Vash_std.exec args;
    | Std_params args -> Vash_std.params args;    
    | Std_touch args -> Vash_std.touch args;

    | Dir args -> Vash_dir.dir args;
    | Cd args -> Vash_dir.cd args;
    | Mkdir args -> Vash_dir.mkdir args;
    | Del args -> Vash_dir.del args;
    | A2c args -> Vash_dir.a2c args;

    | Cp args -> Vash_file.cp args;

    | Ax args -> Vash_exec.ax args;
    | Stun args -> Vash_exec.stun args;

    | Mkhost args -> Vash_ksys.mkhost args;

    | Kmesg args -> Vash_ksys.kmesg args;
    | Reboot args -> Vash_ksys.reboot args;
    | Kstat args -> Vash_ksys.kstat args;    
    | Installk args -> Vash_ksys.installk args;

    | C2a args -> Vash_dir.c2a args;
    | Stream_read args -> Vash_stream.stream_read args;
    | Stream_write args -> Vash_stream.stream_write args;

    | Exit_shell -> raise Exit;

    | _ -> std_COMBAD
  end
  with
    | Error err -> err

let eval line =
    try
    begin
        let stat,com = parse_command_line line in
        if (stat <> std_OK) then
            raise (Error stat); 
        let stat = exec_com com in
        if (stat <> std_OK) then
            raise (Error stat); 
    end
    with
        | Error err -> 
        begin
            out ("Eval failed: "^(err_why err)); nl ();
        end;
        | Failure str -> 
        begin
            out ("Eval failed: "^str); nl ();
        end;
        | Exit -> raise Exit;
        | _ ->
        begin
            out ("Eval failed: unknown error"); nl ();
        end

        

let enter () =
    print_version ();
    nl ();
    try
    begin
        while (true)
        do
            let path = match !work_dir with
                       | Unix_path p -> "/unix"^p;
                       | Amoeba_path p -> p
                in
            out ("["^path^"] "); nl();
            let input = terminal_readline ">> " in
            eval input;    
        done;
        std_OK
    end
    with
        | Exit -> std_OK
        | _ -> std_IOERR


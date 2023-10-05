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
**    $INITIAL:     (C) BSSLAB 2003-2006
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.07
**
**    $INFO:
**
**  Build process environment and execute Amoeba binary.
**
**    $ENDOFINFO
**
*)



open Amoeba
open Stderr
open Buf
open Bytebuf
open Name
open Proc
open Ar
open Rpc

open Cap_env
open Vax_io
open Vax_tty
open Signals 

let terminating = ref 0

let build_capenv capenv =
    ignore(put_env_cap "TTY" !tty_cap);
    ignore(put_env_cap "STDIN" !tty_cap);
    ignore(put_env_cap "STDOUT" !tty_cap);
    ignore(put_env_cap "STDERR" !tty_cap);
    List.iter (fun nc -> 
                let n,c = nc in 
                ignore(put_env_cap n c);
                (*
                ** Special case: TTY -> must be also applied to
                ** STDOUT/STDERR/STDIN
                *)
                match n with
                | "TTY" ->
                    let n' = ["STDOUT";"STDIN";"STDERR"] in
                    List.iter (fun n -> ignore(put_env_cap n c)) n';
                |  _ -> ();
        ) capenv; 
    get_env_caps ()


let ax procsrv filecap args strenv capenv =

    try
    begin
        let p_args = ref args in
        let p_strenv = ref strenv in
        let p_capenv = ref (build_capenv capenv) in
        if (!verbose > 0) then
        begin
            out "Process argument list: [ "; nl ();
            List.iter (fun a -> out (a^"\n")) !p_args; out "]"; nl ();
            out "Process string environment list: [ "; nl ();
            List.iter (fun a -> out (a^"\n")) !p_strenv; out "]"; nl ();
            out "Process capability environment list: [ "; nl ();
            List.iter (fun a -> 
                    let n,c = a in 
                    out (n^"="^(ar_cap c)^"\n")) !p_capenv;
            out "]"; nl();
            out ("File capability: "^(ar_cap filecap)); nl (); 
        end;


        (*
        ** Read execfile descriptor.
        *)

        if (!verbose > 0) then out "Reading PD...\n";

        let stat,pd = pd_read filecap in 
        if (stat <> std_OK) then
        begin
            out ("Reading process descriptor failed."); nl();
            raise (Error stat);
        end;



        let owner_getport = uniqport () in
        let owner_putport = priv2pub owner_getport in
        let owner = {cap_port = owner_putport; cap_priv = nilpriv} in

        if (!verbose > 0) then out "Executing PD...\n";

        let stat,proccap =
                exec_pd ~pd:pd
                        ~host:procsrv
                        ~owner:owner
                        ~stacksize:0
                        ~args:!p_args
                        ~strenv:!p_strenv
                        ~capenv:!p_capenv
                        ~opt:(if !debug then [P_loadsym] else [])
        in

        if (!verbose > 0) then out "PD Executed.\n";

        let symtab = pd.pd_symtab in
        if (stat != std_OK) then
        begin
            out (Printf.sprintf "exec_pd failed: %s" (err_why stat)); nl();
            raise (Error stat);
        end;
        if (!verbose > 0) then
        begin
            out ("Process capability: "^(ar_cap proccap)); nl ();
        end;

        let command_line = ref "" in
        List.iter (fun a -> command_line := !command_line ^ " " ^ a) args;
        pro_setcomment proccap !command_line;
        
        (*
        ** Install signal handler:
        ** Catch sig_INT for terminating the am process
        *)
        sig_catch sig_INT (fun i ->
                if !terminating = 0 then
                begin
                    incr terminating;
                    Printf.printf "Terminating am process...";
                    print_newline ();
                    let stat=pro_stun ~cap:proccap ~signal:15 in
                    Printf.printf "Status: %s" (err_why stat);
                    print_newline ();
                end
                else if !terminating = 1 then
                begin
                    incr terminating;
                    Printf.printf "Stunning am process with signal -2 ...";
                    print_newline ();
                    let stat=pro_stun ~cap:proccap ~signal:(-2) in
                    Printf.printf "Status: %s" (err_why stat);
                    print_newline ();
                end
                else
                begin 
                    Printf.printf "User Abort.";
                    print_newline ();
                    exit 0 
                end;
                
            );
        

        (*
        ** Wait for process termination - we get a request...
        ** On abnormal termination(exception), extract the 
        ** returned process descriptor and print all informations
        ** suitable for the user.
        *)
        let pdbufsize = 30000 in
        let pdbuf = buf_create pdbufsize in
        let stat,n,hdr_req = getreq(owner_getport,pdbuf,pdbufsize) in

        tty_restore ();

        let sign = hdr_req.h_offset in
        let signal_name =
            "\""^
            (match sign with
            | (-2) -> "Illegal instruction";
            | (-3) -> "Mis-aligned reference";
            | (-4) -> "Non-existent memory";
            | (-5) -> "Breakpoint instruction";
            | (-6) -> "Undefined instruction";
            | (-7) -> "Division by zero";
            | (-8) -> "Floating Point exception";
            | (-9) -> "Memory access control violation";
            | (-10) -> "Bad system call";
            | (-11) -> "Illegal operand (to instruction)";
            | (-12) -> "System call emulation";
            | (-13) -> "abort() called";
            | _ -> "unknown or user signal";
            )^"\" ("^(string_of_int sign)^")"
            in

        let state,normal = match hdr_req.h_extra with
                    | stat when (stat = term_NORMAL) -> 
                        "Process terminated normal.",true;
                    | stat when (stat = term_STUNNED) ->
                        "Process was stunned with signal "^
                        signal_name^".",false;
                    | stat when (stat = term_EXCEPTION) ->
                        "Process got uncaught exception with signal "^
                        signal_name^".",false;
                    | stat ->
                        "Process terminates with unknown status "^
                        (string_of_int stat)^"!",false;
            in
        out state; nl ();
        if (normal = false) then
        begin
            let _,pd = buf_get_pd ~buf:pdbuf ~pos:0 in
            pd.pd_symtab <- symtab;
            print_pd pd; nl ();
        end;        
        ignore(putrep(hdr_req,nilbuf,0));
        stat
    end
    with
        | Buf_overflow -> std_OVERFLOW;
        | Error err -> err



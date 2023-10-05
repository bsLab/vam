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
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     0.97
**
**    $INFO:
**
**
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
open Vash_io
open Vash_env



let last_tty = ref nilcap

(*
** Build an initial process capability environment
*)

let build_capenv () =
    let capenv = ref [] in
    tty_cap := (let stat,cap = get_env_cap "TTY" in 
                        if (stat <> std_OK) then
                        begin
                            out "Warning: no TTY environment!"; nl ();
                        end; cap);
    if (!tty_cap <> !last_tty) then
    begin
        stdin_cap := !tty_cap;            
        ignore(put_env_cap "STDIN" !stdin_cap);
        stdout_cap := !tty_cap;            
        ignore(put_env_cap "STDOUT" !stdout_cap);
        stderr_cap := !tty_cap;
        ignore(put_env_cap "STDERR" !stderr_cap);
        last_tty := !tty_cap;
    end;
    get_env_caps ()


let ax args =
    let p_args = ref [] in
    let p_strenv = ref [] in
    let p_capenv = ref [] in
    let hostpath = ref "/hosts" in
    let tmphost = ref "" in
    let filepath = ref "" in
    let verbose = ref false in
    let inargs = ref false in

    try
    begin
        let usage err =
            out  "usage: ax [-h -v -E <earg>] <hostpath> <progpath> [-<progargs>,..]"; 
            nl ();
            out ("       default host path:"^(!hostpath)); nl ();
            out ("       -E: string environment variable"); nl ();
            out ("       -v: print additional informations"); nl ();
            raise (Error err);
            in

        let rec iter args =
            match args with
            | hd::tl -> 
            begin
                match hd with
                | "-h" when !inargs = false -> usage std_OK;
                | "-v" when !inargs = false -> verbose := true; iter tl;
                | "-C" when !inargs = false -> 
                begin
                    match tl with 
                    | hd::tl -> p_strenv := !p_strenv @ [hd]; iter tl;
                    | [] -> out "argument missing"; nl (); usage std_ARGBAD;
                end;
                | _ -> 
                begin
                    if hd.[0] = '-' && !inargs = false then
                    begin
                        out ("unknown option: "^hd); nl ();
                        usage std_ARGBAD; 
                    end;
                    if (!tmphost = "") then
                        tmphost := hd
                    else if (!filepath = "") then
                        filepath := hd
                    else 
                        p_args := !p_args @ [hd];
                    inargs := true;
                    iter tl;
                end;
            end;
            | [] -> ()
        in
        iter args;
        if (!filepath = "" ||
            !tmphost = "") then
            usage std_ARGBAD;

        if (!tmphost.[0] <> '/') then
            hostpath := !hostpath ^ "/" ^ !tmphost
        else
            hostpath := !tmphost;


        p_args := [Filename.basename !filepath] @ !p_args;
        p_capenv := build_capenv ();
        if (!verbose) then
        begin
            out "Process argument list: [ "; nl ();
            List.iter (fun a -> out (a^" ")) !p_args; out "]"; nl ();
            out "Process string environment list: [ "; nl ();
            List.iter (fun a -> out (a^" ")) !p_strenv; out "]"; nl ();
            out "Process capability environment list: [ "; nl ();
            List.iter (fun a -> 
                    let n,c = a in 
                    out (n^"="^(ar_cap c)^" ")) !p_capenv;
            out "]"; nl();
        end;
        let stat,procsrv =
            if ((Filename.basename !hostpath) = "proc") then
                name_lookup !hostpath
            else
                name_lookup (!hostpath^"/proc")    
            in
        if (stat <> std_OK) then
        begin
            out ("Can't lookup process server "^(!hostpath)); nl();
            raise (Error stat);
        end;
        let stat,fcap = name_lookup !filepath in
        if (stat <> std_OK) then
        begin
            out ("Can't lookup process file "^(!filepath)); nl();
            raise (Error stat);
        end;
        let stat,pd = pd_read fcap in 
        if (stat <> std_OK) then
        begin
            out ("Reading process descriptor failed."); nl();
            raise (Error stat);
        end;

        let owner_getport = uniqport () in
        let owner_putport = priv2pub owner_getport in
        let owner = {cap_port = owner_putport; cap_priv = nilpriv} in
        let stat,proccap =
                exec_pd ~pd:pd
                        ~host:procsrv
                        ~owner:owner
                        ~stacksize:0
                        ~args:!p_args
                        ~strenv:!p_strenv
                        ~capenv:!p_capenv
                        ~opt: []
        in
        if (!verbose) then
        begin
            out ("Process capability: "^(ar_cap proccap)); nl ();
        end;

        (*
        ** Wait for process termination - we get a request...
        ** On abnormal termination(exception), extract the 
        ** returned process descriptor and print all informations
        ** suitable for the user.
        *)
        let pdbufsize = 30000 in
        let pdbuf = buf_create pdbufsize in
        let stat,n,hdr_req = getreq(owner_getport,pdbuf,pdbufsize) in
        let sign = hdr_req.h_offset in
        let state,normal = match hdr_req.h_extra with
                    | stat when (stat = term_NORMAL) -> 
                        "Process terminated normal.",true;
                    | stat when (stat = term_STUNNED) ->
                        "Process was stunned with signal "^
                        (string_of_int sign)^".",false;
                    | stat when (stat = term_EXCEPTION) ->
                        "Process got uncaught exception with signal "^
                        (string_of_int sign)^".",false;
                    | stat ->
                        "Process terminates with unknown status "^
                        (string_of_int stat)^"!",false;
            in
        out state; nl ();
        if (normal = false) then
        begin
            let _,pd = buf_get_pd ~buf:pdbuf ~pos:0 in
            print_pd pd; nl ();
        end;        
        ignore(putrep(hdr_req,nilbuf,0));
        stat
    end
    with
        | Buf_overflow -> std_OVERFLOW;
        | Error err -> err


(*
** Send a process a signal.
**
** For example: 15 -> terminate
**
*)
let stun args =
    let hostpath = ref "/hosts" in
    let tmphost = ref "" in
    let procnum = ref "" in
    let signum = ref "" in
    let verbose = ref false in

    try
    begin
        let usage err =
            out  "usage: stun [-h] <hostpath or hostname> <process #> <signal #>"; 
            nl ();
            out ("       Default host path: /hosts"); nl ();
            raise (Error err);
            in

        let rec iter args =
            match args with
            | hd::tl -> 
            begin
                match hd with
                | "-h" -> usage std_OK;
                | _ -> 
                begin
                    if hd.[0] = '-' then
                    begin
                        out ("unknown option: "^hd); nl ();
                        usage std_ARGBAD; 
                    end;
                    if (!tmphost = "") then
                        tmphost := hd
                    else if (!procnum = "") then
                        procnum := hd
                    else if (!signum = "") then
                        signum := hd
                    else
                        usage std_ARGBAD;
                    iter tl;
                end;
            end;
            | [] -> ()
        in
        iter args;
        if (!tmphost = "" ||
            !procnum = "" ||
            !signum = "") then
            usage std_ARGBAD;

        if !tmphost.[0] = '/' then
            hostpath := !tmphost 
        else
            hostpath := !hostpath ^ "/" ^ !tmphost;

        let stat,proccap = name_lookup 
                    (!hostpath ^ "/ps/" ^ !procnum) in

        if (stat <> std_OK) then
        begin
            out ("Can't lookup process on host "^(!hostpath)); nl();
            raise (Error stat);
        end;

        let stat = pro_stun proccap (int_of_string !signum) in
        stat        
    end
    with
        | Buf_overflow -> std_OVERFLOW;
        | Error err -> err



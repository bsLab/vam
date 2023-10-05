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
**    $INITIAL:     (C) 2004-2005
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.15
**
**    $INFO:
**
** VAM boot server implementation
**	
**
**    $ENDOFINFO
**
*)


open Amoeba
open Unix
open Thread
open Sema
open Printf
open Cap_env
open Stderr
open Thread
open Ar
open Dir
open Stdcom
open Rpc 
open Proc
open Buf
open Bytebuf 
open Name
open Capset
open Syslog

open Afs_common
open Afs_client
open Dns_client
open Afu_server

let out = print_string
let nl = print_newline

let stats = ref [||] 


(*
** Boot object capabilty:
**  1. looked up from a path (1)
**  2. a given capability
**  3. a given capability in ASCII representation format
** 
*)

type boot_objcap = 
    | Boot_path of string       (* the object filesystem path (1)(2)        *)
    | Boot_cap  of capability   (* a capability specifies an amoeba object  *)
    | Boot_capstr of string     (* a capability in string format:        
                                ** XX:XX:XX:XX:XX:XX/n(r)/YY:YY:YY:YY:YY:YY
                                *)
    | Boot_nil

(*
** Footnotes: 
**
** (1) Either Amoeba filesystem (specified with environment cap ROOT) or
**     local Unix filesystem (Unix_path: string prefixed with '/unix') 
**
** (2) Path to a Unix file holding a capability in binary format 
**     (Type Unix_path only)
*)

let nilpath = Boot_path ""

(*
** Source file loaction
*)

type boot_src =
    | Unix_src of boot_objcap      (* Unix local file              *)
    | Amoeba_src of boot_objcap    (* Amoeba file                  *)
    | Fun_src of (unit -> status)  (* ML function to be executed   *)
    | Nil_src

(*
** Destination description, for example the boot server.
*)

type boot_dst =
    | Unix_dst                  (* executes on Unix ...             *)
    | Amoeba_dst of boot_objcap (* amoeba process server            *)
    | Nil_dst

(*
** Status of a boot server object
*)

type boot_stat =
    | Boot_cold         (* nothing done untill now      *)
    | Boot_starting     (* The boot object starts...    *) 
    | Boot_killing      (* Shutdown the boot object ... *)
    | Boot_up           (* Polling says: the boot object is alive *)
    | Boot_down         (* Polling says: the boot object is not rechable *)
    | Boot_executed     (* Without polling: boot object started. *)
    | Boot_restarting   (* After Boot_down: boot object restarts *)
    | Boot_unknown      (* Unknown boot object state *)



(*
** Boot object operations
*)
type boot_op =
    | Boot_poll of (boot_objcap * int)  (* poll path/cap and time interval  *)
    | Boot_start                        (* start the service - of course    *)
    | Boot_stop                         (* stop the service                 *)
    | Boot_restart                      (* restart if poll failed           *)
    | Boot_coldstart                    (* start without polling            *)

(*
** Boot object environment 
**
** Note: If the ROOT capability is missing in the environment list,
** the root capability is inherited by the boot server. Also the
** standard channel capabilities STDOUT, STDIN, STDERR, and TOD
** and random, all members of the standard environment.
** All Amoeba path lookups are relative to the specified ROOT capability.
*)

type boot_env =
    (*
    ** Capability environment
    *)
    | Env_cap of (string * capability)      (* Capability                   *)
    | Env_cappath of (string * string)      (* Cap. looked up from path     *)
    | Env_capstr of (string * string)       (* Cap. in ASCII representation *)

    (*
    ** String environment
    *)
    | Env_str of (string * string)         
    
    (*
    ** Internal
    *)
    | Env_self of string                    (* handled by the boot srv    *)

(*
** One boot object: Part I public
*)

type boot_def = {
            boot_name : string;             (* name of the boot object  *)
            boot_src  : boot_src;           (* the boot object source   *)
            boot_dst  : boot_dst;           (* the boot object dst.     *)
            boot_args : string list;        (* program arguments        *)
    mutable boot_env  : boot_env list;      (* program cap and str env. *)
            boot_ops : boot_op list;        (* boot obj. operations     *)
            boot_deps : string list;        (* boot obj. dependencies   *)
}

let nilcap' = ar_cap nilcap

let nildef = { boot_name = "";
               boot_src = Nil_src;
               boot_dst = Nil_dst; 
               boot_args = []; 
               boot_env = []; 
               boot_ops = []; 
               boot_deps = [] }
(*
** Part II private
*)

(*
** Different types of boot objects (modes) are supported.
*)

type boot_type =
    | Unix_type          (* start an Unix binary     *)
    | Amoeba_type        (* start an Amoeba binary  *)
    | Fun_type           (* execute ML function     *)
    | Nil_type 

(*
** Standard capability environment of a boot object
*)

type cap_env = {
    mutable boot_std_in :   capability;     (* standard IO channels *)
    mutable boot_std_out :  capability;
    mutable boot_std_err :  capability;
    mutable boot_root: capability;          (* root cap. of this boot obj.  *)
    mutable boot_tod: capability;
    mutable boot_rand: capability;
}

let nilenv = {
    boot_std_in=nilcap;
    boot_std_out=nilcap;
    boot_std_err=nilcap;
    boot_root=nilcap;
    boot_tod=nilcap;
    boot_rand=nilcap;
}
    
type boot_obj = {
    mutable boot_def :      boot_def;
    mutable boot_type :     boot_type;      (* the boot object type         *)
    mutable boot_stat :     boot_stat;      (* current state of boot obj.   *)
    mutable boot_op   :     boot_op;        (* the current boot op. (1)     *)
    mutable boot_owner :    capability;     (* owner of process             *)
    mutable boot_poll_cap : capability;     (* alive capability (std_info)  *)
    mutable boot_tid :      int;            (* Thread id                    *)
    mutable boot_id :       int;            (* Object number                *)
    mutable boot_pid :      int;            (* Unix process id              *)
    mutable boot_proc_cap:  capability;     (* Amoeba process cap           *)
    mutable boot_chain:     boot_obj list;  (* list of dep. boot objs.      *)
    mutable boot_chain_inv: boot_obj list;  (* inv. list of dep. boot objs. *)
    mutable boot_capenv:    cap_env;        (* capability environment       *)
}   
(*
** Notes:
**
**  (1): the boot operation currently executed: 
**          Boot_stop   ( even in the case on startup else start -> stop )
**          Boot_start  ( the boot object is up/down/polled )
**          Boot_restart    ( stop - start - poll )
**  With these three operation modes it's possible to control the
**  boot object at runtime, for example to stop a boot object.
*)

let nilobj = { boot_def = nildef; 
               boot_type = Nil_type;
               boot_stat = Boot_unknown;
               boot_op = Boot_stop;
               boot_owner = nilcap;
               boot_poll_cap = nilcap;
               boot_tid = -1; 
               boot_id = -1; 
               boot_pid = -1;
               boot_proc_cap = nilcap;
               boot_chain = []; 
               boot_chain_inv = [];
               boot_capenv=nilenv}


let newobj () =
    let obj = { boot_def = nildef; 
                boot_type = Nil_type;
                boot_stat = Boot_unknown;
                boot_op = Boot_stop;
                boot_owner = nilcap;
                boot_poll_cap = nilcap;
                boot_tid = -1; 
                boot_id = -1; 
                boot_pid = -1;
                boot_proc_cap = nilcap;
                boot_chain = []; boot_chain_inv = [];
                boot_capenv={boot_std_in=nilcap;
                             boot_std_out=nilcap;
                             boot_std_err=nilcap;
                             boot_root=nilcap;
                             boot_tod=nilcap;
                             boot_rand=nilcap
                             }}
       in
    obj
        
(*
** The boot server admin structure
*)

type boot_server = {

    boot_prv_port : port;
    boot_pub_port : port;
    boot_rnd_port : port;
    boot_supercap : capability;

    boot_objs : boot_obj array;     (* Array of all boot objects    *)
    boot_afu:   afu_server;         (* AFS-UNIX file mapper         *)

    mutable boot_dying : bool;
    mutable boot_timer : int;
    boot_sema : semaphore;

}



(*
** Standard capabilities from the boot server
*)


(*
** util functions
*)

type file_type =
    | Boot_binary
    | Boot_bytecode     (* -> #!/.../vamrun *)
    | Boot_script       (* -> #!/.../vam    *)

let unix_file_type filename =
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
                    std_OBJBAD,Boot_binary
                else
                begin
                    let str' = String.sub str 0 !pos in
                    let basename = Filename.basename str' in
                    match basename with
                    | "vamrun"  -> std_OK,Boot_bytecode;
                    | "vam"     -> std_OK,Boot_script;  
                    | "vamsys"  -> std_OK,Boot_script;  
                    | _ -> std_OBJBAD,Boot_binary;
                end;
            end;
        | _ -> std_OK,Boot_binary
    end
    with | _ -> std_IOERR,Boot_binary

let amoeba_file_type fcap =
    let buf = buf_create 100 in   
    let stat,n = afs_read fcap 0 buf 100 in
    if stat = std_OK then
    begin
        let str = string_of_buf buf in
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
                    std_OBJBAD,Boot_binary
                else
                begin
                    let str' = String.sub str 0 !pos in
                    let basename = Filename.basename str' in
                    match basename with
                    | "vamrun"  -> std_OK,Boot_bytecode;
                    | "vam"     -> std_OK,Boot_script;  
                    | "vamsys"  -> std_OK,Boot_script;  
                    | _ -> std_OBJBAD,Boot_binary;
                end;
            end;
        | _ -> std_OK,Boot_binary
    end
    else 
        std_IOERR,Boot_binary
    

(*
** Get the boot operations
*)


let op_start ops =
    let found = ref false in
    List.iter (fun o -> 
            match o with
            | Boot_start -> found := true;
            | _ -> ();
        ) ops;
    !found
let op_coldstart ops =
    let found = ref false in
    List.iter (fun o -> 
            match o with
            | Boot_coldstart -> found := true;
            | _ -> ();
        ) ops;
    !found
let op_stop ops =
    let found = ref false in
    List.iter (fun o -> 
            match o with
            | Boot_stop -> found := true;
            | _ -> ();
        ) ops;
    !found
let op_poll ops =
    let found = ref false in
    List.iter (fun o -> 
            match o with
            | Boot_poll _ -> found := true;
            | _ -> ();
        ) ops;
    !found
let op_restart ops =
    let found = ref false in
    List.iter (fun o -> 
            match o with
            | Boot_restart -> found := true;
            | _ -> ();
        ) ops;
    !found



(*
** Print informations and warnings ...
*)
let boot_warn_obj bo s =
    sys_log Sys_warn
            "BOOT: Warning: Boot object %s: %s\n" 
            bo.boot_def.boot_name
            s

let boot_fatal_obj bo s =
    sys_log Sys_fatal
            "BOOT: Fatal: Boot object %s: %s\n" 
            bo.boot_def.boot_name
            s;
    exit 1

let boot_info_obj bo s =
    sys_log Sys_info
            "BOOT: Boot object %s: %s\n" 
            bo.boot_def.boot_name
            s

let boot_warn s =
    sys_log Sys_warn
            "BOOT: Warning: %s\n" 
            s

let boot_info s =
    sys_log Sys_info
            "BOOT: %s\n" 
            s



let boot_stop_obj bo =
    
    boot_info_obj bo "Shutting down object...";
    bo.boot_stat <- Boot_killing;

    match bo.boot_type with
    | Unix_type ->
    begin
        (*
        ** Unix_dst
        *)
        if bo.boot_pid = -1 then
            boot_fatal_obj bo "boot_stop_obj: Unix_type, but no pid!";

        if (bo.boot_poll_cap <> nilcap) then
        begin
            (*
            ** Send the std_EXIT request to the poll cap...
            *)
            let stat = std_exit bo.boot_poll_cap in
            __(thread_delay 2 SEC);

            if (stat = std_OK) then
            begin
                boot_info_obj bo "Boot object down.";
                bo.boot_stat <- Boot_down;
                std_OK
            end
            else
            begin
                boot_warn_obj bo (sprintf "STD_EXIT shutdown failed: %s" (err_why stat));
                (*
                ** Now the hard way ...
                *)
                ( try
                    Unix.kill bo.boot_pid 9;
                  with
                    _ -> ()
                ); 
                __(thread_delay 2 SEC);
                boot_info_obj bo "Boot object down.";
                bo.boot_stat <- Boot_down;
                std_OK
            end;
        end
        else
        begin
            (*
            ** Give the process the chance to quit...
            *)
            ( try
                Unix.kill bo.boot_pid 15;
              with
                _ -> ()
            ); 
            __(thread_delay 2 SEC);
            (*
            ** Now the hard way ...
            *)
            ( try
                Unix.kill bo.boot_pid 9;
              with
                _ -> ()
            ); 
            __(thread_delay 2 SEC);

            boot_info_obj bo "Boot object down.";
            bo.boot_stat <- Boot_down;
            std_OK
        end;
    end;
    | Amoeba_type ->
    begin
        if bo.boot_proc_cap = nilcap then
            boot_fatal_obj bo "boot_stop_obj: Amoeba_type, but no proc cap!";

        if (bo.boot_poll_cap <> nilcap ) then
        begin
            let stat = std_exit bo.boot_poll_cap in
            __(thread_delay 2 SEC);
            if (stat = std_OK) then
            begin
                boot_info_obj bo "Boot object down.";
                bo.boot_stat <- Boot_down;
                std_OK
            end
            else
            begin
                boot_warn_obj bo (sprintf "STD_EXIT shutdown failed: %s" (err_why stat));
                (*
                ** Now the hard way
                *)
                let stat = pro_stun bo.boot_proc_cap 0 in
                if stat <> std_OK then
                    boot_warn_obj bo (sprintf "process stun  failed: %s" (err_why stat))
                else
                    boot_info_obj bo "Boot object down.";
                bo.boot_stat <- Boot_down;                    
                stat;
            end;
        end
        else
        begin
            let stat = pro_stun bo.boot_proc_cap 0 in
            if stat <> std_OK then                
                boot_warn_obj bo (sprintf "process stun failed: %s" (err_why stat))
            else                                  
                boot_info_obj bo "Boot object down.";
            bo.boot_stat <- Boot_down;
            stat;                         
        end;
    end;
    | _ -> bo.boot_stat <- Boot_down;   (* ??? *)
           std_OK


(*
** Start and control a single boot object.
*)

let boot_control_obj server bo =
    let stat = ref std_OK in
    let tmo = ref server.boot_timer in
    let cap_env = bo.boot_capenv in
    let afu_server = ref None in

    let get_type () =
        match bo.boot_def.boot_dst with
        | Unix_dst     -> Unix_type;
        | Amoeba_dst _ -> Amoeba_type;
        | _ -> 
        begin
            match bo.boot_def.boot_src with
            | Fun_src _ -> Fun_type;
            | _ -> Nil_type 
        end;
        in
    bo.boot_type <- get_type ();

    boot_info_obj bo "I'm alive";

    let control () =
        let mode = bo.boot_type in
        let src = bo.boot_def.boot_src in  
        let dst = bo.boot_def.boot_dst in      
        let coldstart,start,stop,poll,restart = 
            op_coldstart bo.boot_def.boot_ops,
            op_start bo.boot_def.boot_ops,
            op_stop  bo.boot_def.boot_ops,
            op_poll  bo.boot_def.boot_ops,
            op_restart bo.boot_def.boot_ops in

        bo.boot_stat <- Boot_cold;

        let args = bo.boot_def.boot_args in
        let envs = bo.boot_def.boot_env in

        let do_killafu () =
                match !afu_server with
                | Some afu_server ->        
                        let stat = afu_stop afu_server in
                        if stat <> std_OK then  
                            boot_warn_obj bo "can't stop AFU server"
                        else
                            boot_info_obj bo "AFU server terminated";
                | None -> ();
            in

        (*
        ** Get the poll capability if any. Also used for shutdown using the
        ** std_exit request.
        *)
        let poll_cap_path = ref (Unix_path "") in

        if (poll = true) then
        begin
            List.iter (fun o ->
                match o with
                | Boot_poll (p,tmo') ->
                begin
                    tmo := tmo';
                    match p with
                    | Boot_capstr s -> 
                    begin
                            bo.boot_poll_cap <- ar_tocap s;
                    end;
                    | Boot_cap c -> 
                    begin
                            bo.boot_poll_cap <- c;
                    end;
                    | Boot_path p ->
                    begin
                        let p'= path_resolve p in
                        match p' with    
                        | Unix_path p ->
                        begin
                            poll_cap_path := p';
                            let stat,cap = Buf.read_cap p in
                            if (stat = std_OK) then
                                bo.boot_poll_cap <- cap
                            else
                                boot_warn_obj bo "Invalid poll specifier!";
                        end;
                        | Amoeba_path p ->
                        begin
                            poll_cap_path := p';
                            let stat,cap = dir_lookup cap_env.boot_root p in
                            if (stat = std_OK) then
                                bo.boot_poll_cap <- cap
                            else
                                boot_warn_obj bo "Invalid poll specifier!";
                        end;
                    end;
                    | _ -> boot_fatal_obj bo "Invalid poll capability";
                end;
                | _ -> ();
                ) bo.boot_def.boot_ops;
        end;

        (*
        ** Are there any dependencies ?
        *)
        List.iter (fun d ->
                let tries = ref 10 in
                while ((d.boot_stat <> Boot_up &&
                        d.boot_stat <> Boot_executed) && !tries <> 0) 
                do
                    __(thread_delay 1 SEC);
                    tries := !tries - 1;
                done;
                if (!tries = 0) then
                    boot_warn_obj bo ("Giving up to wait for "^d.boot_def.boot_name);
            ) bo.boot_chain;


        let root_cs = cs_singleton cap_env.boot_root in

        (*
        ** Executes a UNIX program.
        *)

        let do_unix_exec path args env =
            (*
            ** Determine file type:
            ** Binary,Bytecode,Script
            *)
            let args = ref args in
            let stat,ftype = unix_file_type path in

            if stat <> std_OK then boot_fatal_obj bo "can't open file";

            let execfile = match ftype with
                    | Boot_binary -> path;
                    | Boot_bytecode ->
                    begin
                        (*
                        ** The vamrun program path must be
                        ** specified in the environment.
                        *)
                        let vamrun = ref "" in
                        List.iter (fun e ->
                                match e with
                                | Env_str ("VAMRUN",path') -> vamrun := path';
                                | _ -> ();
                                ) envs;
                        if !vamrun = "" then boot_fatal_obj bo 
                                                    "no VAMRUN environment";
                        args := ["-b";path] @ !args;
                        let path = path_resolve !vamrun in
                        match path with 
                        | Unix_path path' -> path';
                        | _ -> boot_fatal_obj bo "unexpected vamrun path";
                    end          
                    | Boot_script ->
                                    boot_fatal_obj bo "VAM TODO";
                in

            boot_info_obj bo "Starting Unix boot object...";


            (*
            ** Prepare the capability environment
            *)
            let envunix =  ref [] in
 
            List.iter(fun be ->
                    match be with
                    | Env_cap (s,c) ->
                        (*
                        ** only for UNIX: append 'CAP' to capability name
                        *)
                        envunix := !envunix @ [(sprintf "%sCAP=%s"
                                               s (ar_cap c))];
                    | Env_capstr (s,c) ->
                        envunix := !envunix @ [(sprintf "%sCAP=%s"
                                               s c)];
                    | Env_cappath (s,p) ->
                    begin                    
                        let path = path_resolve p in
                        match path with
                        | Amoeba_path path' ->
                            let stat,cs = dns_lookup root_cs p in
                            if stat <> std_OK then
                                boot_fatal_obj bo "can't lookup Env_cappath";
                            let _,cap = cs_to_cap cs in
                            envunix := !envunix @ [(sprintf "%sCAP=%s"
                                               s (ar_cap cap))];
                        | Unix_path path' ->
                            boot_fatal_obj bo "unexpected Unix_path/TODO";
                    end;
                    | Env_str (s1,s2) -> 
                        envunix := !envunix @ [(sprintf "%s=%s" s1 s2)];
                    | _ -> ();
                ) env;

            args := [execfile] @ !args;

            let pid = Unix.fork () in
            if (pid = 0) then
            begin
                Unix.execve execfile  (Array.of_list !args) 
                                      (Array.of_list !envunix);
            end;
            bo.boot_pid <- pid;
            in 



        (*
        ** Executes an Amoeba file on a native Amoeba machine
        ** either specified with file path
        ** or capability. VM bytecode programs must be specified
        ** with the Boot_path argument, that means filepath <> ""!
        *)

        let do_amoeba_exec procsrv filepath filecap args env =
            let args = ref args in
            (*
            ** Determine file type:
            ** Binary,Bytecode,Script
            *)
            let stat,ftype = amoeba_file_type filecap in

            if stat <> std_OK then boot_fatal_obj bo "can't open file";

            let filecap = match ftype with
                    | Boot_binary -> 
                            if filepath = "" 
                                then args := [bo.boot_def.boot_name] @ !args
                                else args := [filepath] @ !args;
                            filecap;
                    | Boot_bytecode ->
                    begin
                        (*
                        ** The vamrun program path or capability must be
                        ** specified in the environment.
                        *)
                        let vamrun = ref "" in
                        let vamcap = ref nilcap in
                        List.iter (fun e ->
                                match e with
                                | Env_str ("VAMRUN",path) -> vamrun := path;
                                | Env_cap ("VAMRUN",cap) -> vamcap := cap;
                                | Env_capstr ("VAMRUN",capstr) ->
                                                vamcap := ar_tocap capstr;
                                | Env_cappath ("VAMRUN",cappath) ->
                                        let stat,fcap_cs = dns_lookup
                                                            root_cs 
                                                            cappath in
                                        if stat <> std_OK then
                                            boot_fatal_obj bo
                                                "can't lookup vamrun cap";
                                        let _,cap = cs_to_cap fcap_cs in
                                        vamcap := cap;
                                | _ -> ();                                                                        
                                ) env;
                        if !vamrun = "" &&
                           !vamcap = nilcap 
                            then boot_fatal_obj bo "no VAMRUN environment";

                        if filepath = "" 
                            then boot_fatal_obj bo "filepath not specified";

                        args := ["-b";filepath] @ !args;
                        if !vamrun <> "" then
                        begin
                            let path = path_resolve !vamrun in
                            match path with 
                            | Amoeba_path path' -> 
                            begin
                                let stat,fcap_cs = dns_lookup
                                                            root_cs 
                                                            path' in
                                if stat <> std_OK then
                                            boot_fatal_obj bo
                                                "can't lookup vamrun path";
                                let _,cap = cs_to_cap fcap_cs in
                                args := [path'] @ !args;
                                cap
                            end;
                            | _ -> boot_fatal_obj bo "unexpected vamrun path";
                        end
                        else
                        begin
                            args := ["vamrun"] @ !args;
                            !vamcap
                        end;
                    end          
                    | Boot_script ->
                                    boot_fatal_obj bo "VAM TODO";
                in


            boot_info_obj bo "Starting Amoeba boot object...";

            (*
            ** Prepare the capability environment
            *)
            let envstr = ref [] in
            let envcap = ref [] in


            List.iter(fun be ->
                    match be with
                    | Env_cap (s,c) ->
                        envcap := !envcap @ [s,c];
                    | Env_capstr (s,c) ->
                        envcap := !envcap @ [s,ar_tocap c];
                    | Env_cappath (s,p) ->
                    begin                    
                        let stat,cs = dns_lookup root_cs p in
                        if stat <> std_OK then
                            boot_fatal_obj bo (sprintf 
                                               "can't lookup Env_cappath: %s"
                                               p);
                        let _,cap = cs_to_cap cs in
                        envcap := !envcap @ [s,cap];
                    end;
                    | Env_str (s1,s2) -> 
                        envstr := !envstr @ [(sprintf "%s=%s" s1 s2)];
                    | _ -> ();
                ) env;


            (*
            ** Read execfile descriptor.
            *)
            let stat,pd = pd_read filecap in 
            if (stat <> std_OK) then
            begin
                boot_fatal_obj bo "Reading process descriptor failed.";
            end;

            let owner_getport = uniqport () in
            let owner_putport = priv2pub owner_getport in
            let owner = {cap_port = owner_putport; cap_priv = nilpriv} in
            let stat,proccap =
                    exec_pd ~pd:    pd
                        ~host:      procsrv
                        ~owner:     owner
                        ~stacksize: 0
                        ~args:      !args
                        ~strenv:    !envstr
                        ~capenv:    !envcap
                        ~opt:       []
                    in
            if (stat != std_OK) then
            begin
                out (Printf.sprintf "exec_pd failed: %s" (err_why stat)); nl();
                boot_fatal_obj bo (sprintf "exec_pd failed: %s" (err_why stat));
                raise (Error stat);
            end;

            bo.boot_proc_cap <- proccap;

            let waiter () =
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

                boot_info_obj bo state;     
                if (normal = false) then
                begin
                    let _,pd = buf_get_pd ~buf:pdbuf ~pos:0 in
                    print_pd pd; nl ();
                end;        
                ignore(putrep(hdr_req,nilbuf,0));
                in
            __(thread_create waiter ());
            in


        (*
        ** Start a program on a native Amoeba machine. The program is loaded
        ** from the local UNIX filesystem.
        *)
        let do_amoeba_unix_exec procsrv filepath args env =
            let filecap = nilcap in

            (*
            ** Determine file type:
            ** Binary,Bytecode,Script
            *)
            let args = ref args in
            let stat,ftype = unix_file_type filepath in

            if stat <> std_OK then boot_fatal_obj bo "can't open file";

            (*
            ** We must provide an Amoeba AFS interface for the
            ** program file, and in the case of bytecode/scripts
            ** for the VM or toplevel program.
            *)
            do_killafu ();

            let afu = afu_init afs_REQBUFSZ 5 in
            afu_server := Some afu;

            let tempfile = ref "" in

            let filecap = match ftype with
                    | Boot_binary -> 
                        let stat,afu_file = afu_open_file afu filepath in
                        if stat <> std_OK then 
                            boot_fatal_obj bo "afu_open_file failed";
                        afu_file_cap afu afu_file;
                    | Boot_bytecode ->
                    begin
                        (*
                        ** The vamrun program path must be
                        ** specified in the environment.
                        *)
                        let stat,afu_file = afu_open_file afu filepath in
                        if stat <> std_OK then 
                            boot_fatal_obj bo "afu_open_file of target file failed";
                        let vamrun = ref "" in
                        List.iter (fun e ->
                                match e with
                                | Env_str ("VAMRUN",path') -> vamrun := path';
                                | _ -> ();
                                ) envs;
                        if !vamrun = "" then boot_fatal_obj bo 
                                                    "no VAMRUN environment";
                        vamrun := (match (path_resolve !vamrun) with
                                   | Unix_path path -> path;
                                   | _ -> boot_fatal_obj bo "unexpected vamrun path";
                                  );

                        let stat,afu_vamrun = afu_open_file afu !vamrun in
                        if stat <> std_OK then 
                            boot_fatal_obj bo "afu_open_file of vamrun failed";

                        (*
                        ** Publish file cap in a temporary Amoeba directory
                        *)
                        let tmpfile =  
                            sprintf "/tmp/%s@%d.vax"
                                (Filename.basename filepath)                   
                                (Random.bits ()) in
                        __(afu_publ_file afu afu_file tmpfile);

                        tempfile := tmpfile;

                        args := [!vamrun;"-b";tmpfile] @ !args;
                        afu_file_cap afu afu_vamrun;                        
                    end          
                    | Boot_script ->
                                    boot_fatal_obj bo "VAM TODO";
                in

    
            boot_info_obj bo "Starting AFU server...";
    
            ignore (thread_create (fun () -> afu_server_loop afu) ());

            boot_info_obj bo "Starting Amoeba object loaded from UNIX...";

            (*
            ** Prepare the capability environment
            *)
            let envstr = ref [] in
            let envcap = ref [] in


            List.iter(fun be ->
                    match be with
                    | Env_cap (s,c) ->
                        envcap := !envcap @ [s,c];
                    | Env_capstr (s,c) ->
                        envcap := !envcap @ [s,ar_tocap c];
                    | Env_cappath (s,p) ->
                    begin                    
                        let stat,cs = dns_lookup root_cs p in
                        if stat <> std_OK then
                            boot_fatal_obj bo (sprintf 
                                               "can't lookup Env_cappath: %s"
                                               p);
                        let _,cap = cs_to_cap cs in
                        envcap := !envcap @ [s,cap];
                    end;
                    | Env_str (s1,s2) -> 
                        envstr := !envstr @ [(sprintf "%s=%s" s1 s2)];
                    | _ -> ();
                ) env;


            (*
            ** Read execfile descriptor.
            *)
            let stat,pd = pd_read filecap in 
            if (stat <> std_OK) then
            begin
                boot_fatal_obj bo "Reading process descriptor failed.";
            end;

            let owner_getport = uniqport () in
            let owner_putport = priv2pub owner_getport in
            let owner = {cap_port = owner_putport; cap_priv = nilpriv} in
            let stat,proccap =
                    exec_pd ~pd:    pd
                        ~host:      procsrv
                        ~owner:     owner
                        ~stacksize: 0
                        ~args:      !args
                        ~strenv:    !envstr
                        ~capenv:    !envcap
                        ~opt:       []
                    in
            if (stat != std_OK) then
            begin
                out (Printf.sprintf "exec_pd failed: %s" (err_why stat)); nl();
                boot_fatal_obj bo (sprintf "exec_pd failed: %s" (err_why stat));
                raise (Error stat);
            end;

            bo.boot_proc_cap <- proccap;

            let waiter () =
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

                boot_info_obj bo state;     
                if (normal = false) then
                begin
                    let _,pd = buf_get_pd ~buf:pdbuf ~pos:0 in
                    print_pd pd; nl ();
                end;        
                ignore(putrep(hdr_req,nilbuf,0));
                if !tempfile <> "" then
                    __(name_delete !tempfile);
                in
            __(thread_create waiter ());
            in





        let do_start () =
            match src with
            | Unix_src loc ->
            begin
              match loc with
              | Boot_path srcpath ->
              begin
                let srcpath' = path_resolve srcpath in
                match srcpath' with
                | Unix_path srcpath'' ->
                begin
                    match dst with
                    | Unix_dst ->
                        (*
                        ** from Unix to Unix
                        *)
                        do_unix_exec srcpath'' args envs;

                    | Amoeba_dst proccserver ->
                    begin
                        match dst with
                        | Amoeba_dst procserver->
                        begin
                            (*
                            ** from Unix to Amoeba    
                            *)
                            match procserver with
                            | Boot_path dstpath ->
                            begin
                                let dstpath' = path_resolve dstpath in
                                match dstpath' with
                                | Amoeba_path dstpath'' -> 
                                    let stat,pcap_cs = dns_lookup 
                                                        root_cs dstpath'' in
                                    if stat <> std_OK then
                                        boot_fatal_obj bo "can't lookup process server";
                                    let _,pcap=cs_to_cap pcap_cs in

                                    do_amoeba_unix_exec pcap 
                                                        srcpath''
                                                        args 
                                                        envs;
                                | Unix_path _ -> 
                                    boot_fatal_obj bo "unexpected Unix_path";
                            end;
                            | Boot_cap pcap ->
                                do_amoeba_unix_exec pcap 
                                                    srcpath''
                                                    args 
                                                    envs;
                            | Boot_capstr capstr ->
                                do_amoeba_unix_exec (ar_tocap capstr) 
                                                    srcpath'' 
                                                    args 
                                                    envs;
                            | Boot_nil ->
                                boot_fatal_obj bo "unexpected dst Boot_nil";
                        end;
                        | Unix_dst ->
                            boot_fatal_obj bo "unexpected Unix_dst";
                        | _ -> boot_fatal_obj bo "unexpected destination/TODO";
                    end;
                    | _ -> boot_fatal_obj bo "unexpected destination/TODO";
                end;
                | _ -> boot_fatal_obj bo "unexpected src Amoeba_path";
              end; 
              | Boot_cap cap ->  
              begin
                boot_fatal_obj bo "unexpected src Boot_cap";
              end;
              | Boot_capstr str ->  
              begin
                boot_fatal_obj bo "unexpected src Boot_capstr";
              end;
              | Boot_nil ->
                boot_fatal_obj bo "unexpected src Boot_nil";
            end;
            | Amoeba_src loc -> 
            begin
              match loc with
              | Boot_path srcpath ->
              begin
                let srcpath' = path_resolve srcpath in
                match srcpath' with
                | Amoeba_path srcpath'' ->
                begin
                    let stat,fcap_cs = dns_lookup root_cs srcpath'' in
                    if stat <> std_OK then
                        boot_fatal_obj bo "can't lookup file capability";
                    let _,fcap = cs_to_cap fcap_cs in

                    match dst with
                    | Amoeba_dst procserver->
                    begin
                        (*
                        ** Simple: from Amoeba to Amoeba    
                        *)
                        match procserver with
                        | Boot_path dstpath ->
                        begin
                            let dstpath' = path_resolve dstpath in
                            match dstpath' with
                            | Amoeba_path dstpath'' -> 
                                let stat,pcap_cs = dns_lookup 
                                                        root_cs dstpath'' in
                                if stat <> std_OK then
                                    boot_fatal_obj bo "can't lookup process server";
                                let _,pcap=cs_to_cap pcap_cs in

                                do_amoeba_exec pcap 
                                               srcpath'' 
                                               fcap 
                                               args 
                                               envs;
                            | Unix_path _ -> 
                                boot_fatal_obj bo "unexpected Unix_path";
                        end;
                        | Boot_cap pcap ->
                            do_amoeba_exec pcap 
                                           srcpath''
                                           fcap 
                                           args 
                                           envs;
                        | Boot_capstr capstr ->
                            do_amoeba_exec (ar_tocap capstr) 
                                           srcpath''
                                           fcap 
                                           args 
                                           envs;
                        | Boot_nil ->
                            boot_fatal_obj bo "unexpected dst Boot_nil";
                    end;
                    | Unix_dst ->
                        boot_fatal_obj bo "unexpected Unix_dst";
                    | _ -> boot_fatal_obj bo "unexpected destination/TODO";
                end;
                | _ -> boot_fatal_obj bo "unexpected src Unix_path";
              end; 
              | Boot_cap cap ->  
              begin
                boot_fatal_obj bo "unexpected src Boot_cap/TODO";
              end;
              | Boot_capstr str ->  
              begin
                boot_fatal_obj bo "unexpected src Boot_capstr/TODO";
              end;
              | Boot_nil ->
                boot_fatal_obj bo "unexpected src Boot_nil";
            end;
            | Fun_src f -> 
                boot_info_obj bo "Executing function object...";
                let stat = f () in
                if stat <> std_OK then
                    boot_fatal_obj bo (
                        sprintf 
                        "boot object function returns with status: %s"
                        (err_why stat));

            | _ -> boot_fatal_obj bo "unexpected source"; 
            in


        let do_poll () =
            let stat,buf = std_info bo.boot_poll_cap 50 in

            !stats.(bo.boot_id) <- !stats.(bo.boot_id) + 1;

            if (stat <> std_OK) then
            begin
                (*
                ** Try to resolve again poll cap paths...
                *)
                match !poll_cap_path with
                | Unix_path "" -> ();
                | Unix_path p -> 
                            let stat,cap = Buf.read_cap p in
                            if (stat = std_OK) then
                                bo.boot_poll_cap <- cap
                | Amoeba_path p -> 
                            let stat,cap = dir_lookup cap_env.boot_root p in
                            if (stat = std_OK) then
                                bo.boot_poll_cap <- cap
            end;
            stat
            in


        let shutdown_dependencies () =
            (*
            ** Resolve dependencies ...
            *)
            List.iter (fun d ->
                boot_info_obj bo ("waiting for "^d.boot_def.boot_name);
                let tries = ref 10 in
                while (d.boot_stat <> Boot_down && !tries <> 0) 
                do
                    __(thread_delay 1 SEC);
                    tries := !tries - 1;
                done;
                if (!tries = 0) then
                    boot_warn_obj bo ("Giving up to wait for "^d.boot_def.boot_name);
                ) bo.boot_chain_inv;
            in


        (*
        ** Kill the boot object. Either due to Boot_start -> Boot_stop
        ** transition or on boot server shutdown.
        *)
        let do_kill () = 
            if (bo.boot_stat <> Boot_killing) then
            begin
                let stat = boot_stop_obj bo in
                if (stat <> std_OK) then
                begin
                    boot_warn_obj bo
                        (sprintf "Can't shutdown boot object: %s"
                         (err_why stat));    
                end;
                (*
                ** Maybe a afu server is running... stop it too ...
                *)
                do_killafu ();
            end;
            in

        (*
        ** The main control state machine
        *)
        let do_control () =
            let state = ref Boot_cold in
            let next_state = ref Boot_cold in
            while (server.boot_dying = false)
            do
                (match !state with 
                | Boot_cold -> 
                begin
                    if (coldstart = true) ||
                       (start = true && poll = false) then
                    begin
                       bo.boot_op <- Boot_start;
                       bo.boot_stat <- Boot_starting;
                       do_start ();
                       next_state := Boot_starting;
                    end
                    else if (start = true  && poll = true)
                       then
                    begin
                       bo.boot_op <- Boot_start;
                        (*
                        ** First look for an already running boot object
                        ** checking the poll server capability.
                        *)
                        let stat = do_poll () in
                        if stat = std_OK then
                        begin
                            boot_info_obj bo "Found boot object already up!";
                            bo.boot_stat <- Boot_up;
                            next_state := Boot_up;
                        end
                        else
                        begin
                           bo.boot_stat <- Boot_starting;
                           do_start ();
                           next_state := Boot_starting;
                        end;
                    end;
                    next_state := Boot_starting;
                end;
                | Boot_starting -> 
                begin
                    if poll = true then
                    begin
                        let stat = do_poll () in
                        if stat = std_OK then
                        begin
                            boot_info_obj bo "Boot object up!";
                            next_state := Boot_up;
                            bo.boot_stat <- Boot_up;
                        end; 
                    end
                    else
                    begin
                        next_state := Boot_executed;
                        bo.boot_stat <- Boot_executed;
                    end;
                end;                    
                | Boot_up ->
                begin
                    if bo.boot_op = Boot_start then
                    begin
                        if poll then
                        begin
                            let stat = do_poll () in
                            if stat <> std_OK then
                            begin
                                boot_info_obj bo "Boot object down!";
                                bo.boot_stat <- Boot_down;
                                next_state := Boot_down;
                                if restart = true then
                                begin
                                    boot_info_obj bo "Restarting boot object...";
                                    bo.boot_stat <- Boot_restarting;
                                    do_start ();
                                    next_state := Boot_restarting;
                                end;
                            end;
                        end;
                    end
                    else if bo.boot_op = Boot_stop then
                    begin
                        bo.boot_stat <- Boot_down;
                        do_kill ();
                        next_state := Boot_down;
                    end
                    else (* Boot_restart *)
                    begin
                        bo.boot_stat <- Boot_down;
                        do_kill ();
                        bo.boot_op <- Boot_start;
                        do_start ();
                        bo.boot_stat <- Boot_restarting;
                        next_state := Boot_restarting;
                    end;
                end;
                | Boot_restarting ->
                begin
                    let stat = do_poll () in
                    if stat = std_OK then
                    begin
                        boot_info_obj bo "Boot object up again!";
                        next_state := Boot_up;
                        bo.boot_stat <- Boot_up;
                    end
                    else
                    begin
                        boot_info_obj bo "Boot object still down!";
                        next_state := Boot_down;
                        bo.boot_stat <- Boot_down;
                    end
                end;
                | Boot_down ->
                begin
                    (*
                    ** Either the boot object vanished due a 
                    ** failure (boot_op still Boot_start), or an
                    ** external boot object stop was initiated.
                    *)
                    if bo.boot_op = Boot_start then
                    begin
                        let stat = do_poll () in
                        if stat = std_OK then
                        begin
                            boot_info_obj bo "Boot object up again!!!";
                            next_state := Boot_up;
                            bo.boot_stat <- Boot_up;
                        end
                        else
                        begin
                            next_state := Boot_down;
                            bo.boot_stat <- Boot_down;
                        end;
                    end
                    else if bo.boot_op = Boot_stop then
                    begin
                        next_state := Boot_down;
                    end 
                    else (* Boot_restart *)
                    begin
                        bo.boot_stat <- Boot_starting;
                        do_start ();
                        next_state := Boot_restarting;
                    end;
                end;
            
                | Boot_executed -> 
                begin
                    boot_info_obj bo "Boot object up!";
                    next_state := Boot_up; 
                end;
                | _ -> ();
                );
                state := !next_state;
                __(thread_delay !tmo SEC);
            done;
            in


        do_control ();

        (* TODO: stop service on shutdown in do_control *)
        if server.boot_dying then
            shutdown_dependencies ();

        do_kill ();


        boot_info_obj bo "Object exits.";

    
        sema_up server.boot_sema;
        thread_exit ();
        in

    let tid = thread_create control () in
    bo.boot_tid <- tid;
    (* 
    ** We need flipd, too! Give this program a chance to initialize...
    *)
    if (bo.boot_def.boot_name = "flipd") then
        __(thread_delay 2 SEC);
    !stat

(*
** Do coldstart of all objects. Each objects will be controlled
** by a separate boot thread.
*)

let boot_start_all bs =
    Array.iter (fun bo ->
        let stat = boot_control_obj bs bo in
        if (stat <> std_OK) then
            boot_fatal_obj bo (Printf.sprintf 
                    "boot_control_obj failed: %s" (err_why stat));
        ) bs.boot_objs;
    std_OK

(*
** Return the status informations of all boot objects.
*)

let boot_status bs =
    let s = ref "" in
    Array.iter (fun bo ->
            s := !s ^ (Printf.sprintf "%20s : %30s         (polled=%d)\n"
                        bo.boot_def.boot_name
                        (match bo.boot_stat with
                            | Boot_cold -> "Ground zero ...";
                            | Boot_starting -> "Starting ...";
                            | Boot_killing -> "Killing ...";
                            | Boot_executed -> "Executed!";
                            | Boot_up -> "Up!";
                            | Boot_down -> "Down or not reachable!";
                            | Boot_restarting -> "Restarting ...";
                            | Boot_unknown -> "What's going on ?";
                        ) 
                        (!stats.(bo.boot_id)));
        ) bs.boot_objs;
    !s,std_OK
    

(*
** Public entry for boot object configuration.
** The boot control state machine of each object will do the real work...
*)

let boot_start bs bo =
    if bo.boot_op = Boot_stop then
    begin
        bo.boot_op <- Boot_start;
        std_OK
    end
    else
        std_ARGBAD


let boot_stop bs bo =
    if bo.boot_op = Boot_start then
    begin
        bo.boot_op <- Boot_stop;
        std_OK
    end
    else
        std_ARGBAD

let boot_restart bs bo =
    if bo.boot_op = Boot_start then
    begin
        bo.boot_op <- Boot_restart;
        std_OK
    end
    else
        std_ARGBAD

(*
** Main loop. Waits for shutdown ...
*)

let boot_loop ~server =
    let bs = server in
    boot_info "Init stage performed.";
    boot_info (Printf.sprintf "I, voyager: %s" (Ar.ar_cap bs.boot_supercap));

    let buf = buf_create 30000 in
    let repsiz = ref 0 in
    while (server.boot_dying = false)
    do
        repsiz := 0;
        let stat,reqsiz,hreq = getreq (bs.boot_prv_port,nilbuf,0) in
        if (stat = std_OK) then
        begin
            begin
                match hreq.h_command with
                | com when com = std_EXIT -> 
                begin
                    server.boot_dying <- true;
                    hreq.h_status <- std_OK
                end;
                | com when com = std_INFO ->
                begin
                    let info = "BOOT server" in
                    buf_sets buf 0 info;
                    repsiz := (String.length info) + 1;
                    hreq.h_status <- std_OK;
                end;
                | com when com = std_STATUS ->
                begin
                    let s,stat = boot_status bs in
                    if (stat = std_OK) then
                    begin
                        buf_sets buf 0 s;
                        repsiz := (String.length s) + 1;
                        hreq.h_status <- std_OK;
                    end
                    else hreq.h_status <- stat;
                end;
                | _ -> hreq.h_status <- std_ARGBAD;
            end;
            ignore(putrep (hreq,buf,!repsiz));
        end;
    done;
    std_OK
    
(*
** Wait after shutdown for all bootobject threads
*)
let boot_wait ~server =
    let bs = server in
    let objn = Array.length bs.boot_objs in
    for i = 1 to objn
    do
        sema_down bs.boot_sema;
    done;
    std_OK

(*
** Read the boot object definitions and create the
** boot server super structure.
*)

let boot_init defs =

    (*
    ** An AFS-UNIX file mapper server is needed for execution of
    ** native Amoeba binaries loaded from UNIX filesystem.
    *)
    let afu_server = afu_init afs_REQBUFSZ 4 in

    (*
    ** Check the user specified capability environment. Add
    ** missing standard capabilities, update boot_obj caps, too.
    **
    ** Standard capabilities:
    **
    **      ROOT
    **      AFS
    **      DNS
    **      TTY
    **      STDOUT
    **      STDIN
    **      STDERR
    **      TOD
    **      RANDOM
    **      WORK
    *)
    
    let build_env bo =
        let user_env = bo.boot_def.boot_env in
        let cap_env = bo.boot_capenv in

        let get_cap e =
            match e with
            | Env_cap (_,cap) -> cap;
            | Env_cappath (_,path) ->
                    (*
                    ** get capability from DNS relative to current boot
                    ** object root
                    *)
                    let root_cs = cs_singleton cap_env.boot_root in
                    let stat,cap_cs = dns_lookup root_cs path in
                    if stat <> std_OK then
                        boot_fatal_obj bo "can't lookup environment cap";
                    let _,cap=cs_to_cap cap_cs in
                    cap
            | _ -> failwith "progerror";
            in

        let put_env e cap =
            bo.boot_def.boot_env <- bo.boot_def.boot_env @
                                    [Env_cap (e,cap)];
            in

        let stdl = [
            "ROOT";
            "AFS";
            "DNS";
            "TTY";
            "STDIN";
            "STDOUT";
            "STDERR";
            "WORK";
            "TOD";
            "RANDOM"; ] in
        
        List.iter (fun e ->
                try 
                begin
                    let env = List.find (fun be ->
                                match be with
                                | Env_cap (name,_) -> name = e;
                                | Env_cappath (name,_) -> name=e;
                                | _ -> false;
                                ) user_env in
                    match e with
                    | "ROOT" -> cap_env.boot_root <- get_cap env;
                    | "TTY"  -> cap_env.boot_std_in <- get_cap env;
                                cap_env.boot_std_out <- get_cap env;
                                cap_env.boot_std_err <- get_cap env;
                    | "STDIN" -> cap_env.boot_std_in <- get_cap env;
                    | "STDOUT" -> cap_env.boot_std_out <- get_cap env;
                    | "STDERR" -> cap_env.boot_std_err <- get_cap env;
                    | "TOD" -> cap_env.boot_tod <- get_cap env;
                    | "RANDOM" -> cap_env.boot_rand <- get_cap env;
                    | _ -> ();
                end
                with Not_found -> 
                begin
                    match e with
                    | "ROOT" -> let _,cap = get_env_cap e in
                                cap_env.boot_root <- cap;
                                put_env e cap;
                    | "TTY"  -> let stat,cap = get_env_cap e in
                                if stat = std_OK then
                                begin
                                    cap_env.boot_std_in <- cap;
                                    cap_env.boot_std_out <- cap;
                                    cap_env.boot_std_err <- cap;
                                    put_env "STDIN" cap_env.boot_std_in;
                                    put_env "STDOUT" cap_env.boot_std_out;
                                    put_env "STDERR" cap_env.boot_std_err;
                                end;
                    | "STDIN" -> let _,cap = get_env_cap e in
                                 if cap_env.boot_std_in = nilcap then
                                     cap_env.boot_std_in <- cap;
                                 put_env e cap_env.boot_std_in;
                    | "STDOUT" -> let _,cap = get_env_cap e in 
                                  if cap_env.boot_std_out = nilcap then 
                                    cap_env.boot_std_out <- cap;
                                  put_env e cap_env.boot_std_out;
                    | "STDERR" -> let _,cap = get_env_cap e in
                                  if cap_env.boot_std_err = nilcap then
                                      cap_env.boot_std_err <- cap;
                                  put_env e cap_env.boot_std_err;
                    | "TOD" -> let _,cap = get_env_cap e in
                               cap_env.boot_tod <- cap;
                               put_env e cap;
                    | "RANDOM" -> let _,cap = get_env_cap e in  
                                  cap_env.boot_rand <- cap;
                                  put_env e cap;
                    | _ -> ();

                end;
            ) stdl;
        in

    let defsn = Array.length defs in
    let objs = Array.create defsn nilobj in
    stats := Array.create defsn 0;

    for i = 0 to defsn - 1
    do
        
        objs.(i) <- (newobj ());
        (*
        ** Initial boot object environment either derived from
        ** user specified environment or inherited by the boot servers
        ** environment.
        *)

        build_env objs.(i);

        objs.(i).boot_def <- defs.(i);
        objs.(i).boot_id <- i;
    done;

    (*
    ** Build dependency chain.
    *)

    for i = 0 to defsn - 1
    do
        List.iter (fun ds ->
                try
                (
                    for j = 0 to defsn - 1
                    do
                        if (defs.(j).boot_name = ds && i <> j) then
                        begin
                            objs.(i).boot_chain <- 
                                objs.(i).boot_chain @ [objs.(j)];
                            raise Exit;
                        end;
                    done;
                    boot_fatal_obj objs.(i) ("Can't find dependency object: "^ds);
                )
                with Exit -> ();
            ) defs.(i).boot_deps;     
    done;

    (*
    ** Build inverse (shutdown) dependency chain. Wait only for
    ** boot object with Boot_stop operation specified.
    *)
    
    let inv_list = ref [] in
    for i = 0 to defsn - 1
    do
            List.iter (fun os ->
                if (List.mem objs.(i) os.boot_chain_inv)=false &&
                   (op_stop objs.(i).boot_def.boot_ops) then
                    os.boot_chain_inv <- os.boot_chain_inv @ [objs.(i)];
            ) objs.(i).boot_chain;
    done;
    

    let prvport = uniqport () in
    let rndport = uniqport () in
    let pubport = priv2pub prvport in


    let bs = {
        boot_prv_port = prvport;
        boot_pub_port = pubport;
        boot_rnd_port = rndport;
        boot_supercap = {cap_port = pubport;
                         cap_priv = prv_encode (Objnum 0) prv_all_rights
                                               rndport};
        boot_objs = objs;
        boot_afu = afu_server;
        boot_sema = sema_create 0;
        boot_dying = false;
        boot_timer = 5;
    } in
    bs,std_OK


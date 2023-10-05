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
**    $INITIAL:     (C) 2005 BSSLAB
**    $CREATED:     28.7.2005
**    $VERSION:     1.07
**
**    $INFO:
**
**  High level process execution. Both Unix and Amoeba sources
**  and targets are supported. Derived from Vamboot module.
**
**    $ENDOFINFO
**
*)

open Amoeba
open Unix
open Stderr
open Thread
open Name
open Ksys
open Cap_env
open Printf
open Afs_common
open Afs_client
open Dns_client
open Afu_server
open Bytebuf
open Syslog
open Ar
open Dir
open Capset
open Proc
open Rpc
open Stdcom
open Sema

let out = print_string
let nl = print_newline


type file_type = 
    | Exec_binary
    | Exec_bytecode      (* #!/.../vamrun *)
    | Exec_script        (* #!/.../vam *)

(*
**  Object capabilty:
**  1. looked up from a path (1)
**  2. a given capability
**  3. a given capability in ASCII representation format
** 
*)

type exec_objcap = 
    | Exec_path of string        (* the object filesystem path (1)(2)        *)
    | Exec_cap  of capability    (* a capability specifies an amoeba object  *)
    | Exec_capstr of string      (* a capability in string format:        
                                ** XX:XX:XX:XX:XX:XX/n(r)/YY:YY:YY:YY:YY:YY
                                *)
    | Exec_nil

(*
** Source file loaction
*)

type exec_src =
    | Unix_src of exec_objcap        (* Unix local file              *)
    | Amoeba_src of exec_objcap      (* Amoeba file                  *)
    | Nil_src
    
(*
** Destination description, for example the boot server.
*)

type exec_dst =
    | Unix_dst                  (* executes on Unix ...             *)
    | Amoeba_dst of exec_objcap (* amoeba process server            *)
    | Nil_dst


(*
** Different types (modes) of objects are supported.
*)

type exec_type =
    | Unix_type          (* start an Unix binary     *)
    | Amoeba_type        (* start an Amoeba binary  *)
    | Nil_type 


type exec_env =
    (*
    ** Capability environment
    *)
    | Env_cap of (string * capability)      (* Capability   *)
    | Env_cappath of (string * string)      (* Cap. looked up from path *)
    | Env_capstr of (string * string)       (* Cap. in ASCII representation *)

    (*
    ** String environment
    *)
    | Env_str of (string * string)         
    

(*
** Standard capability environment of a exec object
*)

type cap_env = {
    mutable exec_std_in :   capability;     (* standard IO channels *)
    mutable exec_std_out :  capability;
    mutable exec_std_err :  capability;
    mutable exec_root: capability;          (* root cap. for this obj *)
    mutable exec_tod: capability;
    mutable exec_rand: capability;
}

(*
** exec object operations
*)
type exec_op =
    | Exec_poll of (exec_objcap * int)  (* poll path/cap and time interval  *)
    | Exec_start                        (* start the service - of course    *)
    | Exec_stop                         (* stop the service                 *)
    | Exec_restart                      (* restart if poll failed           *)
    | Exec_coldstart                    (* start without polling            *)

(*
** Status of a exec server object
*)

type exec_stat =
    | Exec_cold         (* nothing done untill now      *)
    | Exec_starting     (* The exec object starts...    *) 
    | Exec_killing      (* Shutdown the exec object ... *)
    | Exec_up           (* Polling says: the exec object is alive *)
    | Exec_down         (* Polling says: the exec object is not rechable *)
    | Exec_executed     (* Without polling: exec object started. *)
    | Exec_restarting   (* After Exec_down: exec object restarts *)
    | Exec_unknown      (* Unknown exec object state *)

(*
** exec object defintions
*)
type exec_def = {
    mutable exec_name : string;             (* name of the exec object  *)
    mutable exec_src  : exec_src;           (* the exec object source   *)
    mutable exec_dst  : exec_dst;           (* the exec object dst.     *)
    mutable exec_args : string list;        (* program arguments        *)
    mutable exec_env  : exec_env list;      (* program cap and str env. *)
    mutable exec_ops  : exec_op list;       (* exec obj. operations     *)
}

type exec_obj = {
    mutable exec_def :      exec_def;
    mutable exec_type :     exec_type;      (* the exec object type         *)
    mutable exec_stat :     exec_stat;      (* current state of exec obj.   *)
    mutable exec_tid :      int;
    mutable exec_op   :     exec_op;        (* the current exec op. (1)     *)
    mutable exec_owner :    capability;     (* owner of process             *)
    mutable exec_poll_cap : capability;     (* alive capability (std_info)  *)
    mutable exec_pid :      int;            (* Unix process id              *)
    mutable exec_proc_cap:  capability;     (* Amoeba process cap           *)
    mutable exec_capenv:    cap_env;        (* capability environment       *)
    mutable exec_print :    (string -> unit);   (* print messages *)
}   


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
                    std_OBJBAD,Exec_binary
                else
                begin
                    let str' = String.sub str 0 !pos in
                    let basename = Filename.basename str' in
                    match basename with
                    | "vamrun"  -> std_OK,Exec_bytecode;
                    | "vam"     -> std_OK,Exec_script;  
                    | "vamsys"  -> std_OK,Exec_script;  
                    | _ -> std_OBJBAD,Exec_binary;
                end;
            end;
        | _ -> std_OK,Exec_binary
    end
    with | _ -> std_IOERR,Exec_binary

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
                    std_OBJBAD,Exec_binary
                else
                begin
                    let str' = String.sub str 0 !pos in
                    let basename = Filename.basename str' in
                    match basename with
                    | "vamrun"  -> std_OK,Exec_bytecode;
                    | "vam"     -> std_OK,Exec_script;  
                    | "vamsys"  -> std_OK,Exec_script;  
                    | _ -> std_OBJBAD,Exec_binary;
                end;
            end;
        | _ -> std_OK,Exec_binary
    end
    else 
        std_IOERR,Exec_binary

let max_files = 5

let buf_size = 30000

(*
** Print informations and warnings ...
*)
let exec_warn_obj bo s =
    bo.exec_print 
            (sprintf "Warning: Exec object %s: %s\n"
                     bo.exec_def.exec_name
                     s)

let exec_fatal_obj bo s =
    bo.exec_print 
            (sprintf "Fatal: Exec object %s: %s\n"
                     bo.exec_def.exec_name
                     s);
    raise Exit

let exec_info_obj bo s =
    bo.exec_print
            (sprintf "Exec object %s: %s\n"
                     bo.exec_def.exec_name
                     s)


(*
** Get the exec operations
*)


let op_start ops =
    let found = ref false in
    List.iter (fun o -> 
            match o with
            | Exec_start -> found := true;
            | _ -> ();
        ) ops;
    !found
let op_coldstart ops =
    let found = ref false in
    List.iter (fun o -> 
            match o with
            | Exec_coldstart -> found := true;
            | _ -> ();
        ) ops;
    !found
let op_stop ops =
    let found = ref false in
    List.iter (fun o -> 
            match o with
            | Exec_stop -> found := true;
            | _ -> ();
        ) ops;
    !found
let op_poll ops =
    let found = ref false in
    List.iter (fun o -> 
            match o with
            | Exec_poll _ -> found := true;
            | _ -> ();
        ) ops;
    !found
let op_restart ops =
    let found = ref false in
    List.iter (fun o -> 
            match o with
            | Exec_restart -> found := true;
            | _ -> ();
        ) ops;
    !found


let nilcap' = ar_cap nilcap

let nildef = { exec_name = "";
               exec_src = Nil_src;
               exec_dst = Nil_dst;
               exec_args = [];
               exec_env = [];
               exec_ops = []}


let newobj () =
    let obj = { exec_def = nildef;
                exec_type = Nil_type;
                exec_stat = Exec_unknown;
                exec_op = Exec_stop;
                exec_owner = nilcap;
                exec_poll_cap = nilcap;
                exec_tid = -1;
                exec_pid = -1;
                exec_proc_cap = nilcap;
                exec_capenv={exec_std_in=nilcap;
                             exec_std_out=nilcap;
                             exec_std_err=nilcap;
                             exec_root=nilcap;
                             exec_tod=nilcap;
                             exec_rand=nilcap
                             };
                exec_print = (fun str -> print_string str;
                                         print_newline ());
                }
       in
    obj


(*
** Stop an exec object
*)
let exec_stop_obj obj =
  let err = ref std_SYSERR in
  protect (
    exec_info_obj obj "Shutting down object...";
    obj.exec_stat <- Exec_killing;

    match obj.exec_type with
    | Unix_type ->
    begin
        (*
        ** Unix_dst
        *)
        if obj.exec_pid = -1 then
            exec_fatal_obj obj "exec_stop_obj: Unix_type, but no pid!";

        if (obj.exec_poll_cap <> nilcap) then
        begin
            (*
            ** Send the std_EXIT request to the poll cap...
            *)
            let stat = std_exit obj.exec_poll_cap in
            __(thread_delay 2 SEC);

            if (stat = std_OK) then
            begin
                exec_info_obj obj "Exec object down.";
                obj.exec_stat <- Exec_down;
                err := std_OK
            end
            else
            begin
                exec_warn_obj obj (sprintf "STD_EXIT shutdown failed: %s" (err_why stat));
                (*
                ** Now the hard way ...
                *)
                ( try
                    Unix.kill obj.exec_pid 9;
                  with
                    _ -> ()
                ); 
                __(thread_delay 2 SEC);
                exec_info_obj obj "Exec object down.";
                obj.exec_stat <- Exec_down;
                err := std_OK;
            end;
        end
        else
        begin
            (*
            ** Give the process the chance to quit...
            *)
            ( try
                Unix.kill obj.exec_pid 15;
              with
                _ -> ()
            ); 
            __(thread_delay 2 SEC);
            (*
            ** Now the hard way ...
            *)
            ( try
                Unix.kill obj.exec_pid 9;
              with
                _ -> ()
            ); 
            __(thread_delay 2 SEC);

            exec_info_obj obj "Exec object down.";
            obj.exec_stat <- Exec_down;
            err := std_OK
        end;
    end;
    | Amoeba_type ->
    begin
        if obj.exec_proc_cap = nilcap then
            exec_fatal_obj obj "exec_stop_obj: Amoeba_type, but no proc cap!";

        if (obj.exec_poll_cap <> nilcap ) then
        begin
            let stat = std_exit obj.exec_poll_cap in
            __(thread_delay 2 SEC);
            if (stat = std_OK) then
            begin
                exec_info_obj obj "Exec object down.";
                obj.exec_stat <- Exec_down;
                err := std_OK
            end
            else
            begin
                exec_warn_obj obj (sprintf "STD_EXIT shutdown failed: %s" (err_why stat));
                (*
                ** Now the hard way
                *)
                let stat = pro_stun obj.exec_proc_cap 0 in
                if stat <> std_OK then
                    exec_warn_obj obj (sprintf "process stun  failed: %s" (err_why stat))
                else
                    exec_info_obj obj "Exec object down.";
                obj.exec_stat <- Exec_down;                    
                err := stat;
            end;
        end
        else
        begin
            let stat = pro_stun obj.exec_proc_cap 0 in
            if stat <> std_OK then                
                exec_warn_obj obj (sprintf "process stun failed: %s" (err_why stat))
            else                                  
                exec_info_obj obj "Exec object down.";
            obj.exec_stat <- Exec_down;
            err := stat;                         
        end;
    end;
    | _ -> obj.exec_stat <- Exec_down;   (* ??? *)
           err := std_OK
    ); !err


(*
** Start and control a single exec object. First argument specifies
** exec object definitions, and the second a log print function.
*)

let exec_control_obj obj_def print =

    (*
    ** Check the user specified capability environment. Add
    ** missing standard capabilities, update exec_obj caps, too.
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
    
    let build_env obj =
        let user_env = obj.exec_def.exec_env in
        let cap_env = obj.exec_capenv in

        let get_cap e =
            match e with
            | Env_cap (_,cap) -> cap;
            | Env_cappath (_,path) ->
                    (*
                    ** get capability from DNS relative to current exec
                    ** object root
                    *)
                    let root_cs = cs_singleton cap_env.exec_root in
                    let stat,cap_cs = dns_lookup root_cs path in
                    if stat <> std_OK then
                        exec_fatal_obj obj "can't lookup environment cap";
                    let _,cap=cs_to_cap cap_cs in
                    cap
            | _ -> failwith "progerror";
            in

        let put_env e cap =
            obj.exec_def.exec_env <- obj.exec_def.exec_env @
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
                    let cap = get_cap env in
                    match e with
                    | "ROOT" -> cap_env.exec_root <- cap;
                    | "TTY"  -> cap_env.exec_std_in <- cap;
                                cap_env.exec_std_out <- cap;
                                cap_env.exec_std_err <- cap;
                    | "STDIN" -> cap_env.exec_std_in <- cap;
                    | "STDOUT" -> cap_env.exec_std_out <- cap;
                    | "STDERR" -> cap_env.exec_std_err <- cap;
                    | "TOD" -> cap_env.exec_tod <- cap;
                    | "RANDOM" -> cap_env.exec_rand <- cap;
                    | _ -> ();
                end
                with Not_found -> 
                begin
                    match e with
                    | "ROOT" -> let _,cap = get_env_cap e in
                                cap_env.exec_root <- cap;
                                put_env e cap;
                    | "TTY"  -> let stat,cap = get_env_cap e in
                                if stat = std_OK then
                                begin
                                    cap_env.exec_std_in <- cap;
                                    cap_env.exec_std_out <- cap;
                                    cap_env.exec_std_err <- cap;
                                    put_env "TTY" cap;
                                    put_env "STDIN" cap_env.exec_std_in;
                                    put_env "STDOUT" cap_env.exec_std_out;
                                    put_env "STDERR" cap_env.exec_std_err;
                                end;
                    | "STDIN" -> let _,cap = get_env_cap e in
                                 if cap_env.exec_std_in = nilcap then
                                     cap_env.exec_std_in <- cap;
                                 put_env e cap_env.exec_std_in;
                    | "STDOUT" -> let _,cap = get_env_cap e in 
                                  if cap_env.exec_std_out = nilcap then
                                      cap_env.exec_std_out <- cap;
                                  put_env e cap_env.exec_std_out;
                    | "STDERR" -> let _,cap = get_env_cap e in
                                  if cap_env.exec_std_err = nilcap then
                                      cap_env.exec_std_err <- cap;
                                  put_env e cap_env.exec_std_err;
                    | "TOD" -> let _,cap = get_env_cap e in
                               cap_env.exec_tod <- cap;
                               put_env e cap;
                    | "RANDOM" -> let _,cap = get_env_cap e in  
                                  cap_env.exec_rand <- cap;
                                  put_env e cap;
                    | _ -> ();

                end;
            ) stdl;
        in

    let obj = newobj () in
    obj.exec_print <- print;

    (*
    ** Initial object environment either derived from
    ** user specified environment or inherited by current process
    ** environment.
    *)
    obj.exec_def <- obj_def;
    build_env obj;

    

    let stat = ref std_OK in
    let tmo = ref 1 in
    let cap_env = obj.exec_capenv in
    let afu_server = ref None in
    let dying = ref false in

    let get_type () =
        match obj.exec_def.exec_dst with
        | Unix_dst     -> Unix_type;
        | Amoeba_dst _ -> Amoeba_type;
        | _ ->  Nil_type 
        in
    obj.exec_type <- get_type ();

    exec_info_obj obj "I'm alive";

    let control () =
        let mode = obj.exec_type in
        let src = obj.exec_def.exec_src in  
        let dst = obj.exec_def.exec_dst in      
        let coldstart,start,stop,poll,restart = 
            op_coldstart obj.exec_def.exec_ops,
            op_start obj.exec_def.exec_ops,
            op_stop  obj.exec_def.exec_ops,
            op_poll  obj.exec_def.exec_ops,
            op_restart obj.exec_def.exec_ops in

        obj.exec_stat <- Exec_cold;

        let args = obj.exec_def.exec_args in
        let envs = obj.exec_def.exec_env in


        let do_killafu () =
                match !afu_server with
                | Some afu_server ->        
                        let stat = afu_stop afu_server in
                        if stat <> std_OK then  
                            exec_warn_obj obj "can't stop AFU server"
                        else
                            exec_info_obj obj "AFU server terminated";
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
                | Exec_poll (p,tmo') ->
                begin
                    tmo := tmo';
                    match p with
                    | Exec_capstr s -> 
                    begin
                            obj.exec_poll_cap <- ar_tocap s;
                    end;
                    | Exec_cap c -> 
                    begin
                            obj.exec_poll_cap <- c;
                    end;
                    | Exec_path p ->
                    begin
                        let p'= path_resolve p in
                        match p' with    
                        | Unix_path p ->
                        begin
                            poll_cap_path := p';
                            let stat,cap = Buf.read_cap p in
                            if (stat = std_OK) then
                                obj.exec_poll_cap <- cap
                            else
                                exec_warn_obj obj "Invalid poll specifier!";
                        end;
                        | Amoeba_path p ->
                        begin
                            poll_cap_path := p';
                            let stat,cap = dir_lookup cap_env.exec_root p in
                            if (stat = std_OK) then
                                obj.exec_poll_cap <- cap
                            else
                                exec_warn_obj obj "Invalid poll specifier!";
                        end;
                    end;
                    | _ -> exec_fatal_obj obj "Invalid poll capability";
                end;
                | _ -> ();
                ) obj.exec_def.exec_ops;
        end;


        let root_cs = cs_singleton cap_env.exec_root in

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

            if stat <> std_OK then exec_fatal_obj obj "can't open file";

            let execfile = match ftype with
                    | Exec_binary -> path;
                    | Exec_bytecode ->
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
                        if !vamrun = "" then exec_fatal_obj obj 
                                                    "no VAMRUN environment";
                        args := ["-b";path] @ !args;
                        let path = path_resolve !vamrun in
                        match path with 
                        | Unix_path path' -> path';
                        | _ -> exec_fatal_obj obj "unexpected vamrun path";
                    end          
                    | Exec_script ->
                                    exec_fatal_obj obj "VAM TODO";
                in

            exec_info_obj obj "Starting Unix object...";


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
                                exec_fatal_obj obj "can't lookup Env_cappath";
                            let _,cap = cs_to_cap cs in
                            envunix := !envunix @ [(sprintf "%sCAP=%s"
                                               s (ar_cap cap))];
                        | Unix_path path' ->
                            exec_fatal_obj obj "unexpected Unix_path/TODO";
                    end;
                    | Env_str (s1,s2) -> 
                        envunix := !envunix @ [(sprintf "%s=%s" s1 s2)];
                ) env;

            args := [execfile] @ !args;

            let pid = Unix.fork () in
            if (pid = 0) then
            begin
                Unix.execve execfile  (Array.of_list !args) 
                                      (Array.of_list !envunix);
            end;
            obj.exec_pid <- pid;
            in 



        (*
        ** Executes an Amoeba file on a native Amoeba machine
        ** either specified with file path
        ** or capability. VM bytecode programs must be specified
        ** with the Exec_path argument, that means filepath <> ""!
        *)

        let do_amoeba_exec procsrv filepath filecap args env =
            let args = ref args in
            (*
            ** Determine file type:
            ** Binary,Bytecode,Script
            *)
            let stat,ftype = amoeba_file_type filecap in

            if stat <> std_OK then exec_fatal_obj obj "can't open file";

            let filecap = match ftype with
                    | Exec_binary -> 
                            if filepath = "" 
                                then args := [obj.exec_def.exec_name] @ !args
                                else args := [filepath] @ !args;
                            filecap;
                    | Exec_bytecode ->
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
                                            exec_fatal_obj obj
                                                "can't lookup vamrun cap";
                                        let _,cap = cs_to_cap fcap_cs in
                                        vamcap := cap;
                                | _ -> ();                                                                        
                                ) env;
                        if !vamrun = "" &&
                           !vamcap = nilcap 
                            then exec_fatal_obj obj "no VAMRUN environment";

                        if filepath = "" 
                            then exec_fatal_obj obj "filepath not specified";

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
                                            exec_fatal_obj obj
                                                "can't lookup vamrun path";
                                let _,cap = cs_to_cap fcap_cs in
                                args := [path'] @ !args;
                                cap
                            end;
                            | _ -> exec_fatal_obj obj "unexpected vamrun path";
                        end
                        else
                        begin
                            args := ["vamrun"] @ !args;
                            !vamcap
                        end;
                    end          
                    | Exec_script ->
                                    exec_fatal_obj obj "VAM TODO";
                in


            exec_info_obj obj "Starting Amoeba object...";

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
                            exec_fatal_obj obj (sprintf 
                                               "can't lookup Env_cappath: %s"
                                               p);
                        let _,cap = cs_to_cap cs in
                        envcap := !envcap @ [s,cap];
                    end;
                    | Env_str (s1,s2) -> 
                        envstr := !envstr @ [(sprintf "%s=%s" s1 s2)];
                ) env;


            (*
            ** Read execfile descriptor.
            *)
            let stat,pd = pd_read filecap in 
            if (stat <> std_OK) then
            begin
                exec_fatal_obj obj "Reading process descriptor failed.";
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
                exec_fatal_obj obj (sprintf "exec_pd failed: %s" (err_why stat));
                raise (Error stat);
            end
            else
            begin
                __(pro_setcomment proccap (Filename.basename filepath));
            end;

            obj.exec_proc_cap <- proccap;

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

                exec_info_obj obj state;     
                obj.exec_stat <- Exec_down;
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

            if stat <> std_OK then exec_fatal_obj obj "can't open file";

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
                    | Exec_binary -> 
                        let stat,afu_file = afu_open_file afu filepath in
                        if stat <> std_OK then 
                            exec_fatal_obj obj "afu_open_file failed";
                        args := [filepath] @ !args;
                        afu_file_cap afu afu_file;
                    | Exec_bytecode ->
                    begin
                        (*
                        ** The vamrun program path must be
                        ** specified in the environment.
                        *)
                        let stat,afu_file = afu_open_file afu filepath in
                        if stat <> std_OK then 
                            exec_fatal_obj obj "afu_open_file of target file failed";
                        let vamrun = ref "" in
                        List.iter (fun e ->
                                match e with
                                | Env_str ("VAMRUN",path') -> vamrun := path';
                                | _ -> ();
                                ) envs;
                        if !vamrun = "" then exec_fatal_obj obj 
                                                    "no VAMRUN environment";
                        vamrun := (match (path_resolve !vamrun) with
                                   | Unix_path path -> path;
                                   | _ -> exec_fatal_obj obj "unexpected vamrun path";
                                  );

                        let stat,afu_vamrun = afu_open_file afu !vamrun in
                        if stat <> std_OK then 
                            exec_fatal_obj obj "afu_open_file of vamrun failed";

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
                    | Exec_script ->
                                    exec_fatal_obj obj "VAM TODO";
                in

    
            exec_info_obj obj "Starting AFU server...";
    
            ignore (thread_create (fun () -> afu_server_loop afu) ());

            exec_info_obj obj "Starting Amoeba object loaded from UNIX...";

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
                            exec_fatal_obj obj (sprintf 
                                               "can't lookup Env_cappath: %s"
                                               p);
                        let _,cap = cs_to_cap cs in
                        envcap := !envcap @ [s,cap];
                    end;
                    | Env_str (s1,s2) -> 
                        envstr := !envstr @ [(sprintf "%s=%s" s1 s2)];
                ) env;


            (*
            ** Read execfile descriptor.
            *)
            let stat,pd = pd_read filecap in 
            if (stat <> std_OK) then
            begin
                exec_fatal_obj obj "Reading process descriptor failed.";
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
                exec_fatal_obj obj (sprintf "exec_pd failed: %s" (err_why stat));
                raise (Error stat);
            end
            else
            begin
                __(pro_setcomment proccap (Filename.basename filepath));
            end;

            obj.exec_proc_cap <- proccap;

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

                exec_info_obj obj state;     
                obj.exec_stat <- Exec_down;
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
              | Exec_path srcpath ->
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
                            | Exec_path dstpath ->
                            begin
                                let dstpath' = path_resolve dstpath in
                                match dstpath' with
                                | Amoeba_path dstpath'' -> 
                                    let stat,pcap_cs = dns_lookup 
                                                        root_cs dstpath'' in
                                    if stat <> std_OK then
                                        exec_fatal_obj obj "can't lookup process server";
                                    let _,pcap=cs_to_cap pcap_cs in

                                    do_amoeba_unix_exec pcap 
                                                        srcpath''
                                                        args 
                                                        envs;
                                | Unix_path _ -> 
                                    exec_fatal_obj obj "unexpected Unix_path";
                            end;
                            | Exec_cap pcap ->
                                do_amoeba_unix_exec pcap 
                                                    srcpath''
                                                    args 
                                                    envs;
                            | Exec_capstr capstr ->
                                do_amoeba_unix_exec (ar_tocap capstr) 
                                                    srcpath'' 
                                                    args 
                                                    envs;
                            | Exec_nil ->
                                exec_fatal_obj obj "unexpected dst Exec_nil";
                        end;
                        | Unix_dst ->
                            exec_fatal_obj obj "unexpected Unix_dst";
                        | _ -> exec_fatal_obj obj "unexpected destination/TODO";
                    end;
                    | _ -> exec_fatal_obj obj "unexpected destination/TODO";
                end;
                | _ -> exec_fatal_obj obj "unexpected src Amoeba_path";
              end; 
              | Exec_cap cap ->  
              begin
                exec_fatal_obj obj "unexpected src Exec_cap";
              end;
              | Exec_capstr str ->  
              begin
                exec_fatal_obj obj "unexpected src Exec_capstr";
              end;
              | Exec_nil ->
                exec_fatal_obj obj "unexpected src Exec_nil";
            end;
            | Amoeba_src loc -> 
            begin
              match loc with
              | Exec_path srcpath ->
              begin
                let srcpath' = path_resolve srcpath in
                match srcpath' with
                | Amoeba_path srcpath'' ->
                begin
                    let stat,fcap_cs = dns_lookup root_cs srcpath'' in
                    if stat <> std_OK then
                        exec_fatal_obj obj "can't lookup file capability";
                    let _,fcap = cs_to_cap fcap_cs in

                    match dst with
                    | Amoeba_dst procserver->
                    begin
                        (*
                        ** Simple: from Amoeba to Amoeba    
                        *)
                        match procserver with
                        | Exec_path dstpath ->
                        begin
                            let dstpath' = path_resolve dstpath in
                            match dstpath' with
                            | Amoeba_path dstpath'' -> 
                                let stat,pcap_cs = dns_lookup 
                                                        root_cs dstpath'' in
                                if stat <> std_OK then
                                    exec_fatal_obj obj "can't lookup process server";
                                let _,pcap=cs_to_cap pcap_cs in

                                do_amoeba_exec pcap 
                                               srcpath'' 
                                               fcap 
                                               args 
                                               envs;
                            | Unix_path _ -> 
                                exec_fatal_obj obj "unexpected Unix_path";
                        end;
                        | Exec_cap pcap ->
                            do_amoeba_exec pcap 
                                           srcpath''
                                           fcap 
                                           args 
                                           envs;
                        | Exec_capstr capstr ->
                            do_amoeba_exec (ar_tocap capstr) 
                                           srcpath''
                                           fcap 
                                           args 
                                           envs;
                        | Exec_nil ->
                            exec_fatal_obj obj "unexpected dst Exec_nil";
                    end;
                    | Unix_dst ->
                        exec_fatal_obj obj "unexpected Unix_dst";
                    | _ -> exec_fatal_obj obj "unexpected destination/TODO";
                end;
                | _ -> exec_fatal_obj obj "unexpected src Unix_path";
              end; 
              | Exec_cap cap ->  
              begin
                exec_fatal_obj obj "unexpected src Exec_cap/TODO";
              end;
              | Exec_capstr str ->  
              begin
                exec_fatal_obj obj "unexpected src Exec_capstr/TODO";
              end;
              | Exec_nil ->
                exec_fatal_obj obj "unexpected src Exec_nil";
            end;
            | _ -> exec_fatal_obj obj "unexpected source"; 
            in


        let do_poll () =
            let stat,buf = std_info obj.exec_poll_cap 50 in

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
                                obj.exec_poll_cap <- cap
                | Amoeba_path p -> 
                            let stat,cap = dir_lookup cap_env.exec_root p in
                            if (stat = std_OK) then
                                obj.exec_poll_cap <- cap
            end;
            stat
            in



        (*
        ** Kill the exec object. 
        *)
        let do_kill () = 
            if (obj.exec_stat <> Exec_killing) then
            begin
                let stat = exec_stop_obj obj in
                if (stat <> std_OK) then
                begin
                    exec_warn_obj obj
                        (sprintf "Can't shutdown exec object: %s"
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
            let state = ref Exec_cold in
            let next_state = ref Exec_cold in
            while (!dying = false)
            do
                (match !state with 
                | Exec_cold -> 
                begin
                    if (coldstart = true) ||
                       (start = true && poll = false) then
                    begin
                       obj.exec_op <- Exec_start;
                       obj.exec_stat <- Exec_starting;
                       do_start ();
                       next_state := Exec_starting;
                    end
                    else if (start = true  && poll = true)
                       then
                    begin
                       obj.exec_op <- Exec_start;
                        (*
                        ** First look for an already running exec object
                        ** checking the poll server capability.
                        *)
                        let stat = do_poll () in
                        if stat = std_OK then
                        begin
                            exec_info_obj obj "Found exec object already up!";
                            obj.exec_stat <- Exec_up;
                            next_state := Exec_up;
                        end
                        else
                        begin
                           obj.exec_stat <- Exec_starting;
                           do_start ();
                           next_state := Exec_starting;
                        end;
                    end;
                    next_state := Exec_starting;
                end;
                | Exec_starting -> 
                begin
                    if poll = true then
                    begin
                        let stat = do_poll () in
                        if stat = std_OK then
                        begin
                            exec_info_obj obj "Exec object up!";
                            next_state := Exec_up;
                            obj.exec_stat <- Exec_up;
                        end
                        else if obj.exec_op = Exec_stop then
                        begin
                            (*
                            ** Forced object stop
                            *)
                            next_state := Exec_down;
                        end;
                    end
                    else
                    begin
                        next_state := Exec_executed;
                        obj.exec_stat <- Exec_executed;
                    end;
                end;                    
                | Exec_up ->
                begin
                    if obj.exec_op = Exec_start then
                    begin
                        if poll then
                        begin
                            let stat = do_poll () in
                            if stat <> std_OK then
                            begin
                                exec_info_obj obj "Exec object down!";
                                obj.exec_stat <- Exec_down;
                                next_state := Exec_down;
                                if restart = true then
                                begin
                                    exec_info_obj obj "Restarting exec object...";
                                    obj.exec_stat <- Exec_restarting;
                                    do_start ();
                                    next_state := Exec_restarting;
                                end;
                            end;
                        end;
                    end
                    else if obj.exec_op = Exec_stop then
                    begin
                        obj.exec_stat <- Exec_down;
                        do_kill ();
                        next_state := Exec_down;
                    end
                    else (* Exec_restart *)
                    begin
                        obj.exec_stat <- Exec_down;
                        do_kill ();
                        obj.exec_op <- Exec_start;
                        do_start ();
                        obj.exec_stat <- Exec_restarting;
                        next_state := Exec_restarting;
                    end;
                end;
                | Exec_restarting ->
                begin
                    let stat = do_poll () in
                    if stat = std_OK then
                    begin
                        exec_info_obj obj "Exec object up again!";
                        next_state := Exec_up;
                        obj.exec_stat <- Exec_up;
                    end
                    else
                    begin
                        exec_info_obj obj "Exec object still down!";
                        next_state := Exec_down;
                        obj.exec_stat <- Exec_down;
                    end
                end;
                | Exec_down ->
                begin
                    (*
                    ** Either the exec object vanished due a 
                    ** failure (exec_op still Exec_start), or an
                    ** external exec object stop was initiated.
                    *)
                    if obj.exec_op = Exec_start then
                    begin
                        let stat = do_poll () in
                        if stat = std_OK then
                        begin
                            exec_info_obj obj "Exec object up again!!!";
                            next_state := Exec_up;
                            obj.exec_stat <- Exec_up;
                        end
                        else
                        begin
                            next_state := Exec_down;
                            obj.exec_stat <- Exec_down;
                        end;
                    end
                    else if obj.exec_op = Exec_stop then
                    begin
                        next_state := Exec_down;
                    end 
                    else (* Exec_restart *)
                    begin
                        obj.exec_stat <- Exec_starting;
                        do_start ();
                        next_state := Exec_restarting;
                    end;
                end;
            
                | Exec_executed -> 
                begin
                    exec_info_obj obj "Exec object up!";
                    next_state := Exec_up; 
                end;
                | _ -> ();
                );
                state := !next_state;
                __(thread_delay !tmo SEC);
            done;
            in

        do_control ();

        do_kill ();

        exec_info_obj obj "Object exits.";

        thread_exit ();
        in

    let tid = thread_create (fun () ->
                    protect (control ())) () in
    obj.exec_tid <- tid;
    (* 
    ** We need flipd, too! Give this program a chance to initialize...
    *)
    if (obj.exec_def.exec_name = "flipd") then
        __(thread_delay 2 SEC);

    obj


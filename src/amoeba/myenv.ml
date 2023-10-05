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
**    $INITIAL:     (C) 2003 BSSLAB
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.11
**
**    $INFO:
**
** Amoeba capability environment == local named capabilities
**
** Standard environment capabilities:
**
** ROOT             - the root directory
** WORK             - the working directory
** STDIN            - standard input
** STDOUT           - standard output
** STDERR           - standard error
** SESSION/_SESSION - session server
** TTY              - the current terminal
**
** Native Amoeba:
**      Environment is supplied by the process environment 
**      by the process erver. A 'CAP prefix in the environment
**      name is here ignored!
**
** AMUNIX:
**      All environment variables are defined in the user
**      shell environment, for example in the shell profile
**      file. The used format:
**
**          ROOTCAP="0:0:0:0:0:0/0(0)/0:0:0:0:0:0"
**          export ROOTCAP ...
**
**      or with an absolute path name prefixed with
**      /unix for a UNIX file, which holds
**      the capability (stored with buf_put_cap/write_cap). 
**
**          ROOTCAP=/unix/amoeba/server/dns/.rootcap
**
**      or with an absolute path without this prefix. Then, the
**      cap is resolved by the directory server. This implicit that
**      the ROOT capability is already resolved (under UNIX only
**      possible with a direct cap or a unix cap file).
**
**      The CAP prefix is only present in the UNIX environment
**      name, not within VAM.
**
** In addition to the get_env_cap function, there is a 
** put_env_cap function to create or change environment variables -
** but only in the context of the current program. To perform this,
** an environment variable-capability hash is used.
**
** Note: environment capabilities and strings are supported     
**       on both the native Amoeva and AMUNIX VAM execution environment!
**
**    $ENDOFINFO
**
*)


open Amoeba
open Bytebuf
open Buf
open Stderr
open Os
open Name
open Ar
open Soap
open Capset

(*
** VAM can access both, the UNIX and the Amoeba file
** system - either native or within the AMUNIX environment.
** Therefore it's usefull to handle both, UNIX and Amoeba
** paths.
*)

(*
** Different types of paths:
**
** /unix/.... -> UNIX host system, UNIX interface is used
** else       -> Amoeba directory interface is used
**
*)

type path_arg = 
    | Unix_path of string
    | Amoeba_path of string


(*
** Resolve the type of a path string, do additional path
** cleanup (remove double slashs and so on).
** Is it a relative path without a leading '/' char, the Operating
** System we're currently running on determines the path type,
** extended with the content of the string WORK environment 
** variable. Moreover, if the path starts with a '~' char,
** the string HOME environment variable extends the (relative) path.
** Space chars in the path string will be eliminated! The
** parent reference ".." is resolved, too, and the '.' char is
** eliminated, too.
*)

let path_resolve path =
  if path <> "" then
  begin
    let path_normalize p =
        let p' = Str.global_replace (Str.regexp "//") "/" p in
        let p'' = Str.global_replace (Str.regexp " ") "" p' in        
        p'' in        

    let path = path_normalize path in

    let home_rel = '~' in
    let path_abs = '/' in

    let unix_prefix = "/unix" in
    let unix_prefix_len = 5 in

    (*
    ** Resolve relative paths
    *)
    let path = 
        let len = String.length path in
        match path.[0] with
        | c when c = path_abs -> path;
        | c when c = home_rel -> 
        begin
            (*
            ** Both native Amoeba and AMUNIX VAM support environment strings.
            *)
            let unix,path' = 
                let str = (String.sub path 1 (len-1)) in
                match Os.vm_os with
                | Os.OS_Unix -> "/unix",str;
                | Os.OS_Amoeba -> "",str;
                in

            let home = ref "" in
            let exists = protects(home:=Unix.getenv "HOME") in
            if exists then
                (unix ^ 
                 !home ^ 
                 (if path.[1] <> '/' then "/" else "") ^ 
                 path')
            else    (* return a fake path !!! *)            
                (unix ^ "/no_home_environment_specified/"^path')
        end
        | _ -> 
        begin
            (*
            ** relative to current WORK directory. Both native Amoeba
            ** and AMUNIX VAM support environment strings.
            *)
            let work = ref "" in
            let unix =
                match Os.vm_os with
                | Os.OS_Unix -> "/unix";
                | Os.OS_Amoeba -> "";
                in


            (*
            ** try first working directory...
            *)
            let exists = protects(work:=Unix.getenv "WORK") in
            if exists then
                (unix ^ !work ^ "/" ^ path)
            else
            begin
                (*
                ** and finally PWD...
                *)
                let pwd = ref "" in
                let exists = protects(pwd:=Unix.getenv "PWD") in
                if exists then
                    (unix ^ !pwd ^ "/" ^ path)
                else
                    (* return a fake path !!! *)            
                    (unix ^ "/no_work_or_pwd_environment_specified/"^path)
            end;
        end 
        in

    (*
    ** Resolve parent references ".."
    *)
    let path = 
        let par_ref = protects (__(Str.search_forward
                                    (Str.regexp "\.\.") path 0)) in
        if not par_ref then
            path
        else
        begin
            let pathl = Str.split (Str.regexp "/") path in
            let new_path = ref "" in
            let rec p_iter pl =
                match pl with
                | hd::tl ->
                begin
                    (
                      match tl with
                      | hd'::tl' -> 
                        if hd' <> ".." && hd <> ".." then
                            new_path := !new_path ^ "/" ^ hd;
                      | [] -> 
                        if hd <> ".." then
                            new_path := !new_path ^ "/" ^ hd;
                    );
                    p_iter tl;
                end;
                | [] -> ()
                in
            p_iter pathl;
            !new_path
        end
        in
        

    let path_len = String.length path in

    let postsl = if (path.[path_len-1] = '/') then
                    1
                 else
                    0
        in

    if (path_len >= unix_prefix_len) then
    begin
        let pref_test = String.sub path 0 unix_prefix_len in
        if (pref_test = unix_prefix) then
        begin
            let path = String.sub path unix_prefix_len
                                      (path_len-unix_prefix_len-postsl) in
            Unix_path (if path = "" then "/" else path)
        end
        else
        begin
            let path = String.sub path 0 (path_len-postsl) in
            Amoeba_path (if path = "" then "/" else path);
        end;
    end
    else
    begin
        let path = String.sub path 0 (path_len-postsl) in
        Amoeba_path (if path = "" then "/" else path)
    end
  end
  else
        Amoeba_path "/"

(*
** Input: Environment variable name
** Output: Capability stored in a string (buf_put_cap)
*)

(*
** This function is only used for native Amoeba OS.
*)

external am_getcap: string -> string
    = "am_getcap"


let hash = Hashtbl.create 20 

let get_env_cap env =

    (*
    ** Remove 'CAP' prefix if any
    *)
    let amenv = List.hd (Str.split (Str.regexp "CAP") env) in
    let env = amenv^"CAP" in

    try
        std_OK,(Hashtbl.find hash amenv)
    with
    | Not_found ->
    begin
        match vm_os with
        | OS_Amoeba ->
        begin
            let (pos,cap) = buf_get_cap ~buf:(buf_of_string (am_getcap amenv)) 
                                        ~pos:0 in
            if (cap=nilcap) then
                std_NOTFOUND,nilcap
            else
            begin
                Hashtbl.add hash amenv cap;
                std_OK,cap
            end
        end;
        | OS_Unix ->
        begin
            try
            begin
                let envcap = Unix.getenv env in
                if (envcap.[0] <> '/') then
                    std_OK,(ar_tocap envcap)
                else
                begin
                    let ap = path_resolve envcap in
                    match ap with 
                    | Unix_path p ->
                    begin
                        let stat,cap = read_cap p in
                        Hashtbl.add hash amenv cap;
                        stat,cap
                    end;
                    | Amoeba_path p -> 
                    begin
                        let stat,cap = name_lookup p in
                        Hashtbl.add hash amenv cap;
                        stat,cap
                    end;
                end;
            end
            with
                | Not_found -> std_NOTFOUND,nilcap
        end;
    end

(*
** Create or replace an environment capability.
*)

let put_env_cap env cap =
    (*
    ** Remove 'CAP' specifier if any
    *)
    let amenv = List.hd (Str.split (Str.regexp "CAP") env) in
    if (Hashtbl.mem hash env) = true then
        Hashtbl.remove hash amenv;

    Hashtbl.replace hash amenv cap;
    std_OK

(*
** Return all currently known environment variables
*)
let env_init = ref false

let get_env_caps () =
    if (!env_init = false) then
    begin
        (*
        ** search for common ones and install them here..
        *)
        let evs = [
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
        List.iter (fun name ->
            let _,cap = get_env_cap (name^"CAP") in
            ignore(put_env_cap name cap);
            ) evs;
        env_init := true;
    end;
    let ev = ref [] in
    Hashtbl.iter (fun key data ->
                    ev := !ev @ [key,data]) hash;
    !ev
    

(*
** String environment. Returns empty string if not found.
*)
let get_env_str env =
    let str = ref "" in
    protect (str:=Unix.getenv env);
    !str

let put_env_str env value =
    Unix.putenv env value

(*
** Initialize the capability environment
*)
        
let _ =
    let _,rootcap = get_env_cap "ROOTCAP" in
    let _,workcap = get_env_cap "WORKCAP" in
    dir_rootcs := cs_singleton rootcap; 
    dir_workcs := cs_singleton workcap


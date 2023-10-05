
open Amoeba
open Name
open Stderr
open Stdcom
open Cap_env


let version = 0.61



let stdin   = ref Pervasives.stdin
let stdout  = ref Pervasives.stdout
let stderr  = ref Pervasives.stderr
 
let out str =
    output_string !stdout str;
    flush !stdout

let err str =
    output_string !stderr str;
    flush !stderr
    
let nl () =
    output_string !stdout "\n";
    flush !stdout


     
(*
** Different types of paths:
**
** /unix/.... -> UNIX host system, UNIX interface is used
** else       -> Amoeba DNS interface is used
**
*)

type path_arg = 
    | Unix_path of string
    | Amoeba_path of string


let path_resolve path =
    let prefix = "/unix" in
    let prefix_len = String.length prefix in
    let path_len = String.length path in

    let postsl = if (path.[path_len-1] = '/') then
                    1
                 else
                    0
        in
    if (path_len >= prefix_len) then
    begin
        let pref_test = String.sub path 0 prefix_len in
        if (pref_test = prefix) then
            Unix_path ("/"^(String.sub path prefix_len
                                      (path_len-prefix_len-postsl)))
        else
            Amoeba_path (String.sub path 0 (path_len-postsl))
    end
    else
        Amoeba_path (String.sub path 0 (path_len-postsl))
        


(*
** Root and working directories
*)

let root_cap = ref (let _,cap = get_env_cap "ROOT" in cap)
let work_dir = ref (Amoeba_path "/")
let work_cap = ref (!root_cap)

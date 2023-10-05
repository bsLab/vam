
open Amoeba
open Name
open Stderr
open Stdcom
open Cap_env


let verbose = ref 0
let debug = ref false

let hostpath = ref "/hosts"

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
** Root and working directories
*)

let root_cap = ref (let _,cap = get_env_cap "ROOT" in cap)
let file_cap = ref (let _,cap = get_env_cap "AFS" in cap)
let work_dir = ref (Amoeba_path "/")
let work_cap = ref (!root_cap)

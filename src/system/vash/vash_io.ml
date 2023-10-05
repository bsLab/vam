
open Amoeba
open Name
open Stderr
open Stdcom
open Cap_env


let version = "0.61"


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


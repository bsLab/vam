
open Amoeba
open Stderr
open Stdcom
open Stdcom2

open Io
open Std_fun

type std_com = 
    | Noop of string list
    | Help of string list
    | Std_exit of string list
    | Std_info of string list
    | Std_status of string list


let to_command com args =
    match com with
    | "exit" -> Std_exit args;
    | "info" -> Std_info args;
    | "status" -> Std_status args;
    | _ -> raise (Error std_COMBAD)



let help = "
usage: std <command> <path> [<path2>...]
       commands: info, status, exit
       path: /unix -> UNIX, else Amoeba DNS
"

let usage err =
    out help; nl ();
    raise (Error err)
    

let exec_com com =
  try
  begin
    match com with

    | Help args ->
    begin
        out help; nl ();
        std_OK
    end;

    | Std_exit args -> f_exit args;
    | Std_info args -> f_info args;
    | Std_status args -> f_status args;
    | _ -> std_COMBAD
  end
  with
    | Error err -> err


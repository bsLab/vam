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
**    $CREATED:     11.12.2005
**    $VERSION:     1.01
**
**    $INFO:
**
**  Print a Process Descriptor read from a file
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

open Vpd_proc

let help = "
usage: vpd [-h -v] <filepath>
"
let out = print_string
let nl = print_newline

let usage err =
    out help; nl ();
    raise (Error err)


let main argv = 
    let prog = ref "" in
    let verbose = ref 0 in
    let rec eval li =
            match li with
            | hd::tl ->
            begin
                match hd with
                | "-h" -> usage std_OK;
                | "-v" -> incr verbose; eval tl;
                | _ -> 
                begin
                    if (!prog = "") then
                        prog := hd; 
                    eval tl;
                end;
            end;
            | [] -> ();
            in

    eval argv;
    if (!prog = "") then
        usage std_ARGBAD;
    let stat,pd = Vpd_proc.pd_read !prog !verbose in
    if stat = std_OK then
        print_pd pd
    else
        usage stat


let _ =
    try
        main (List.tl (Array.to_list Sys.argv))
    with
        | Error err -> if err <> std_OK then 
                        out ("failed: "^(err_why err)); nl();


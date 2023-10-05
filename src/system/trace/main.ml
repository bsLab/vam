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
**    $CREATED:     ??
**    $VERSION:     1.01
**
**    $INFO:
**
**  Thread and FLIP network trace logging frontend.
**
**    $ENDOFINFO
**
*)

open Amoeba
open Unix
open Io
open Trace
open Stderr

let _ =
    try
    begin
        let args = Sys.argv in
        let nargs = Array.length args in
       
        if nargs < 4 then usage std_ARGBAD;

        let server = args.(1) in
        let ttype = args.(2) in
        let coma = args.(3) in  
        let comargs = Array.sub args 4 (nargs - 4) in 

        let com = to_command coma comargs in
        let stat = exec_com server ttype com in
        if (stat <> std_OK) then
            raise (Error stat); 
    end
    with
        | Error err -> out ("failed: "^(err_why err)); nl()

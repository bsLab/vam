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
**    $CREATED:     ??
**    $VERSION:     1.01
**
**    $INFO:
**
** VAM shell main module.
**
**    $ENDOFINFO
**
*)


open Vash
open Vash_io
open Vash_env
open Vash_dir
open Vash_file
open Vash_std
open Vash_ksys
open Vash_exec
open Vash_tty


let usage_str = "
Program Arguments:
  -h : Print this help message.
  -V : Print the server version
  -t : Start internal TTY server [default server cap "^(!tty_path)^"]
"

let usage () =
    out usage_str;
    nl ();
    Pervasives.exit (-1)

let _ =
    let withtty = ref false in
    let args = Array.to_list (Sys.argv) in
    let rec iter al =
        match al with
        | hd::tl ->
        begin
            match hd with 
            | "-h" | "-help" -> usage ();
            | "-V" -> print_version (); Pervasives.exit (0);
            | "-t" -> withtty := true; iter tl;
            | _ -> iter tl;
        end;
        | [] -> ()
    in
    iter args;
    if (!withtty = true) then
        start_tty ();

    ignore(enter ());

    nl ();

    if (!withtty = true) then
    begin
        exit_tty ();
    end

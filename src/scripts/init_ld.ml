/*
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
**    $INITIAL:     (C) 2003 BSSLAB
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.01
**
**    $INFO:
**
** Init VAMsys.
** You can add own modules here.
**
** For example:
**          #load "mymod.cma" ;;
**          open Mymod;;
**
**
**    $ENDOFINFO
**
*/





open Os
open Topdirs;;
open Printf;;

let ppf = Format.std_formatter ;;

(*
** Grabage collector
*)
open Gc;;

let gcc = Gc.get () 
let _ = gcc.max_overhead <- 100;
Gc.set gcc

(*
** Concurrent programming
*)

open Thread;;
open Mutex;;
open Sema;;


(*
** Available modules
*)

type vam_modules = 
    M_Amoeba    |
    M_Unix      |
    M_Xlib      |
    M_WXlib 
;;

print_newline ();;
printf "=> Standard modules: [M_Amoeba,M_Unix,M_Xlib,M_WXlib]";;
print_newline ();;
printf "=> Use 'ld <mod>' to load a standard module.";;
print_newline ();print_newline ();;


(*
** Open modules by default after the module group was loaded
*)

(*
** M_Amoeba 
*)

let amoeba_open = [
        "Des48";
        "Amoeba";
        "Rpc";
        "Cmdreg";
        "Stderr";
        "Stdcom";
        "Capset";
        "Buf";
        "Ar";
        "Soap";
        "Dir";
        "Name";
        "Circbuf";
        "Virtcirc";
];;

let m_unix = ref false ;;
let m_amoeba = ref false ;;
let m_xlib = ref false ;;
let m_wxlib = ref false ;;


(*
** Load a module group.
*)

let rec ld modname =
    let phrase pstr =
        let lb = Lexing.from_string pstr in
        let phr = Parse.toplevel_phrase lb in 
        ignore(Toploop.execute_phrase false ppf phr)
    in
    
    let rec open_mod nl =
        match nl with
        | hd::tl -> ();
                    phrase ("open "^hd^";;");
                    open_mod tl;
        | [] -> ();
    in

    

    match modname with
    | M_Amoeba  ->
        if (!m_amoeba = false) then
        begin
            dir_load ppf "amoeba.cma";
            dir_load ppf "amoeba_help.cma";
            open_mod amoeba_open;
            m_amoeba := true;
        end;
    | M_Unix    ->
        if (!m_unix = false) then
        begin
            dir_load ppf "unix.cma";
            phrase "open Unix;;";
            m_unix := true;
        end;
    | M_Xlib    ->
        if (!m_xlib = false) then
        begin
            (
                match Os.vm_os with
                | OS_Unix ->    if (!m_unix = false) then
                                    ld M_Unix;
                | OS_Amoeba ->  if (!m_amoeba = false) then
                                    ld M_Amoeba;
            );

            dir_load ppf "xlib.cma";

            (
                match Os.vm_os with
                | OS_Unix ->    dir_load ppf "xos_unix.cma";
                | OS_Amoeba ->  dir_load ppf "xos_amoeba.cma";
            );

            m_xlib := true;
        end;
    | M_WXlib   ->
        if (!m_xlib = false) then
            ld M_Xlib;

        if (!m_wxlib = false) then
        begin
            dir_load ppf "wxlib.cma";
            m_wxlib := true;
        end;
;;




(*
** Help system
*)

open Help;;
open Thread_help;;

   
(*
** Install pretty printers
*)

(*
#install_printer print_amoeba_status;;
#install_printer print_amoeba_port;;
#install_printer print_amoeba_priv;;
#install_printer print_amoeba_cap;;
#install_printer print_amoeba_cb;;
#install_printer print_amoeba_buf;;
#install_printer print_amoeba_sema;;
#install_printer print_amoeba_vc;;
*)

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
**    $CREATED:     4.7.2005
**    $VERSION:     1.01
**
**    $INFO:
**
**  Server interface for the distributed tuple space
**  ML-LINDA. Fully compatible to HP-Linda, the C implementation of
**  the tuple space system LINDA. 
**
**    $ENDOFINFO
**
*)


open Amoeba
open Cmdreg
open Bytebuf
open Stderr
open Thread
open Linda_common
open Printf

type lk = linda_data_kind
type lt = linda_data_type

external linda_server_init : string -> status
    = "ext_linda_server_init"

external linda_server : unit -> unit 
    = "ext_linda_server"

let server_threads = 8

let info str =
    print_string ("LINDA: "^str);
    print_newline ()

let init spacename =
  let stat = ref std_OK in
  protect (
    let err = linda_server_init spacename in
    if err <> std_OK then
    begin
        info (sprintf "linda_server_inti failed: %s" (err_why err));
        stat := err;
        raise Exit;        
    end;

    (*
    ** Start some server threads
    *)
    
    info (sprintf "starting %d server threads for tuple space <%s>"
                  server_threads spacename);

    for i = 1 to server_threads
    do
        __(thread_create linda_server ());
    done;
  ); !stat

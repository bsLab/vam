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
**    $INITIAL:     (C) 2004
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.01
**
**    $INFO:
**
**  Official Amoeba RPC server test program for VAM
**
**    $ENDOFINFO
**
*)

open Amoeba
open Vrpc_test
open Printf
open Cap_env
open Thread
open Sema

let usage_str = "
vrpc_server [options] <port> <bufsize> <servicetime>

Program Arguments:
  <port>: The named tes port, for example MyTest
  <bufsize> : Size of transaction buffer
  <servicetime> : servicetime withinone transaction in milli seconds

  -h : Print this help message
  -v : Verbose mode
"

let usage str =
    print_string (str^"\n"^usage_str);
    print_newline ();
    exit (-1)

let nl = print_newline 
let info str =
    print_string ("VRPC_SERVER: "^str); nl ()

let verbose = ref false
let reverse = ref false
let portname = ref ""
let size = ref (-1) 
let cnt  = ref (-1)
let sema = sema_create 0

let _ =
    (* make sure we got our Amoeba root cap ! *)
    let _,rootcap = get_env_cap "ROOTCAP" in
    (*
    ** Scan program arguments...
    *)
    let args = Array.to_list (Sys.argv) in
    let rec iter al =
        match al with
        | hd::tl ->
        begin
            match hd with 
            | "-h" | "-help" -> usage "";
            | "-v" -> verbose := true; iter tl;
            | "-r" -> reverse := true;
            | _ -> if !portname = "" then portname := hd
                   else if !size = (-1) then size := int_of_string hd
                   else if !cnt = (-1) then cnt := int_of_string hd
                   else usage "invalid argument";
                   iter tl;
        end;
        | [] -> ()
    in
    iter (List.tl args);
    if !portname = "" ||
       !size = (-1) ||
       !cnt = (-1) then usage "to few arguments";

    info "Starting 2 server threads...";

    try
    begin
        ignore(thread_create (fun () -> 
            try
            begin
                ignore(rpc_server !portname
                                     !size
                                     !cnt);
                                     
                info "Server thread exit";
                sema_up sema;
            end
            with
                | Test_failure str ->
                        print_string str;
                        print_newline ();
                        exit 1;
                ) ());

        ignore(rpc_server !portname
                          !size
                          !cnt);
        info "Server thread exit.";
        sema_up sema;
        for i = 1 to 2 
        do
            sema_down sema;
        done;
    end
    with | Test_failure str ->
           print_string str;
           print_newline ();
           exit 1
 

                                               


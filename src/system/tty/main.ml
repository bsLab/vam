
let version = "0.90"

open Amoeba
open Bytebuf
open Stderr
open Stdcom
open Thread
open Buf
open Unix
open Printf
open Name
open Io
open Tty

let nthr = ref 4 
let capfile = ref "/server/tty"
let server_version = "0.90"

let print_version () =
    print_string ("TTY: terminal server "^
                    (server_version));
    print_newline ();
    print_string          "     Copyright: BSS-LAB, Stefan Bosse (2003)";
    print_newline ();
    exit 0

let usage_str = "
Program Arguments:
  -h : Print this help message.
  -V : Print the server version
"

let usage () =
    out usage_str;
    nl ();
    exit (-1)

let start () =
    let getport = uniqport () in
    let putport = priv2pub getport in
    let checkfield = uniqport () in

    let srv = {
        tty_getport = getport;
        tty_putport = putport;
        tty_checkfield = checkfield;
    } in

    let pubcap = {cap_port = putport;
                  cap_priv = prv_encode ~obj:(Objnum 0)
                                        ~rights:prv_all_rights    
                                        ~rand:checkfield
    } in
    let stat,cap = name_lookup !capfile in
    if (stat = std_OK) then
    begin
        let stat = name_delete !capfile in
        if (stat <> std_OK) then
        begin
            out ("TTY: can't delete old tty cap: "^(err_why stat));
            nl ();
            raise (Error stat);
        end;
    end;
    let stat = name_append !capfile pubcap in
    if (stat <> std_OK) then
    begin
        out ("TTY: can't append old tty cap: "^(err_why stat));
        nl ();
        raise (Error stat);
    end;
    

    let sema = Sema.sema_create 0 in
    print_string ("TTY: starting "^(string_of_int !nthr)^" server threads...");
    print_newline () ;

    for i = 1 to !nthr
    do
        ignore(thread_create (fun () -> 
                tty_srv ~server:srv
                    ~sema:sema
                    ~nthreads:!nthr
                    ~inbuf_size:tty_REQBUFSZ
                    ~outbuf_size:tty_REQBUFSZ
               ) ());
    done;

    print_string "TTY: Ready>>";
    print_newline ();
    for i = 1 to !nthr
    do
        Sema.sema_down sema;
    done;

    print_string "TTY: <<Exit";
    print_newline ()
    

let _ =
    let args = Array.to_list (Sys.argv) in
    let rec iter al =
        match al with
        | hd::tl ->
        begin
            match hd with 
            | "-h" | "-help" -> usage ();
            | "-V" -> print_version ();
            | "-C" -> 
            begin
                match tl with
                | hd::tl -> 
                begin
                    capfile := hd;
                    iter tl;
                end;
                | [] -> usage ();
            end;
            | "-t" -> 
            begin
                match tl with
                | hd::tl -> 
                begin
                    nthr := (int_of_string hd);
                    iter tl;
                end;
                | [] -> usage ();
            end;
            | _ -> iter tl;
        end;
        | [] -> ()
    in
    iter args;
    start ()

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
**      BSSLAB, Dr. Stefan Bosse www.bsslab.de
**
**    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF: 
**    Free Software Foundation-Europe, GNU GPL License, Version 2
**
**    $MODIFIEDBY:  BSSLAB
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C)
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.01
**
**    $INFO:
**
** Simple background debugger with RPC interface. A unique server
** capability in string format (8 chares) is used for public access. 
** This random generated port is printed to the stdout channel.
** Anyone who can read the message can connect to the internal debugger!
**
**
**    $ENDOFINFO
**
*)

open Amoeba
open Stderr
open Stdcom
open Rpc
open Thread
open Printf
open Bytebuf
open Buf
open Stdcom

let debug_SIZE = 30000

/*
** Start the background debugger with a different thread environment
** executing the RPC service loop.
*/

let inited = ref false

let init_debugger () =
    if !inited = false then
    begin
        inited := true;
        let portname = String.create 10 in
        let chars = [| 'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';
                   'm';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z';
                   'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';
                   'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';
                   '0';'1';'2';'3';'4';'5';'6';'7';'8';'9'|] in
        let nchars = Array.length chars in
        for i = 0 to 7
        do
            portname.[i] <- chars.(((Random.bits ()) mod nchars));
        done;

        let priv_port = Port portname in
        print_string ("DEBUGGER: Portname=\""^portname^"\"");
        print_newline ();

        ignore(thread_create (fun () ->
            print_string ("DEBUGGER: service thread started...");
            print_newline ();
            let hdr = header_new () in
            let buf = buf_create debug_SIZE in
            let exit = ref false in
            while (!exit = false)
            do
                let stat,n',hdr = getreq (priv_port,buf,debug_SIZE) in
                if stat <> std_OK then raise (Error stat);

                hdr.h_size <- 0;
                hdr.h_status <- std_OK;

                (match hdr.h_command with
                    | com when com = std_INFO ->
                    begin
                        let str = sprintf "DEBUGGER" in
                        let pos = buf_put_string buf 0 str in
                        hdr.h_size <- pos;
                    end;
                    | com when com = std_STATUS ->
                    begin
                        (*
                        ** Simply print the thread stack traces...
                        *)
                        let str = thread_debuginfo false in
                        let pos = buf_put_string buf 0 str in
                        hdr.h_size <- pos;
                    end;
                    | com when com = std_EXIT ->
                    begin
                        exit := true;
                    end;
                    | _ ->
                        hdr.h_status <- std_COMBAD;
                );
                ignore(putrep (hdr,buf,hdr.h_size));
            done; 
            ) 
            () );
    end
    

(*
** Client interface
*)

let debug_trace portname =
    let priv_port = Port portname in
    let pub_port  = priv2pub priv_port in
    let cap = {cap_port = pub_port;cap_priv=nilpriv} in
    let stat,str = std_status cap debug_SIZE in
    if (stat = std_OK) then
    begin
        print_string ("Thread trace for process "^portname^"\n"^str);
        print_newline ();
    end;
    stat


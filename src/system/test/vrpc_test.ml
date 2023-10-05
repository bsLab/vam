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
**    $AUTHORS:     
**    $INITIAL:     (C)
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.01
**
**    $INFO:
**
**  Official RPC performance test utils
**
**    $ENDOFINFO
**
*)


open Amoeba
open Stderr 
open Stdcom 
open Rpc  
open Bytebuf 
open Unix
open Printf
open Signals
open Thread 

let port str = Port (let p = String.create port_SIZE in 
                        let n = min port_SIZE (String.length str) in
                        for i = 0 to n-1
                        do
                            p.[i] <- str.[i];
                        done; p) 

(*
** RPC Client
*)
exception Test_failure of string

let rpc_client ~portname ~bufsize ~cnt ~reverse = 
    (*
    ** Private and public ports
    *)
    let priv_port = port portname in
    let pub_port  = priv2pub priv_port in
    let size = bufsize in
    let gsize = bufsize in

    let buf = if size > 0 
                then buf_create size 
                else nilbuf in
    let hdr = header_new () in
    let n = ref (
        if cnt = 0 then
        begin
            (*
            ** Shutdown test server...
            *)
            hdr.h_extra <- 1; 
            2; 
        end
        else
            cnt)
        in
    timeout 1000;
    let start = time () in
    
    while (!n > 0)
    do
        hdr.h_port <- pub_port;
        let stat,n',hdr' = trans (hdr,buf,size,buf,gsize) in
        if stat <> std_OK then
            raise (Test_failure 
                    (sprintf "transaction to rpc server failed: %s"
                              (err_why stat)));
        decr n;
    done;

    let finish = (time ()) -. start in

    if cnt <> 0 then
    begin
        let finish = int_of_float finish in
        sprintf "transfered %d bytes in %d seconds\n%s%s"
                (gsize * cnt) finish
                (if finish <> 0 then
                    sprintf "transfer rate = %d bytes/second\n"
                            (gsize * cnt / finish)
                    else "")
                (if finish <> 0 then
                    sprintf "response time = %d microsecs\n" 
                            (1000000 * finish / cnt)
                    else ""); 
    end
    else
        "Done."

(*
** RPC server
*)

let rpc_server ~portname ~bufsize ~servicetime =
    (*
    ** Private and public ports
    *)
    let priv_port = port portname in
    let pub_port  = priv2pub priv_port in

    if bufsize <= 0 then
        raise (Test_failure "invalid buffer size");

    let server () =
        sig_catch sig_TRANS (fun i ->
                Printf.printf "Got Transaction signal...";
                print_newline ();
            );
        let buf = buf_create bufsize in
        
        let exit = ref false in

        while (!exit = false)
        do
            let stat,n',hdr = getreq (priv_port,buf,bufsize) in
            if stat <> std_OK then
                raise (Test_failure (sprintf "getreq returned with %s\n"
                                            (err_why stat)));
            if servicetime > 0 then
                ignore(thread_delay servicetime MILLISEC);

            if hdr.h_extra = 1 then exit := true;
            ignore(putrep (hdr,buf,hdr.h_offset));
        done; 
        in
    server ();
    "Done."
 
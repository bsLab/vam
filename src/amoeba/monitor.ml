(*
**  THIS SOFTWARE IS OWNED AND COPYRIGHTED BY
**
**    ###     ####   ####               #         ##         #####
**    #  #    #      #                 #         # #        #     #
**    #   #   #      #                #         #  #       #      #
**    #   #   #      #               #         #   #      #      #   
**    ####    ####   ####  ####     #         ######     ########
**    #   #      #      #          #         #     #    #      #
**    #   #      #      #         #         #      #   #       #
**    #  #       #      #        #         #       #  #       #
**    ###     ####   ####       ######### #        # #########
**
**    Stefan Bosse (c) 2003
**   
**  THIS SOFTWARE MAY NOT BE COPIED, EXTRACTED, MODIFIED, OR 
**  OTHERWISE USED IN A CONTEXT OUTSIDE OF THE VAM SYSTEM.
** 
*)

(*
** Server Event Monitoring Module
*)

open Amoeba
open Bytebuf
open Thread
open Rpc
open Stdcom
open Stderr
open Buf

(*
** All event strings are referenced by a circular
** cache with the 'event' function.
** A client can extract all events from the cache with the
** 'event_get' function. An event can only be read one time.
**
** Each module or the whole program can start a separate server thread 
** with the 'event_server' function together with the event structure, 
** previously created with the 'event_init' function (== 'event_start'). 
** The 'event_init' functions expect the number of cache entries and
** the portname string, which
** is converted in a private port. This port name must also be
** given to the 'event_get' function.
**
*)

let mod_monitor_ver = 0.4


(*
** Assuming 100 bytes for each event string, all event strings
** together must have a s size below the old RPC limit of
** approximate 30000 bytes, else a client request will be truncated.
** Therefore, a size of 300 entries should be sufficient.
*)
 
let nilref = ref "" 
let myevents_size = ref 300
let myevents = Array.create !myevents_size nilref
let myevents_head = ref 0 
let myevents_tail = ref 0
let myevents_lock = mu_create ()


let event evstr =
    mu_lock myevents_lock;
    let str = ref evstr in
    myevents.(!myevents_head) <- str;

    incr myevents_head;
    if (!myevents_head = !myevents_size) then
    begin
        myevents_head := 0;
        if (!myevents_head = !myevents_tail) then
        begin
            incr myevents_tail;
            if (!myevents_tail = !myevents_size) then
            begin
                myevents_tail := 0;
            end;
        end; 
    end;

    mu_unlock myevents_lock
    

let event_server portname =

  let doit () =
    let server_port = port_of_str portname in
    let repbuf = buf_create max_TRANS in
    let repsiz = ref 0 in
    let rephdr = header_new () in
    let dying = ref false in

    while (!dying = false)
    do
        repsiz := 0;
        let err,n,reqhdr = getreq (server_port,nilbuf,0) in

        if (err = std_OK) then
        begin
            match reqhdr.h_command with 

            | com when (com = std_INFO) ->
            begin
                let p = buf_put_string repbuf 0 "Monitor Event Server" in
                repsiz := p;
                rephdr.h_status <- std_OK;
                rephdr.h_size <- p;
            end;

            | com when (com = std_STATUS) ->
            begin
                let evn = if !myevents_head < !myevents_tail then
                            !myevents_size
                          else
                            !myevents_head
                in
                let p = buf_put_string repbuf 0 
                            ("Monitor Event Server. Stored events:"^
                             (string_of_int evn)) in
                repsiz := p;
                rephdr.h_status <- std_OK;
                rephdr.h_size <- p;
            end;
            | com when (com = std_MONITOR) ->
            begin
                mu_lock myevents_lock;
                
                let p = ref 0 in

                (
                  try
                    if (!myevents_head > !myevents_tail) then
                    begin
                      for i = !myevents_tail to !myevents_head - 1
                      do
                        let p' = buf_put_string repbuf 
                                            !p
                                            !(myevents.(i))
                        in
                        p := p';
                        incr myevents_tail;
                      done
                    end
                    else if (!myevents_head <> !myevents_tail) then
                    begin
                      for i = !myevents_tail to !myevents_size - 1
                      do
                        let p' = buf_put_string repbuf 
                                            !p
                                            !(myevents.(i))
                        in
                        p := p';
                        incr myevents_tail;
                      done;
                      if (!myevents_head > 0) then
                      begin
                          myevents_tail := 0;
                          for i = 0 to !myevents_head - 1
                          do
                            let p' = buf_put_string repbuf 
                                                !p
                                                !(myevents.(i))
                            in
                            p := p';
                            incr myevents_tail;
                          done;
                      end;
                    end;
                  with
                    | Buf_overflow -> ();
                );              
                repsiz := !p;
                rephdr.h_status <- std_OK;
                rephdr.h_size <- !p;
                mu_unlock myevents_lock;
            end;

            | com when (com = std_EXIT) ->
            begin
                repsiz := 0;
                rephdr.h_status <- std_OK;
                dying := true;
            end;

            | _ -> 
            begin
                rephdr.h_status <- std_COMBAD;
            end;
        end;

        ignore(putrep (rephdr,repbuf,!repsiz));
    
    done
  in
  ignore(thread_create (fun _ -> doit ()) ())
    
let event_start () = 
    let whoami = Filename.basename 
                    (Sys.argv.(0))
                    in
    let portname = try
                    Filename.chop_extension whoami
                   with
                    | Invalid_argument _ -> whoami
                    in

    Db.Pr.ss 1 "event_start" portname;
    ignore(thread_create event_server portname)
        

let event_get portname =
    let server_port = port_of_str portname in
    let public_port = priv2pub server_port in
    let repbuf = buf_create max_TRANS in
    let reqhdr = header_new () in

    reqhdr.h_command <- std_MONITOR;
    reqhdr.h_port <- public_port;
        

    let err,n,rephdr = trans (reqhdr,nilbuf,0,
                              repbuf,max_TRANS) 
    in

    let mon = ref "" in
    
    if (err = std_OK && rephdr.h_status = std_OK) then
    begin
        let alldone = ref false in
        let p = ref 0 in

        while(!alldone = false) 
        do
            let p',s = buf_get_string repbuf !p in
            mon := !mon ^ s ^ "\n";        
            p := p';
            if (p' >= n) then
                alldone := true;
        done;
    end;
    (if err <> std_OK then err else rephdr.h_status),!mon


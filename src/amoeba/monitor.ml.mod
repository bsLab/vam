(*
** Server Event Monitoring Module
*)

open Amoeba
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

let mod_monitor_ver = 0.2


(*
** Assuming 100 bytes for each event string, all event strings
** together must have a s size below the old RPC limit of
** approximate 30000 bytes, else a client request will be truncated.
** Therefore, a maximal size of 300 entries should be sufficient.
*)
 
let nilref = ref "" 

type mon_event = {
    mutable events_name : string;
    mutable events_size : int;
    mutable events : (string ref) array;
    mutable events_head : int;
    mutable events_tail : int;
    mutable events_lock : Mutex.t; 
}

let event_init num name = 
    {
        events_name = name;
        events_size = num;
        events =  Array.create num nilref;
        events_head = 0;
        events_tail = 0;
        events_lock = mu_create ()
    }

let event ev evstr =
    mu_lock ev.events_lock;
    let str = ref evstr in
    ev.events.(ev.events_head) <- str;

    ev.events_head <- ev.events_head + 1;
    if (ev.events_head = ev.events_size) then
    begin
        ev.events_head <- 0;
        if (ev.events_head = ev.events_tail) then
        begin
            ev.events_tail <- ev.events_tail + 1;
            if (ev.events_tail = ev.events_size) then
            begin
                ev.events_tail <- 0;
            end;
        end; 
    end;

    mu_unlock ev.events_lock
    

let event_server ev =

  let doit () =
    let server_port = port_of_str ev.events_name in
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
                let evn = if ev.events_head < ev.events_tail then
                            ev.events_size
                          else
                            ev.events_head
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
                mu_lock ev.events_lock;
                
                let p = ref 0 in

                (
                  try
                    if (ev.events_head > ev.events_tail) then
                    begin
                      for i = ev.events_tail to ev.events_head - 1
                      do
                        let p' = buf_put_string repbuf 
                                            !p
                                            !(ev.events.(i))
                        in
                        p := p';
                        ev.events_tail <- ev.events_tail + 1;
                      done
                    end
                    else if (ev.events_head <> ev.events_tail) then
                    begin
                      for i = ev.events_tail to ev.events_size - 1
                      do
                        let p' = buf_put_string repbuf 
                                            !p
                                            !(ev.events.(i))
                        in
                        p := p';
                        ev.events_tail <- ev.events_tail + 1;
                      done;
                      if (ev.events_head > 0) then
                      begin
                          ev.events_tail <- 0;
                          for i = 0 to ev.events_head - 1
                          do
                            let p' = buf_put_string repbuf 
                                                !p
                                                !(ev.events.(i))
                            in
                            p := p';
                            ev.events_tail <- ev.events_tail + 1;
                          done;
                      end;
                    end;
                  with
                    | Buf_overflow -> ();
                );              
                repsiz := !p;
                rephdr.h_status <- std_OK;
                rephdr.h_size <- !p;
                mu_unlock ev.events_lock;
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
    
    
let event_start num name =
    let ev = event_init num name in
    event_server ev;
    ev

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


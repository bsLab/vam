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
**    $VERSION:     0.52
**
**    $INFO:
**
** Simple terminal server.
**
**
**    $ENDOFINFO
**
*)




open Amoeba
open Stderr
open Stdcom
open Stdcom2
open Cmdreg
open Rpc
open Thread
open Ar
open Bytebuf
open Buf
open Name

open Vash_io
open Cap_env


let tty_path = ref "/server/tty"
let tty_sema = Sema.sema_create 0 
let tty_nthr = ref 4 
let tty_cap = ref nilcap

let tty_READ    = Command (tty_FIRST_COM+52)
let tty_WRITE   = Command (tty_FIRST_COM+53)


let tty_REQBUFSZ = 1024

type tty_server = {
    mutable tty_getport: port;
    mutable tty_putport: port;
    mutable tty_checkfield: port;
}

let tty_dying = ref false

let tty_write ~buf ~size =
    let str = String.create size in
    blit_bs ~src:buf ~src_pos:0
            ~dst:str ~dst_pos:0
            ~len:size;
    out str; 
    flush !stdout;
    flush !stderr;
    std_OK

let tty_srv ~server
            ~sema 
            ~nthreads
            ~inbuf_size
            ~outbuf_size
    =
    tty_dying := false;
    let initial = ref false in
    let on_exit cap =
            let stat = if (!tty_dying = false) then
                       begin
                            tty_dying := true;
                            (*
                            ** Tell the other threads we're
                            ** dying.
                            *)
                            for i = 1 to (nthreads-1)
                            do
                                ignore(std_exit cap);     
                                Sema.sema_up sema;
                            done;
                            std_OK
                       end
                       else
                            std_OK
                       in
            stat
    in        
    try
    begin
        let ibuf = buf_create inbuf_size in     (* request buffer *)
        let obuf = buf_create outbuf_size in    (* reply buffer   *)
        
        let hdr_rep = header_new () in
        let replen = ref 0 in
        
        let getport = server.tty_getport in
        let putport = server.tty_putport in
        let checkfield = server.tty_checkfield in

        while (true)
        do
        try
          begin
                        
#ifdef DEBUG
            Db.Pr.ss 1 "TTY" "getreq";
#endif
            let stat,reqlen,hdr_req = getreq (getport,ibuf,inbuf_size) in

            replen := 0;
            hdr_rep.h_size <- 0;
            hdr_rep.h_status <- std_OK;
            hdr_rep.h_priv <- priv_copy nilpriv;


            let priv =hdr_req.h_priv in

            (
#ifdef DEBUG
                Db.Pr.ss 1 "TTY request" (ar_priv hdr_req.h_priv);
                Db.Pr.sd 1 "TTY command" (let Command com =
                                                hdr_req.h_command in com);
#endif

                match hdr_req.h_command with

                | com when (com = tty_WRITE) ->
                begin
                    let Objnum obj = prv_number hdr_req.h_priv in
                    let Rights_bits rights = prv_rights hdr_req.h_priv in


        
                    let stat = ref std_OK in
                    let pos = ref 0 in
                    let size = ref (min hdr_req.h_size inbuf_size) in
                        
                    if (prv_decode ~prv:priv ~rand:checkfield
                            = true) then
                    begin
                        stat := tty_write ~buf:ibuf
                                          ~size:!size;
                    end
                    else
                        stat := std_DENIED;

                    hdr_rep.h_size <- !size;
                    hdr_rep.h_status <- !stat;
                    replen := !pos;
                end;

                (*
                ** STD_INFO request
                *)

                | com when (com = std_INFO) ->
                begin
                    let Objnum obj = prv_number hdr_req.h_priv in
                    let Rights_bits rights = prv_rights hdr_req.h_priv in

        
                    let stat = ref std_OK in
                    let pos = ref 0 in
                    if (prv_decode ~prv:priv ~rand:checkfield
                            = true) then
                    begin
                            let pos' = 
                                buf_put_string 
                                    ~buf:obuf
                                    ~pos:0
                                    ~str:"+ (terminal)"
                            in
                            pos := pos';
                    end
                    else
                        stat := std_DENIED;

                    hdr_rep.h_size <- !pos;
                    hdr_rep.h_status <- !stat;
                    replen := !pos;
                end;

                (*
                ** STD_STATUS request
                *)

                | com when (com = std_STATUS) ->
                begin
                    let Objnum obj = prv_number hdr_req.h_priv in
                    let Rights_bits rights = prv_rights hdr_req.h_priv in
        
                    let stat = ref std_OK in
                    let pos = ref 0 in
                    if (prv_decode ~prv:priv ~rand:checkfield
                            = true) then
                    begin
                            let pos' = 
                                buf_put_string 
                                    ~buf:obuf
                                    ~pos:0
                                    ~str:"Terminal Ok (TODO)"
                            in
                            pos := pos';
                    end
                    else
                        stat := std_DENIED;

                    hdr_rep.h_size <- !pos;
                    hdr_rep.h_status <- !stat;
                    replen := !pos;
                end;

                (*
                ** Close the server thread.
                ** Only possible with the super capability.
                **
                *)
                | com when (com = std_EXIT) ->
                begin
                    initial := not (!tty_dying);
                    let rights = prv_rights hdr_req.h_priv in
                    let Objnum obj = prv_number hdr_req.h_priv in
                    if (obj = 0 &&
                       (prv_decode ~prv:hdr_req.h_priv 
                                   ~rand:checkfield)
                       ) = true then
                    begin 
                        let stat = on_exit { cap_port = hdr_req.h_port;
                                             cap_priv = priv} in

                        hdr_rep.h_status <- stat;
                        ignore( putrep (hdr_rep,obuf,!replen));
                        raise Exit;            
                    end
                    else
                    begin
                        hdr_rep.h_status <- std_DENIED;
                    end;
                end;

                | _ -> 
                begin
out "? on tty"; nl();
                    let Command com = hdr_req.h_command in
#if 1
                    Db.Pr.sd 1 "TTY std_COMBAD" com;
#endif

                    hdr_rep.h_status <- std_COMBAD;
                end;

            );
            
            ignore( putrep (hdr_rep,obuf,!replen));
#ifdef DEBUG
            Db.Pr.ss 1 "TTY reply" (err_why hdr_rep.h_status);
#endif
          end
          with
            | Buf_overflow ->
            begin
                replen := 0;
                hdr_rep.h_status <- std_ARGBAD;
                ignore( putrep (hdr_rep,obuf,!replen));
            end;
        done;
    end
    with
        | Exit -> 
                  out "TTY: Server thread exited normally."; 
                  nl ();
                  if (!initial) then
                    Sema.sema_up sema


let start_tty () =
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
    let stat,cap = name_lookup !tty_path in
    if (stat = std_OK) then
    begin
        let stat = name_delete !tty_path in
        if (stat <> std_OK) then
        begin
            out ("TTY: can't delete old tty cap: "^(err_why stat));
            nl ();
            raise (Error stat);
        end;
    end;
    let stat = name_append !tty_path pubcap in
    if (stat <> std_OK) then
    begin
        out ("TTY: can't append old tty cap: "^(err_why stat));
        nl ();
        raise (Error stat);
    end;
    


    for i = 1 to !tty_nthr
    do
        ignore(thread_create (fun () -> 
                tty_srv ~server:srv
                    ~sema:tty_sema
                    ~nthreads:!tty_nthr
                    ~inbuf_size:tty_REQBUFSZ
                    ~outbuf_size:tty_REQBUFSZ
               ) ());
    done;
    tty_cap := pubcap;
    ignore(put_env_cap "TTY" !tty_cap)

let exit_tty () =
    let stat = std_exit !tty_cap in
    if (stat <> std_OK) then
    begin
        out "Can't terminate terminal server TTY!"; nl ();
        raise (Error stat);
    end;
    for i = 1 to !tty_nthr
    do
        Sema.sema_down tty_sema;
    done;
    out "TTY server exited normally."; nl ()


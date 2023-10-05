(*
** Simple terminal server.
*)

open Amoeba
open Stderr
open Stdcom
open Stdcom2
open Cmdreg
open Rpc
open Thread
open Ar
open Io
open Bytebuf
open Buf

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
                    let Command com = hdr_req.h_command in
#ifdef DEBUG
                    Db.Pr.sd debug "TTY std_COMBAD" com;
#endif

                    hdr_rep.h_status <- std_COMBAD;
                end;

            );
#ifdef DEBUG
            Db.Pr.ss 1 "TTY reply" (err_why hdr_rep.h_status);
#endif
            
            ignore( putrep (hdr_rep,obuf,!replen));
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
                  print_string "TTY: Server thread exited normally."; 
                  print_newline ();
                  if (!initial) then
                    Sema.sema_up sema


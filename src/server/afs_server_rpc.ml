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
**    $INITIAL:     (C) BSSLAB 2003
**    $CREATED:     
**    $VERSION:     1.08
**
**    $INFO:
**
** AFS: Atomic Filesystem Service
** RPC server implementation.
** 
** Note: The required rights for an operation are resolved in the 
**       Afs_server module!
**
**
**    $ENDOFINFO
**
*)


open StdLabels

open Amoeba
open Bytebuf
open Cmdreg
open Stderr
open Stdcom
open Stdcom2
open Capset
open Sys
open Rpc
open Buf
open Mutex
open Dblist 
open Thread
open Printf
open Ar
open Syslog

open Afs_common
open Afs_server


let afs_dying = ref false 

(*
** The main RPC server loop.
**
** Args:
**  server: afs_server
**  sema: semaphore used to synchronosize the master server
**  nthreads: number of total server threads
**  inbuf_size: request buffer size (commonly afs_REQBUFSZ)
**  outbuf_size: reply buffer size  (commonly afs_REQBUFSZ)
**
*)

let server_loop ~server
                ~sema
                ~nthreads
                ~inbuf_size
                ~outbuf_size
    =
    afs_dying := false;
    let initial = ref false in
    let on_exit cap =
            let stat = if (!afs_dying = false) then
                       begin
                            afs_dying := true;
                            (*
                            ** Sync the disk.
                            *)
                            let stat' = server.afs_sync () in
                            
                            (*
                            ** Tell the other threads we're
                            ** dying.
                            *)
                            for i = 1 to (nthreads-1)
                            do
                                ignore(std_exit cap);     
                                Sema.sema_up sema;
                            done;
                            stat'
                       end
                       else
                            std_OK
                       in
            stat
    in        
    try
    begin
        let super = server.afs_super in

        let ibuf = buf_create inbuf_size in     (* request buffer *)
        let obuf = buf_create outbuf_size in    (* reply buffer   *)
        
        let hdr_rep = header_new () in
        let replen = ref 0 in
        
        let getport = super.afs_getport in
        let putport = super.afs_putport in

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
                Db.Pr.ss 1 "SERVER request" (ar_priv hdr_req.h_priv);
                Db.Pr.sd 1 "SERVER command" (let Command com =
                                                hdr_req.h_command in com);
#endif

                match hdr_req.h_command with

    
                (*
                ** AFS_SIZE request
                *)
                | com when (com = afs_SIZE) ->
                begin
                    let Objnum obj = prv_number hdr_req.h_priv in
                    let Rights_bits rights = prv_rights hdr_req.h_priv in
                    let stat,size = afs_req_size ~server:server
                                                 ~priv:priv
                                    in
                    hdr_rep.h_offset <- size;
                    hdr_rep.h_status <- stat;
                end;


                (*
                ** AFS_CREATE request.
                ** Required rights: afs_RGT_CREATE.
                **
                *)
                | com when (com = afs_CREATE) ->
                begin
                    let off = hdr_req.h_offset in
                    let flag = hdr_req.h_extra in

                    let size = hdr_req.h_size in
                    
                    let stat,newcap = afs_req_create ~server:server
                                                     ~priv:priv
                                                     ~buf:ibuf
                                                     ~size:size
                                                     ~commit:flag
                                      in

                    if (stat = std_OK) then
                    begin
#ifdef DEBUG
                        Db.Pr.ss 1 "afs_CREATE" (ar_cap newcap);
#endif
                        hdr_rep.h_priv <- newcap.cap_priv;
                        hdr_rep.h_port <- newcap.cap_port;
                    end;
                    hdr_rep.h_status <- stat;
                end;

                (*
                ** AFS_READ request
                ** Required rights: afs_RGT_READ.
                *)
                | com when (com = afs_READ) ->
                begin
                    let off = hdr_req.h_offset in
                    let size = hdr_req.h_size in
                    
                    let stat,size =
                            if (size > outbuf_size) then
                                    std_ARGBAD,0
                            else
                               afs_req_read ~server:server
                                            ~priv:priv
                                            ~buf:obuf
                                            ~off:off
                                            ~size:size
                                      in
                    hdr_rep.h_status <- stat;

                    if (stat = std_OK) then
                    begin
                        hdr_rep.h_priv <- hdr_req.h_priv;
                        hdr_rep.h_port <- putport;
                        hdr_rep.h_size <- size;
                        replen := size;
                    end;
                end;

                (*
                ** AFS_MODIFY request
                ** Required rights: afs_RGT_MODIFY.
                *)
                | com when (com = afs_MODIFY) ->
                begin
                    let off = hdr_req.h_offset in
                    let flag = hdr_req.h_extra in

                    let size = hdr_req.h_size in
                    
                    let stat,newcap = afs_req_modify ~server:server
                                                     ~priv:priv
                                                     ~buf:ibuf
                                                     ~off:off
                                                     ~size:size
                                                     ~commit:flag
                                      in

                    if (stat = std_OK) then
                    begin
                        hdr_rep.h_priv <- newcap.cap_priv;
                        hdr_rep.h_port <- newcap.cap_port;
                    end;
                    hdr_rep.h_status <- stat;

                end;

                (*
                ** AFS_INSERT request
                ** Required rights: afs_RGT_MODIFY.
                *)
                | com when (com = afs_INSERT) ->
                begin
                    let off = hdr_req.h_offset in
                    let flag = hdr_req.h_extra in

                    let size = hdr_req.h_size in
                    
                    let stat,newcap = afs_req_insert ~server:server
                                                     ~priv:priv
                                                     ~buf:ibuf
                                                     ~off:off
                                                     ~size:size
                                                     ~commit:flag
                                      in

                    if (stat = std_OK) then
                    begin
                        hdr_rep.h_priv <- newcap.cap_priv;
                        hdr_rep.h_port <- newcap.cap_port;
                    end;
                    hdr_rep.h_status <- stat;


                end;


                (*
                ** AFS_DELETE request
                ** Required rights: afs_RGT_MODIFY.
                *)
                | com when (com = afs_DELETE) ->
                begin
                    let off = hdr_req.h_offset in
                    let flag = hdr_req.h_extra in

                    let pos,size = buf_get_int32 ~buf:ibuf
                                               ~pos:0
                                   in
                    
                    let stat,newcap = afs_req_delete ~server:server
                                                     ~priv:priv
                                                     ~off:off
                                                     ~size:size
                                                     ~commit:flag
                                      in

                    if (stat = std_OK) then
                    begin
                        hdr_rep.h_priv <- newcap.cap_priv;
                        hdr_rep.h_port <- newcap.cap_port;
                    end;
                    hdr_rep.h_status <- stat;


                end;

                (*
                ** AFS_SYNC request
                *)
                | com when (com = afs_SYNC) ->
                begin
                    let Objnum obj = prv_number hdr_req.h_priv in
                    let Rights_bits rights = prv_rights hdr_req.h_priv in
                    let stat = afs_req_sync server priv in
                    hdr_rep.h_size <- 0;
                    hdr_rep.h_status <- stat;
                    replen := 0;

                end;

                (*
                ** AFS_DESTROY=STD_DESTROY request
                ** Required rights: afs_RGT_DESTROY.
                *)
                | com when (com = std_DESTROY || com = afs_DESTROY) -> 
                begin
                    
                    let stat = 
                               afs_req_destroy 
                                            ~server:server
                                            ~priv:priv
                                      in
                    hdr_rep.h_status <- stat;
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

                    if (obj > 0) then
                    begin
                        let stat',af = acquire_file ~server:server 
                                                    ~obj:obj
                        in
    
                        if (stat' = std_OK) then
                        begin
                            (*
                            ** Validate the private field.
                            *)

                            if (prv_decode ~prv:priv ~rand:af.ff_random
                                = true) then
                            begin
                                let istr = sprintf "- %8d bytes"
                                           af.ff_size
                                in
                                let pos' = buf_put_string 
                                            ~buf:obuf
                                            ~pos:0
                                            ~str:istr
                                in
                                pos := pos';

                            end
                            else
                                stat := std_DENIED;
                        ignore(release_file ~server:server
                                            ~file:af
                                            ~flag:0);
                        end
                        else
                            stat := stat';
                    end
                    else
                    begin
                        (*
                        ** Super Capability.
                        ** Validate the private field.
                        *)

                        if (prv_decode ~prv:priv ~rand:super.afs_checkfield
                            = true) then
                        begin
                            let pos' = 
                                buf_put_string 
                                    ~buf:obuf
                                    ~pos:0
                                    ~str:"Atomic Filesystem Server: Super Cap"
                            in
                            pos := pos';
                        end
                        else
                            stat := std_DENIED;
                    end;

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
        
                    let stat = ref std_OK in
                    let pos = ref 0 in

                    if (obj > 0) then
                    begin
                        let stat',af = acquire_file ~server:server 
                                                    ~obj:obj
                        in
    
                        if (stat' = std_OK) then
                        begin
                            (*
                            ** Validate the private field.
                            *)

                            if (prv_decode ~prv:priv ~rand:af.ff_random
                                = true) then
                            begin
                                let stat',bstr = server.afs_stat obj in
                                stat := stat';
                                let pos' = buf_put_string 
                                        ~buf:obuf
                                        ~pos:0
                                        ~str:bstr
                                in
                                pos := pos';

                            end
                            else
                                stat := std_DENIED;

                            ignore(release_file ~server:server
                                            ~file:af
                                            ~flag:0);
                        end
                        else
                            stat := stat';
                    end
                    else
                    begin
                        (*
                        ** Super Capability.
                        ** Validate the private field.
                        *)

                        if (prv_decode ~prv:priv ~rand:super.afs_checkfield
                            = true) then
                        begin
                            let stat',bstr = server.afs_stat 0 in
                            stat := stat';
                            let pos' = buf_put_string 
                                        ~buf:obuf
                                        ~pos:0
                                        ~str:bstr
                            in
                            pos := pos';
                        end
                        else
                            stat := std_DENIED;
                    end;

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
                    initial := not (!afs_dying);
                    let rights = prv_rights hdr_req.h_priv in
                    let Objnum obj = prv_number hdr_req.h_priv in
                    if (obj = 0 &&
                       (prv_decode ~prv:hdr_req.h_priv 
                                   ~rand:super.afs_checkfield)
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

                | com when (com = std_TOUCH) ->
                begin
                    let stat = afs_req_touch ~server:server 
                                             ~priv:hdr_req.h_priv
                    in
                    hdr_rep.h_status <- stat;
                end;        

                | com when (com = std_AGE) ->
                begin
                    let stat = afs_req_age   ~server:server 
                                             ~priv:hdr_req.h_priv
                    in
                    hdr_rep.h_status <- stat;
                end;        


                | _ -> 
                begin
                    let Command com = hdr_req.h_command in
#ifdef DEBUG
                    Db.Pr.sd debug "afs_server std_COMBAD" com;
#endif

                    hdr_rep.h_status <- std_COMBAD;
                end;

            );
#ifdef DEBUG
                Db.Pr.ss 1 "SERVER reply" (err_why hdr_rep.h_status);
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
                  sys_log Sys_info "AFS: Server thread exited normally.\n"; 
                  if (!initial) then
                    Sema.sema_up sema;
            

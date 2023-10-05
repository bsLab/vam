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
**    $INITIAL:     (C) 2004 BSSLAB
**    $CREATED:
**    $VERSION:     1.03
**    $INFO:
**
**  Default RPC server loop for the virtual disk server.
**
**    $ENDOFINFO
**
*)

open Amoeba
open Machtype
open Disk_common
open Disk_client
open Disk_server
open Stderr     
open Ar
open Stdcom
open Bytebuf
open Printf 
open Machtype
open Buf
open Rpc
open Syslog

let disk_dying = ref false 


(*
** The server loop. The input request and output reply buffer sizes must be
** specified.
**  disk_table: The virtual disk array
**  sema: semaphore used to synchronosize the master server
**  nthreads: number of total server threads
**  inbuf_size: request buffer size (commonly disk_REQBUFSZ)
**  outbuf_size: reply buffer size  (commonly disk_REQBUFSZ)
**  vblksize: virtual block size in bytes
*)

let server_loop ~disk_table
                ~sema
                ~nthreads
                ~inbuf_size
                ~outbuf_size
                ~vblksize
    =
    let l2vblksize = let rec iter v n =
                        if v > 0 
                            then iter (v lsr 1) (n+1)
                            else (n-1) in
                       iter vblksize 0 
            in


    (*
    ** Extract the server port from the first virtual disk cap
    *)

    let prv_port = 
                let p = ref nilport in
                try (
                  Array.iter (fun ovd ->
                    match ovd with
                    | Some vd -> p := vd.v_cap.cap_port;
                                 raise Exit;
                    | None -> ();
                    ) disk_table;
                  nilport)              
                with
                    | Exit -> !p
        in

    if prv_port = nilport 
        then failwith "vdisk server thread: no valid vdisk found!";

    disk_dying := false;
    let initial = ref false in
    let on_exit cap =
            let stat = if (!disk_dying = false) then
                       begin
                            disk_dying := true;
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
        
        (*
        ** hdr_rep = hdr_req !!!
        *)
        let hdr_rep = ref (header_new ()) in
        
        let replen = ref 0 in
        let getport = prv_port in
        

        while (true)
        do
        try
          begin
            replen := 0;
    
    
            let stat,reqlen,hdr = getreq (getport,ibuf,inbuf_size) in
            hdr_rep := hdr;

            let priv =hdr.h_priv in
            let numblks = hdr.h_extra in

            (
            match hdr.h_command with

            (*
            ** Standard requests
            *)

            | com when com = std_INFO ->
            begin
                hdr.h_status <- vdisk_std_info hdr
                                               obuf
                                               disk_table;
                if hdr.h_status <> std_OK 
                    then replen := 0
                    else replen := hdr.h_size;
            end;

            | com when com = std_STATUS ->
            begin
                hdr.h_status <- vdisk_std_status hdr
                                               obuf
                                               disk_table;
                if hdr.h_status <> std_OK 
                    then replen := 0
                    else replen := hdr.h_size;
            end;
            
            | com when com = std_AGE ->
            begin
                hdr.h_status <- std_OK;
            end;

            | com when com = std_TOUCH -> 
            begin
                hdr.h_status <- std_OK;
            end;

            | com when com = std_EXIT ->
            begin
                initial := not (!disk_dying);
                let rights = prv_rights hdr.h_priv in
                (*
                ** A full 'righted' valid disk ?
                *)
                let ok = ref false in
                Array.iter (fun ovd ->
                    match ovd with
                    | Some vd -> if (prv_decode hdr.h_priv
                                        vd.v_cap.cap_priv.prv_random) &&
                                    rights = prv_all_rights
                                    then ok := true;
                    | None -> ();
                    ) disk_table;
                
                if !ok then
                begin 
                    hdr.h_status <- on_exit {cap_port = hdr.h_port;
                                             cap_priv = hdr.h_priv};
                    ignore( putrep (hdr,nilbuf,0));
                    raise Exit;
                end
                else
                        hdr.h_status <- std_DENIED;
            end;

            | com when com = std_RESTRICT -> 
            begin
                hdr.h_status <- vdisk_std_restrict hdr
                                        hdr.h_offset
                                        disk_table;
                hdr.h_size <- 0;
            end;


            (*
            ** Disk requests
            *)
            | com when com = disk_CONTROL ->
            begin
                (*
                ** Currently not imple.
                *)
                hdr.h_status <- std_NOTNOW;
            end;

            | com when com = disk_READ ->
            begin
                hdr.h_status <- vdisk_rw hdr    (* hdr.h_command !!! *)
                                         vblksize
                                         hdr.h_offset
                                         numblks
                                         obuf
                                         disk_table;
                if hdr.h_status <> std_OK
                    then hdr.h_size <- 0
                    else 
                begin
                    hdr.h_size <- numblks lsl l2vblksize;
                    replen := hdr.h_size;
                end;
            end;

            | com when com = disk_WRITE ->
            begin
                hdr.h_status <- vdisk_rw hdr    (* hdr.h_command !!! *)
                                         vblksize
                                         hdr.h_offset
                                         numblks
                                         ibuf
                                         disk_table;
                hdr.h_size <- 0;
            end;

            | com when com = disk_SIZE ->
            begin
                let stat,size  = vdisk_size hdr
                                            vblksize
                                            disk_table in
                hdr.h_status <- stat;
                hdr.h_offset <- size;
                hdr.h_size <- 0;
            end;

            | com when com = disk_INFO ->
            begin
                hdr.h_status <- vdisk_info hdr
                                           obuf
                                           disk_table;
                if hdr.h_status <> std_OK
                    then hdr.h_size <- 0
                    else replen := hdr.h_size;

            end;

            | com when com = disk_GETGEOMETRY ->
            begin
                hdr.h_status <- vdisk_getgeom hdr
                                              obuf
                                              disk_table;
                if hdr.h_status <> std_OK
                    then hdr.h_size <- 0
                    else replen := hdr.h_size;

            end;


            | _ -> 
            begin
                let Command com = hdr.h_command in
#ifdef DEBUG
                Db.Pr.sd debug "dns_server std_COMBAD" com;
#endif
                hdr.h_status <- std_COMBAD;
            end;

            );
            
            ignore( putrep (hdr,obuf,!replen));
          end
          with
            | Buf_overflow ->
            begin
                replen := 0;
                !hdr_rep.h_status <- std_ARGBAD;
                ignore( putrep (!hdr_rep,obuf,!replen));
            end;
        done;
    end
    with
        | Exit -> 
                  sys_log Sys_info "VDISK: Server thread exited normally.\n"; 
                  if !initial 
                      then Sema.sema_up sema;


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
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2003-2005 BSSLAB
**    $CREATED:     ?
**    $VERSION:     1.08
**
**    $INFO:
**
** AFS service: Atomic File System
** Client requests.
**
**    $ENDOFINFO
**
*)


open Amoeba
open Bytebuf
open Rpc
open Stderr
open Stdcom
open Buf

open Afs_common


(*
** afs_size
**      AFS server client stub: gets the size of a file.
**
** Argument:
**      cap:    capability of the file.
**
** Return:
**      err:    gives the status of the operation
**      size:   the size of the file.
*)

let afs_size ~cap
             =
    let hdr = header_new () in
    hdr.h_port <- cap.cap_port;
    hdr.h_priv <- cap.cap_priv;

    hdr.h_command <- afs_SIZE;

    let err,n,hdr' = trans (hdr,nilbuf,0,nilbuf,0) in
    if (err <> std_OK) then
        (err,-1)
    else if (hdr'.h_status <> std_OK) then
        (hdr'.h_status,-1)
    else
        (std_OK,hdr'.h_offset)
        
                     
(*    
** afs_delete
**      The AFS server "delete" client stub.
**      Has a problem because size may not fit into the hdr.h_size field.
**      For now we put the size in the buffer but in AMOEBA4 it can go in
**      the header.
** Arguments:
**  cap:        capability for the bullet file to modify
**  offset:     where to make the change 
**  size:       num bytes to delete 
**  commit:     the commit and safety flags 
**
** Return:
**  status
**  newfile: capability for created file 
*)

let afs_delete ~cap
               ~offset
               ~size
               ~commit
    =
    try
    begin
        if (size < 0) then
            raise (Error std_ARGBAD);

        let hdr = header_new () in
        hdr.h_port <- cap.cap_port;
        hdr.h_priv <- cap.cap_priv;
        hdr.h_offset <- offset;
        hdr.h_command <- afs_DELETE;
        hdr.h_extra <- commit;

        let buf = buf_create 4 in
        let p = buf_put_int32 ~buf:buf ~pos:0 ~int32:size in
        let err,n,hdr' = trans (hdr,buf,p,nilbuf,0) in
        
        if (err <> std_OK)
        then
            (err,nilcap)
        else if (hdr'.h_status <> std_OK) then
            (hdr'.h_status,nilcap)
        else
        begin
            let cap = {cap_port = port_copy hdr'.h_port;
                       cap_priv = priv_copy hdr'.h_priv;
                      } in
            (std_OK,cap)
        end;
    end
    with
        | Error err -> err,nilcap

(*
** afs_create
**
**      The AFS server "create file" client stub.
**      It begins with a create transaction.  If the data in buf didn't
**      fit in one transaction then we have do MODIFY transactions after
**      that.
**      We must not send "commit" until the last transaction.
**
**  Args:
**      cap:        capability of the AFS server or a valid
**                  file capability 
**      buf :       initial data for the file 
**      size:       num bytes data in buf 
**      commit:     the commit and safety flags 
**
**  Return:
**      err   :     status of the request
**      newcap:     capability for the created file 
**
*)


let afs_create ~cap
               ~buf
               ~size
               ~commit
    =
    try
    begin
        let fcap = cap in
        if (size < 0) then
            raise (Error std_ARGBAD);

        let hdr = header_new () in
        let cap = ref (cap_copy nilcap) in

        let frags = size / afs_REQBUFSZ in
        let boff = ref 0 in

        for n = 0 to frags
        do
            if (n=0) then
            begin
                hdr.h_port <- fcap.cap_port;
                hdr.h_priv <- fcap.cap_priv;
            end
            else
            begin
                hdr.h_port <- !cap.cap_port;
                hdr.h_priv <- !cap.cap_priv;
            end;
                
            hdr.h_command <- if (n=0) then
                                    afs_CREATE
                                 else
                                    afs_MODIFY;

            let fsize = if (frags = 0) then
                                size
                        else if (n < frags) then
                                afs_REQBUFSZ
                             else 
                                (size - !boff)        
                             in

            hdr.h_size <- fsize;
            hdr.h_offset <- !boff;
            hdr.h_extra <- if (n=frags) then
                                commit (* last transaction so send 
                                       ** commit flags *)
                           else
                                0;
                
            let err',n,hdr' = transo (hdr,buf,!boff,fsize,
                                              nilbuf,0,0) in



            if (hdr'.h_status <> std_OK &&
                    (nullport hdr'.h_port) = false &&
                    (prv_number hdr'.h_priv) <>
                    (prv_number fcap.cap_priv)) then
            begin
                (*
                ** then a new file was half made 
                *)
                let cap = { cap_port = hdr'.h_port;
                                cap_priv = hdr'.h_priv;
                              } in
                ignore (std_destroy ~cap:cap); 
            end;
                 
            if (err' <> std_OK) then
                raise (Error err');
            if (hdr'.h_status <> std_OK) then
                raise (Error hdr'.h_status);

            if (n=0) then
            begin
                !cap.cap_port <- hdr'.h_port;
                !cap.cap_priv <- hdr'.h_priv;
            end;
            boff := !boff + fsize;  
        done;
        (std_OK,!cap)
    end
    with 
        | Error err -> err,nilcap


(*
** afs_read
**      The AFS server reader client stub.
**
** Arguments:
**  cap:        capability of file to be read 
**  offset:     first byte to read from file 
**  buf:        the buffer where data is read into (bufpos = 0 == offset !!)
**  size:       number of bytes requested to be read
**
** Return:
**  err:        status of the request operation
**  num:        number of actually read bytes
*)


let afs_read ~cap
             ~offset
             ~buf
             ~size
    =

    try
    begin
        if (size < 0) then
            raise (Error std_ARGBAD);

        let hdr = header_new () in

        let frags = size / afs_REQBUFSZ in
        let boff = ref 0 in
        for n = 0 to frags
        do
                hdr.h_port <- cap.cap_port;
                hdr.h_priv <- cap.cap_priv;
                hdr.h_command <- afs_READ;

                let fsize = if (frags = 0) then
                                size
                            else if (n < frags) then
                                afs_REQBUFSZ
                            else 
                                (size - !boff)        
                            in

                hdr.h_size <- fsize;
                hdr.h_offset <- offset + !boff;
                
                let err',n,hdr' = transo (hdr,nilbuf,0,0,
                                              buf,!boff,fsize) in

                if (err' <> std_OK) then
                    raise (Error err');
                if (hdr'.h_status <> std_OK) then
                    raise (Error hdr'.h_status);

                boff := !boff + fsize;  
        done;
        (std_OK,!boff)
    end
    with
            | Error err -> (err,0)


(*
** afs_modify
**      The AFS server "modify file" client stub.
**      If the data in buf won't fit in one transaction then we have
**      do several modify commands.
**      We must not send "commit" until the last transaction.
**
**  Args:
**      cap:        capability for the file to be modified 
**      buf :       initial data for the file 
**      offset:     where to make the change
**      size:       num bytes data in buf 
**      commit:     the commit and safety flags 
**
**  Return:
**      err   :     status of the request
**      newcap:     capability for the modified file 
**
*)


let afs_modify ~cap
               ~buf
               ~size
               ~offset
               ~commit
    =
    try
    begin
        let file = cap in

        if (size < 0) then
            raise (Error std_ARGBAD);

        let hdr = header_new () in
        let cap = ref (cap_copy nilcap) in

        let frags = size / afs_REQBUFSZ in
        let boff = ref 0 in
        for n = 0 to frags
        do
                if (n=0)
                then
                begin
                    (*
                    ** Initial cap.  May be changed by AFS.
                    *)
                    hdr.h_port <- file.cap_port;
                    hdr.h_priv <- file.cap_priv;
                end
                else
                begin
                    hdr.h_port <- !cap.cap_port;
                    hdr.h_priv <- !cap.cap_priv;
                end;
                
                hdr.h_command <- afs_MODIFY;

                let fsize = if (frags = 0) then
                                size
                            else if (n < frags) then
                                afs_REQBUFSZ
                            else 
                                (size - !boff)        
                            in

                hdr.h_size <- fsize;
                hdr.h_offset <- offset + !boff;
                hdr.h_extra <- if (n=frags) then
                                commit     (* 
                                           ** last transaction so send
                                           ** commit flags
                                           *)
                               else
                                0;
                
                let err',rn,hdr' = transo (hdr,buf,!boff,fsize,
                                              nilbuf,0,0) in


                if (err' <> std_OK) then
                    raise (Error err');
                if (hdr'.h_status <> std_OK) then
                    raise (Error hdr'.h_status);

                if (n=0) then
                begin
                    !cap.cap_port <- hdr'.h_port;
                    !cap.cap_priv <- hdr'.h_priv;
                end;
                boff := !boff + fsize;  
 
        done;
        (std_OK,!cap)
    end
    with
        | Error err -> (err,nilcap)
        

(*
** afs_insert
**      The AFS server "insert file" client stub.
**      If the data in buf won't fit in one transaction then we have
**      do several modify commands.
**      We must not send "commit" until the last transaction.
**
**  Args:
**      cap:       capability for the file to be modified 
**      buf :       initial data for the file 
**      offset:     where to make the change
**      size:       num bytes data in buf 
**      commit:     the commit and safety flags 
**
**  Return:
**      err   :     status of the request
**      newcap:     capability for the modified file 
**
*)


let afs_insert ~cap
               ~buf
               ~size
               ~offset
               ~commit
    =

    try
    begin
        let file = cap in

        if (size < 0) then
            raise (Error std_ARGBAD);

        let hdr = header_new () in
        let cap = ref (cap_copy nilcap) in

        let frags = size / afs_REQBUFSZ in
        let boff = ref 0 in

        for n = 0 to frags
        do
                if (n=0)
                then
                begin
                    (*
                    ** Initial cap.  May be changed by AFS.
                    *)
                    hdr.h_port <- file.cap_port;
                    hdr.h_priv <- file.cap_priv;
                end
                else
                begin
                    hdr.h_port <- !cap.cap_port;
                    hdr.h_priv <- !cap.cap_priv;
                end;
                
                hdr.h_command <- afs_INSERT;

                let fsize = if (frags = 0) then
                                size
                            else if (n < frags) then
                                afs_REQBUFSZ
                            else 
                                (size - !boff)        
                            in

                hdr.h_size <- fsize;
                hdr.h_offset <- offset + !boff;
                hdr.h_extra <- if (n=frags) then
                                commit     (* 
                                           ** last transaction so send
                                           ** commit flags
                                           *)
                               else
                                0;
                
                let err',rn,hdr' = transo (hdr,buf,!boff,fsize,
                                              nilbuf,0,0) in


                if (err' <> std_OK) then
                    raise (Error err');
                if (hdr'.h_status <> std_OK) then
                    raise (Error hdr'.h_status);

                if (n=0) then
                begin
                    !cap.cap_port <- hdr.h_port;
                    !cap.cap_priv <- hdr.h_priv;
                end;
                boff := !boff + fsize;  
 
        done;
        (std_OK,!cap)
    end
    with
        | Error err -> (err,nilcap)


(*
** afs_destroy
**      AFS server client stub: destroy a file.
**
** Argument:
**      cap:    capability of the file.
**
** Return:
**      err:    gives the status of the operation
*)

let afs_destroy ~cap
             =
    let hdr = header_new () in
    hdr.h_port <- cap.cap_port;
    hdr.h_priv <- cap.cap_priv;

    hdr.h_command <- afs_DESTROY;

    let err,n,hdr' = trans (hdr,nilbuf,0,nilbuf,0) in
    if (err <> std_OK) then
         err
    else if (hdr'.h_status <> std_OK) then
         hdr'.h_status
    else
        std_OK
        

(*
** afs_sync
**      AFS server client stub: flushes committed files to disk
**      The return value is the error status.
*)

let afs_sync ~server
    =
    let hdr = header_new () in
    hdr.h_port <- server.cap_port;
    hdr.h_priv <- server.cap_priv;
    hdr.h_command <- afs_SYNC;

    let err,n,hdr' = trans (hdr,nilbuf,0,nilbuf,0) in
    
    if (err <> std_OK) then
        err
    else 
        hdr'.h_status


(*    
** afs_fsck
**      AFS Server administrator stub routine for file system check.
*)


let afs_fsck ~server
    =
    let hdr = header_new () in
    hdr.h_port <- server.cap_port;
    hdr.h_priv <- server.cap_priv;
    hdr.h_command <- afs_FSCK;

    let err,n,hdr' = trans (hdr,nilbuf,0,nilbuf,0) in
    
    if (err <> std_OK) then
        err
    else 
        hdr'.h_status

(*
** afs_disk_compact
**      AFS server client stub: requests that the disk fragmentation
**      be eliminated.
**      The return value is the error status.
*)

let afs_disk_compact ~server
    =
    let hdr = header_new () in
    hdr.h_port <- server.cap_port;
    hdr.h_priv <- server.cap_priv;
    hdr.h_command <- afs_DISK_COMPACT;

    let err,n,hdr' = trans (hdr,nilbuf,0,nilbuf,0) in
    
    if (err <> std_OK) then
        err
    else 
        hdr'.h_status



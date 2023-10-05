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
**    $INITIAL:     (C) 2003 BSSLAB
**    $CREATED:     2001.00.00
**    $VERSION:     1.35
**
**    $INFO:
**
** Implementation of Amoeba's std commands
** NB: the maximum command number is STD_LAST_COM.
**
**    $ENDOFINFO
**
*)



open Amoeba
open Rpc
open Cmdreg
open Stderr
open Bytebuf
open Buf

let std_MONITOR     = Command (std_FIRST_COM)
let std_AGE         = Command (std_FIRST_COM + 1)
let std_COPY        = Command (std_FIRST_COM + 2)    
let std_DESTROY     = Command (std_FIRST_COM + 3)
let std_INFO        = Command (std_FIRST_COM + 4)
let std_RESTRICT    = Command (std_FIRST_COM + 5)
let std_STATUS      = Command (std_FIRST_COM + 6)
let std_TOUCH       = Command (std_FIRST_COM + 7)
let std_GETPARAMS   = Command (std_FIRST_COM + 8)
let std_SETPARAMS   = Command (std_FIRST_COM + 9)
let std_NTOUCH      = Command (std_FIRST_COM + 10)
let std_EXIT        = Command (std_FIRST_COM + 11) 
let std_RIGHTS      = Command (std_FIRST_COM + 12)
let std_EXEC        = Command (std_FIRST_COM + 13)


(*
** Standard information rqeuest. Returns the server information string. The
** server capability is specified with 'cap'.
*)

let std_info ~cap:cap ~bufsize:bufsize =

    let buf = buf_create bufsize in

    let hdr_req = {
                h_port = cap.cap_port;
                h_priv = cap.cap_priv;
                h_command = std_INFO;
                h_status = std_OK;
                h_offset = 0;
                h_size = bufsize;
                h_extra = 0;
                } in

    let (err_stat,size,hdr_rep) = trans (hdr_req,nilbuf,0,buf,bufsize) in

    let size = if (size > 0 && (buf_get buf (size-1)) = 0) 
                    then (size-1) 
               else size in
    if(size > 0 && (hdr_rep.h_status = std_OK)) then
        (err_stat,(buf_tostring buf 0 (size)))
    else if (err_stat <> std_OK) then
        (err_stat,"")
    else
        (hdr_rep.h_status,"")

(*
** Standard status request. Returns the server status. The
** server capability is specified with 'cap'.
*)

let std_status ~cap:cap ~bufsize:bufsize =

    let buf = buf_create bufsize in

    let hdr_req = {
                h_port = cap.cap_port;
                h_priv = cap.cap_priv;
                h_command = std_STATUS;
                h_status = std_OK;
                h_offset = 0;
                h_size = bufsize;
                h_extra = 0;
                } in

    let (err_stat,size,hdr_rep) = trans (hdr_req,nilbuf,0,buf,bufsize) in
     
    let size = if (size > 0 && (buf_get buf (size-1)) = 0) 
                    then (size-1) 
               else size in

    if(size > 0 && (hdr_rep.h_status = std_OK)) then
        (err_stat,(buf_tostring buf 0 size))
    else if (err_stat <> std_OK) then
        (err_stat,"")
    else
        (hdr_rep.h_status,"")


(*
** Standard exit request. Send the server the exit command. 
** The server capability is specified with 'cap'.
*)

let std_exit ~cap:cap =


    let hdr_req = {
                h_port = cap.cap_port;
                h_priv = cap.cap_priv;
                h_command = std_EXIT;
                h_status = std_OK;
                h_offset = 0;
                h_size = 0;
                h_extra = 0;
                } in

    let (err_stat,size,hdr_rep) = trans (hdr_req,nilbuf,0,nilbuf,0) in
     
    if (err_stat <> std_OK) then
        err_stat
    else
        hdr_rep.h_status

(*
** Destroy a server object.
*)

let std_destroy ~cap:cap =
    let hdr_req = {
                h_port = cap.cap_port;
                h_priv = cap.cap_priv;
                h_command = std_DESTROY;
                h_status = std_OK;
                h_offset = 0;
                h_size = 0;
                h_extra = 0;
                } in

    let (err_stat,size,hdr_rep) = trans (hdr_req,nilbuf,0,nilbuf,0) in
     
    if (err_stat <> std_OK) then
        err_stat
    else
        hdr_rep.h_status

(*
** Touch a server object. This is a NOP, but increments the
** live time of an object, ify any.
*)

let std_touch ~cap:cap =
    let hdr_req = {
                h_port = cap.cap_port;
                h_priv = cap.cap_priv;
                h_command = std_TOUCH;
                h_status = std_OK;
                h_offset = 0;
                h_size = 0;
                h_extra = 0;
                } in

    let (err_stat,size,hdr_rep) = trans (hdr_req,nilbuf,0,nilbuf,0) in
     
    if (err_stat <> std_OK) then
        err_stat
    else
        hdr_rep.h_status

(*
** Age all objects of a server (decrements the live time of all
** objects and destroys objects with live time equal to zero).
** Only allowed with the servers super capability and 
** prv_all_rights!
*)

let std_age ~cap:cap =
    let hdr_req = {
                h_port = cap.cap_port;
                h_priv = cap.cap_priv;
                h_command = std_AGE;
                h_status = std_OK;
                h_offset = 0;
                h_size = 0;
                h_extra = 0;
                } in

    let (err_stat,size,hdr_rep) = trans (hdr_req,nilbuf,0,nilbuf,0) in
     
    if (err_stat <> std_OK) then
        err_stat
    else
        hdr_rep.h_status

    
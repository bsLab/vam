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
**    $INITIAL:     (C) 2003-2006 BSSLAB
**    $CREATED:     2003.11.19
**    $VERSION:     1.07
**
**    $INFO:
**
**  Generic byte stream interface
**
**    $ENDOFINFO
**
*)

open Amoeba
open Cmdreg
open Stderr
open Bytebuf
open Rpc

(* 
** generic commands to file-like servers  - this is really the tty interface
** and the pipe-, fifo-, parallel-, serial server ... 
*)

let stream_CREATE      = Command (stream_FIRST_COM)
let stream_READ        = Command (stream_FIRST_COM + 1)
let stream_WRITE       = Command (stream_FIRST_COM + 2)
let stream_LINK        = Command (stream_FIRST_COM + 3)
let stream_CLOSE       = Command (stream_FIRST_COM + 4)    (* tty server only *)
let stream_STATUS      = Command (stream_FIRST_COM + 5)
let stream_CONTROL     = Command (stream_FIRST_COM + 6)
let stream_COPY        = Command (stream_FIRST_COM + 7)
let stream_INTCAP      = Command (stream_FIRST_COM + 8)    (* tty server only *)
let stream_TIME_READ   = Command (stream_FIRST_COM + 9)    (* tty server only *)
let stream_QUEUESTAT   = Command (stream_FIRST_COM + 10)


let rgt_CREATE      = Rights_bits 0x01
let rgt_READ        = Rights_bits 0x02
let rgt_WRITE       = Rights_bits 0x04
let rgt_APPEND      = Rights_bits 0x08
let rgt_LINK        = Rights_bits 0x10
let rgt_UNLINK      = Rights_bits 0x20
let rgt_STATUS      = Rights_bits 0x40
let rgt_CONTROL     = Rights_bits 0x80

(*
** Special exception's with queues/cb's, 
** returned by STREAM_QUEUESTAT trans in h.h_extra. 
*)
let stream_READEND_CLOSED           = 1
let stream_WRITEEND_CLOSED          = 2

(*
** h_extra as returned by STREAM_READ 
*)
let stream_MOREDATA     = 0       (* You could get more *)
let stream_NOMOREDATA   = 1       (* Hit data boundary *)

(*
** Unit buffer size - largest chunk of data in one transaction
*)
let stream_bufsize  = 8192

(*
** Read max. # size bytes from stream server specified
** with srv capability.
*)
 
let stream_read ~srv ~size =
    let buf = buf_create size in
    let hdr = header_new () in

    let size = ref size in
    let stat = ref std_OK in
    let off = ref 0 in
    
    while (!size > 0 && !stat = std_OK)
    do
      let this_size = min !size stream_bufsize in
      hdr.h_port <- srv.cap_port;
      hdr.h_priv <- srv.cap_priv;
      hdr.h_command <- stream_READ;
      hdr.h_size <- this_size;
      let err,n,hdr' = transo (hdr,nilbuf,0,0,buf,!off,this_size) in

      let last = hdr'.h_extra = stream_NOMOREDATA in
      if (err <> std_OK) then
        stat := err
      else if (hdr'.h_status <> std_OK) then
        stat := hdr'.h_status
      else if n = 0 then
        size := 0
      else
      begin
        (* eliminate \000 char *)        
        let n = if (buf_get buf (hdr'.h_size-1)) = 0 then n -1 else n in
        size := !size - n;
        off := !off + n;    
        (*
        ** Last request?
        *)
        if last then size := 0;
      end;
    done;
    
    if !stat = std_OK then
      std_OK,(buf_tostring buf 0 !off)
    else
      !stat,""

(*
** same as above, but read into a buffer starting at pos
** of maximal length size.
** Returns # of received bytes.
*)

let stream_buf_read ~srv ~buf ~pos ~size =
    let hdr = header_new () in

    let size = ref size in
    let stat = ref std_OK in
    let off = ref pos in
    
    while (!size > 0 && !stat = std_OK)
    do
      let this_size = min !size stream_bufsize in
      hdr.h_port <- srv.cap_port;
      hdr.h_priv <- srv.cap_priv;
      hdr.h_command <- stream_READ;
      hdr.h_size <- this_size;
      let err,n,hdr' = transo (hdr,nilbuf,0,0,buf,!off,this_size) in


      let last = hdr'.h_extra = stream_NOMOREDATA in
      if (err <> std_OK) then
        stat := err
      else if (hdr'.h_status <> std_OK) then
        stat := hdr'.h_status
      else if n = 0 then
        size := 0
      else
      begin
        (* eliminate \000 char *)        
        let n = if (buf_get buf (hdr'.h_size-1)) = 0 then n -1 else n in
        size := !size - n;
        off := !off + n;    
        (*
        ** Last request?
        *)
        if last then size := 0;
      end;
    done;
    
    if !stat = std_OK then
      std_OK,!off
    else
      !stat,0

(*
** Write # size bytes to stream server specified
** with srv capability starting in buf at position pos.
** Note: no \000 char at the end of the string will be send!
*)

let stream_write ~srv ~str =
    let size = ref (String.length str) in
    let buf = buf_ofstring str 0 !size in
    let hdr = header_new () in

    let stat = ref std_OK in
    let off = ref 0 in

    while (!size > 0 && !stat = std_OK)
    do
      let this_size = min !size stream_bufsize in
      hdr.h_port <- srv.cap_port;
      hdr.h_priv <- srv.cap_priv;
      hdr.h_command <- stream_WRITE;
      hdr.h_size <- this_size;
      let err,n,hdr' = transo (hdr,buf,!off,this_size,nilbuf,0,0) in

      if (err <> std_OK) then
        stat := err
      else if (hdr'.h_status <> std_OK) then
        stat := hdr'.h_status
      else
      begin
        size := !size - this_size;
        off := !off + this_size;    
      end;
    done;
    !stat


let stream_buf_write ~srv ~buf ~pos ~size =
    let hdr = header_new () in

    let size = ref size in
    let stat = ref std_OK in
    let off = ref pos in

    while (!size > 0 && !stat = std_OK)
    do
      let this_size = min !size stream_bufsize in
      hdr.h_port <- srv.cap_port;
      hdr.h_priv <- srv.cap_priv;
      hdr.h_command <- stream_WRITE;
      hdr.h_size <- this_size;
      let err,n,hdr' = transo (hdr,buf,!off,this_size,nilbuf,0,0) in

      if (err <> std_OK) then
        stat := err
      else if (hdr'.h_status <> std_OK) then
        stat := hdr'.h_status
      else
      begin
        size := !size - this_size;
        off := !off + this_size;    
      end;
    done;
    !stat

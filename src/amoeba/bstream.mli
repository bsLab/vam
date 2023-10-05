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
**    $VERSION:     1.05
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

(* 
** generic commands to stream servers  
*)


val stream_CREATE : Amoeba.command
val stream_READ : Amoeba.command
val stream_WRITE : Amoeba.command
val stream_LINK : Amoeba.command
val stream_CLOSE : Amoeba.command
val stream_STATUS : Amoeba.command
val stream_CONTROL : Amoeba.command
val stream_COPY : Amoeba.command
val stream_INTCAP : Amoeba.command
val stream_TIME_READ : Amoeba.command
val stream_QUEUESTAT : Amoeba.command

(*
** rights
*)

val rgt_CREATE      : Amoeba.rights_bits 
val rgt_READ        : Amoeba.rights_bits 
val rgt_WRITE       : Amoeba.rights_bits 
val rgt_APPEND      : Amoeba.rights_bits 
val rgt_LINK        : Amoeba.rights_bits 
val rgt_UNLINK      : Amoeba.rights_bits 
val rgt_STATUS      : Amoeba.rights_bits 
val rgt_CONTROL     : Amoeba.rights_bits 

(*
** Special exception's with queues/cb's, 
** returned by STREAM_QUEUESTAT trans in h.h_extra. 
*)
val stream_READEND_CLOSED           : int
val stream_WRITEEND_CLOSED          : int

(*
** h_extra as returned by STREAM_READ 
*)
val stream_MOREDATA     : int       (* You could get more *)
val stream_NOMOREDATA   : int       (* Hit data boundary *)


(*
** Unit buffer size - largest chunk of data in one transaction
*)
val stream_bufsize  : int


(*
** Read max. # size bytes from stream server specified
** with srv capability.
*)

val stream_read : srv:Amoeba.capability  -> size:int -> 
               Amoeba.status * string

(*
** same as above, but read into a buffer starting at pos
** of maximal length size.
** Returns # of received bytes.
*)

val stream_buf_read : srv:Amoeba.capability  -> buf:Bytebuf.buffer ->
                   pos:int -> size:int -> 
                    Amoeba.status * int


(*
** Write # size bytes to stream server specified
** with srv capability starting in buf at position pos.
*)

val stream_write : srv:Amoeba.capability  -> str:string ->
                Amoeba.status


val stream_buf_write : srv:Amoeba.capability  -> buf:Bytebuf.buffer -> 
                pos:int -> size:int ->
                Amoeba.status


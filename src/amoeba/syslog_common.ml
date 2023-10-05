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
**    $INITIAL:     (C) 2005 BSSLAB
**    $CREATED:     29.4.2005
**    $VERSION:     1.03
**
**    $INFO:
**
**  SYSLOG service - common definitions
** All system servers (and of cources user servers, too)
** can and ***should*** write their messages (info,
** warnings, errors) to this system unique log server.
** The syslog server keep all incoming messages 
** in a local buffer of size SYSLOG_BUFSIZE.
** If the buffer overflows, the syslog server writes the buffer
** to disk with time and and date reference in the
** directory SYSLOG_PATH.
**
** Local, on the client side, all messages are buffered
** with a circular buffer. A different deliver thread put
** all buffered messages out of the circular buffer to the
** syslog server with the generic Amoeba rpc interface.
** After the SYSLOG_CHECK timeout is passed, the deliver thread
** try to connect to the syslog server or send buffered
** measseges.
** The reason for this procedure:
** - The client can start before the syslog server is ready/started
** - messages are collected and can be send all in one transaction
**
**    $ENDOFINFO
**
*)

open Amoeba
open Cmdreg

(*
** log levels 
*)
let  sys_DEBUG  =      0       (* debug level           *)
let  sys_INFO   =      1       (* normal information    *)
let  sys_NOTICE =      2       (* normal, but remarkable *)
let  sys_WARN   =      3       (* warning !             *)
let  sys_ERROR  =      4       (* error !!              *)
let  sys_ERR    =      sys_ERROR
let  sys_FATAL  =      5       (* critical error !!!    *)

let  sys_PRINT  =      6       (* print always (?)      *)
let  sys_STARTUP =     7       (* startup messages (?)  *)


type syslog_type =
    | Sys_debug
    | Sys_info
    | Sys_notice
    | Sys_warn
    | Sys_err
    | Sys_fatal
    | Sys_print
    | Sys_start


(*
** Sys log server interface:
** Standard commands 
*)

let syslog_WRITE   = Command (syslog_FIRST_COM+1)  (* write a message *)
let syslog_READ    = Command (syslog_FIRST_COM+2)  (* read the current message buf *)
let syslog_LEVEL   = Command (syslog_FIRST_COM+3)  (* change report level (?) *)
let syslog_SHUTDOWN= Command (syslog_FIRST_COM+4)  (* shutdown the sys log server *)
let syslog_SYNC    = Command (syslog_FIRST_COM+5)  (* flush the message buffer to disk *)

let syslog_SETOUT  = Command (syslog_FIRST_COM+6)  (* set stdout cap *)
let syslog_VERBOSE = Command (syslog_FIRST_COM+7)  (* switch on/off verbose level *)

(*
** Additional kernel stuff:
** Inform a kernel (system server) about our server cap 
** buf=ar_cap(syslog_server_cap)
** hdr.h_offset=strlen(buf)
*)

let syslog_CAPINFO =(syslog_FIRST_COM+8)

(*
** The syslog server gets kernel messages with a special command
** because syslog must resolve the kernel name in each message!
*)

let syslog_KWRITE =  (syslog_FIRST_COM+9)    (* Write a kernel message *)

(*
** Only messages with level >= ## are logged
*)
let syslog_verbose_level = ref 1

let syslog_BUFSIZE = 30000

(*
** after this timeout the client deliver thread retry
** connecting to the syslog server
*)
let syslog_CHECK = 5    (* seconds *)

(*
** after this timeout the syslog server flushes his buffer to disk
*)
let syslog_FLUSH = (24*3600) (* 1 day *)

(*
** Common path to publish the syslog server capability 
*)

let syslog_SERVERPATH = "/server/syslog"


(*
** syslog file directory - where to write all the senseless messages to
*)
let syslog_PATH = "/messages/syslog"



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
**    $CREATED:     ??
**    $VERSION:     1.01
**
**    $INFO:
**
**
**    $ENDOFINFO
**
*)

open Amoeba
open Stderr
open Stdcom
open Stdcom2

open Io
open Trace_fun

let help = "
usage: trace <system server path> <type> <command>
       commands: init <size> <mask>
                 delete  
                 start 
                 stop 
                 reset <mask>
                 print <file_name>

       type: net, threads

       path: /unix -> UNIX, else Amoeba DNS

       mask: type threads:
                TRACE_THR       0x1
                TRACE_MUTEX     0x2
                TRACE_EVENT     0x4 
                TRACE_RPC       0x8
                TRACE_IPC       0x10
                TRACE_PROC      0x20
                TRACE_SYSCALL   0x40
                TRACE_TIMER     0x80
                TRACE_INFO      0x1000
                TRACE_DEBUG     0x2000
                TRACE_ALL       0xFFFF

              type net:
                TRACE_PKT       0x1   
    First the trace must be initialized using the 'init' command. 
    The number of events is currently limited to 32000. 
    The mask specifies the events to be logged.
    After initialization, the trace logging can be started using the 'start'
    command and stopped any time using the 'stop' command.
    The trace can be dumped to a log file using the 'print' command.
    The actual trace can be cleared with the 'reset' command. The trace
    flags can be changed ot kept.
"

type std_com = 
    | Noop of string array
    | Help of string array
    | Init of string array
    | Delete of string array
    | Start of string array
    | Stop of string array
    | Reset of string array
    | Print of string array

let check_args args num =
    if (Array.length args) <> num then
    begin
        out help; nl (); 
        exit 1;
    end

let to_command com args =
    match com with
    | "init" -> check_args args 2; Init args;
    | "delete" -> check_args args 0; Delete args;
    | "start" -> check_args args 0; Start args;
    | "stop" -> check_args args 0; Stop args;
    | "reset" -> check_args args 1; Reset args;
    | "print" -> check_args args 1; Print args;
    | _ -> raise (Error std_COMBAD)




let usage err =
    out help; nl ();
    raise (Error err)
    

let exec_com server ttype com =
  try
  begin
    match com with

    | Help args ->
    begin
        out help; nl ();
        std_OK
    end;

    | Init args -> f_init server ttype args;
    | Delete args -> f_delete server ttype args;
    | Start args -> f_start server ttype args;
    | Stop args -> f_stop server ttype args;
    | Reset args -> f_reset server ttype args;
    | Print args -> f_print server ttype args;
    | _ -> std_COMBAD
  end
  with
    | Error err -> err


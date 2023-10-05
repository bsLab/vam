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
**    $CREATED:     ?.4.2005
**    $VERSION:     1.01
**
**    $INFO:
**
**  Common types for the Virtual terminal server.
**
**    $ENDOFINFO
**
*)
open Amoeba 
open Stderr  
open Stdcom
open Stdcom2
open Cmdreg

(*
** Terminal I/O
*)

let tty_READ    = Command (tty_FIRST_COM+52)
let tty_WRITE   = Command (tty_FIRST_COM+53)


(*
** Termios Interface 
*)
let tios_FIRST_COM = 3200
let tios_LAST_COM  = 3299
let tios_FIRST_ERR = (-tios_FIRST_COM)
let tios_LAST_ERR  = (-tios_LAST_COM)

let tios_GETATTR   = Command tios_FIRST_COM
let tios_SETATTR   = Command (tios_FIRST_COM+1)
let tios_SENDBREAK = Command (tios_FIRST_COM+2)
let tios_DRAIN     = Command (tios_FIRST_COM+3)
let tios_FLUSH     = Command (tios_FIRST_COM+4)
let tios_FLOW      = Command (tios_FIRST_COM+5)
let tios_GETWSIZE  = Command (tios_FIRST_COM+6)
let tios_SETWSIZE  = Command (tios_FIRST_COM+7)

let tty_REQBUFSZ = 30000

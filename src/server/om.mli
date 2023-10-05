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
**    $CREATED:     2003.11.23
**    $VERSION:     0.11
**
**    $INFO:
**
**  Object manager server -> Garbage collection
**
**    $ENDOFINFO
**
*)

open Amoeba
open Stderr
open Stdcom
open Stdcom2

(*
** Setup configuration
*)
type om_config =
    | Om_root   of string               (* root directory path          *)
    | Om_root_cap   of string           (* same, but cap. specified     *)
    | Om_ignore of string               (* ignore this path             *)
    | Om_ignore_cap of string           (* same, but cap. specified     *)
    | Om_age of string                  (* age server spec. with path   *)
    | Om_age_cap of string              (* same, but cap. specified     *)
    | Om_mintouch of int                (*
                                        ** min. successfull touched
                                        ** objects required
                                        ** before aging is started
                                        *)
    | Om_passnum of int                  (* number of passes(0=continous) *)

type om = {
    mutable om_root: capability;            (* start root dir           *)
    mutable om_ignore: capability list;     (* dirs to ignore           *)
    mutable om_age: capability list;        (* age this servers         *)
    mutable om_mintouch: int;               (* min touched to start age *)
    mutable om_touched: int;                (* touched objects          *)
    mutable om_failed: int;                 (* not rechaed objects      *)
    mutable om_report: string;              (* report string            *)
    mutable om_passnow: int;
    mutable om_passnum: int;
}

(*
** Initialize om from configuration data.
*)

val om_init :   om_config list -> om

(*
** Walk a directory tree and touch all rechable objects.
** We must be aware of recursive directory structures!
*)


val om_walk_dir : om -> status




(*
** Start aging of specified servers
*)

val om_age : om -> status

(*
** Om main loop
*)

val om_loop : om -> status


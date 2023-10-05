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
**    $VERSION:     1.01
**
**    $INFO:
**
** AFS: Atomic File Service 
** Common module
**
**    $ENDOFINFO
**
*)


open Amoeba
open Cmdreg
open Stderr
open Stdcom

(*
** AFS requests
*)

let afs_CREATE          = Command (afs_FIRST_COM + 1) 
let afs_DELETE          = Command (afs_FIRST_COM + 2) 
let afs_FSCK            = Command (afs_FIRST_COM + 3) 
let afs_INSERT          = Command (afs_FIRST_COM + 4) 
let afs_MODIFY          = Command (afs_FIRST_COM + 5) 
let afs_READ            = Command (afs_FIRST_COM + 6) 
let afs_SIZE            = Command (afs_FIRST_COM + 7) 
let afs_DISK_COMPACT    = Command (afs_FIRST_COM + 8) 
let afs_SYNC            = Command (afs_FIRST_COM + 9) 
let afs_DESTROY         = Command (afs_FIRST_COM + 10)

(*
** AFS Server's Error Codes (none at present)
*)

(*
** AFS rights 
** NB: ADMIN bit is only valid in supercap.
*)

let afs_RGT_CREATE      = Rights_bits 0x1
let afs_RGT_READ        = Rights_bits 0x2
let afs_RGT_MODIFY      = Rights_bits 0x4
let afs_RGT_DESTROY     = Rights_bits 0x8
let afs_RGT_ADMIN       = Rights_bits 0x80

let afs_RGT_ALL         = prv_all_rights


(*
** Commit flags 
*)

let afs_UNCOMMIT        = 0x0
let afs_COMMIT          = 0x1
let afs_SAFETY          = 0x2

(*
** Size of AFS Server's getreq buffer in bytes.
** Tuned to Ethernet:
** Ethernet packets have 1490 bytes
*)

let afs_REQBUFSZ        = ((1490 - header_SIZE) + (19 * 1490))


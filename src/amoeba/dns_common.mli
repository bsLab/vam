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
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.16
**
**    $INFO:
**
** DNS: Directory and Name Service
** Common values and structures - both for servers and clients
**
**
**    $ENDOFINFO
**
*)

open Bytebuf


val dns_REQBUFSZ : int

val dns_BUFSIZE : int
val dns_MAXCOLUMNS : int
val dns_NTRY : int

val dns_CREATE : Amoeba.command
val dns_DISCARD : Amoeba.command
val dns_LIST : Amoeba.command
val dns_APPEND : Amoeba.command
val dns_CHMOD : Amoeba.command
val dns_DELETE : Amoeba.command
val dns_LOOKUP : Amoeba.command
val dns_SETLOOKUP : Amoeba.command
val dns_INSTALL : Amoeba.command
val dns_REPLACE : Amoeba.command
val dns_GETMASKS : Amoeba.command
val dns_GETSEQNR : Amoeba.command
val dns_GETDEFBULLET : Amoeba.command
val dns_GETDEFAFS : Amoeba.command
val dns_RENAME: Amoeba.command

val dns_COLMASK : int
val dns_RGT_DEL : int
val dns_RGT_MOD : int
val dns_DEFAULT : Capset.capset
val dns_NOMOREROWS : int
val dns_MAXPATH : int
val dns_DEFAULT_COLS : string array

val path_normalize : path:string -> string

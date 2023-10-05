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


open Bytebuf


val afs_CREATE : Amoeba.command
val afs_DELETE : Amoeba.command
val afs_FSCK : Amoeba.command
val afs_INSERT : Amoeba.command
val afs_MODIFY : Amoeba.command
val afs_READ : Amoeba.command
val afs_SIZE : Amoeba.command
val afs_DISK_COMPACT : Amoeba.command
val afs_SYNC : Amoeba.command
val afs_DESTROY : Amoeba.command

val afs_RGT_CREATE : Amoeba.rights_bits
val afs_RGT_READ : Amoeba.rights_bits
val afs_RGT_MODIFY : Amoeba.rights_bits
val afs_RGT_DESTROY : Amoeba.rights_bits
val afs_RGT_ADMIN : Amoeba.rights_bits
val afs_RGT_ALL : Amoeba.rights_bits

val afs_UNCOMMIT : int
val afs_COMMIT : int
val afs_SAFETY : int

val afs_REQBUFSZ : int



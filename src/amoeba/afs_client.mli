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
**    $VERSION:     1.08
**
**    $INFO:
**
** AFS service: Atomic File System
** Client requests.
**
**    $ENDOFINFO
**
*)

open Bytebuf


val afs_size : cap:Amoeba.capability -> Amoeba.status * int

val afs_delete :
  cap:Amoeba.capability ->
  offset:int -> size:int -> commit:int -> Amoeba.status * Amoeba.capability

val afs_create :
  cap:Amoeba.capability ->
  buf:buffer ->
  size:int -> commit:int -> Amoeba.status * Amoeba.capability

val afs_read :
  cap:Amoeba.capability ->
  offset:int -> buf:buffer -> size:int -> Amoeba.status * int

val afs_modify :
  cap:Amoeba.capability ->
  buf:buffer ->
  size:int -> offset:int -> commit:int -> Amoeba.status * Amoeba.capability

val afs_insert :
  cap:Amoeba.capability ->
  buf:buffer ->
  size:int -> offset:int -> commit:int -> Amoeba.status * Amoeba.capability

val afs_destroy :
  cap:Amoeba.capability ->
  Amoeba.status

val afs_sync : server:Amoeba.capability -> Amoeba.status

val afs_fsck : server:Amoeba.capability -> Amoeba.status

val afs_disk_compact : server:Amoeba.capability -> Amoeba.status

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
**    $INITIAL:     (C) 2004 BSSLAB
**    $CREATED:     
**    $VERSION:     1.15
**
**    $INFO:
**
**  Disk Server Interface
**
**    $ENDOFINFO
**
*)


(*
** Disk server interface
*)

open Bytebuf
open Machtype


val vdisk_SVR_NAME : string
val pdisk_SVR_NAME : string
val ldisk_SVR_NAME : string
val fldisk_SVR_NAME : string
val disk_INFO : Amoeba.command
val disk_READ : Amoeba.command
val disk_SIZE : Amoeba.command
val disk_WRITE : Amoeba.command
val disk_GETGEOMETRY : Amoeba.command
val disk_CONTROL : Amoeba.command
val dsc_eject : int
val dsc_format : int
val disk_RGT_READ : Amoeba.rights_bits
val disk_RGT_WRITE : Amoeba.rights_bits
val disk_RGT_CTL : Amoeba.rights_bits

val disk_INFO_SIZE : int
type disk_info = {
  disk_unit : Machtype.int32;
  disk_firstblk : Machtype.int32;
  disk_numblks : Machtype.int32;
}


(* 
** DISK_INFO
**      This is the client stub for the disk_info command.
**      It returns a list of disk_addr's which are tuples of
**      (unit, firstblock, # blocks).
*) 
val disk_info : Amoeba.capability -> Amoeba.status * disk_info list

(*
** Convert block size in bytes to log2 block size
*)
val l2_of_block_size : int -> int

(*
** DISKREAD
**      This is the client stub for the disk_read command.
**      Since reads may be bigger than fit in a single transaction
**      we loop doing transactions until we are finished.
**      If it can read exactly what was requested it succeeds.
**      Otherwise it fails.  No partial reads are done.
*)
val disk_read :
  Amoeba.capability ->
  start:int ->
  num:int -> blksize:int -> buf:buffer -> pos:int -> Amoeba.status

(*
** DISKWRITE
**      This is the client stub for the disk_write command.
**      Since writes may be bigger than fit in a single transaction
**      we loop doing transactions until we are finished.
*)

val disk_write :
  Amoeba.capability ->
  start:int ->
  num:int -> blksize:int -> buf:buffer -> pos:int -> Amoeba.status

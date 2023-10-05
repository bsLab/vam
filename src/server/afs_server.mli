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
**    $INITIAL:     (C) BSSLAB 2003
**    $CREATED:     
**    $VERSION:     1.02
**
**    $INFO:
**
** AFS: Atomic Filesystem Service
**
** Device independent server implementation.
**      File Sizes:     Logical, in bytes
**      Disk addresses: Physical, in blocks [super.afs_bock_size]
**      File offsets:   Logical, in bytes
**
**      Free/Used
**      Clusters:       Physical (both addr and size!), in blocks
**
**      A File always occupy full blocks.
**
**
**    $ENDOFINFO
**
*)


open Bytebuf


exception Deny

val debug : int
val afs_MAXLIVE : int

type afs_file_state = FF_invalid | FF_unlocked | FF_commit | FF_locked

type afs_inode = { mutable fi_daddr : int; 
                   mutable fi_ioff : int; 
                   mutable fi_res: int; } 

type afs_file = {
  mutable ff_lock : Mutex.t;
  mutable ff_objnum : int;
  mutable ff_random : Amoeba.port;
  mutable ff_time : int;
  mutable ff_live : int;
  mutable ff_state : afs_file_state;
  mutable ff_size : int;
  mutable ff_inode : afs_inode;
  mutable ff_modified : bool;
} 

val nilafsfile : afs_file

type afs_super = {
  mutable afs_lock : Mutex.t;
  mutable afs_name : string;
  mutable afs_nfiles : int;
  mutable afs_nused : int;
  mutable afs_freeobjnums : int list;
  mutable afs_nextfree : int;
  mutable afs_getport : Amoeba.port;
  mutable afs_putport : Amoeba.port;
  mutable afs_checkfield : Amoeba.port;
  mutable afs_block_size : int;
  mutable afs_nblocks: int;
} 

val nilafssuper : afs_super

type afs_server = {
  mutable afs_super : afs_super;
  mutable afs_read_file :
    file:afs_file ->
    off:int -> size:int -> buf:buffer -> Amoeba.status;
  mutable afs_modify_file :
    file:afs_file ->
    off:int -> size:int -> buf:buffer -> Amoeba.status;
  mutable afs_modify_size : file:afs_file -> newsize:int -> Amoeba.status;
  mutable afs_commit_file : file:afs_file -> flag:int -> Amoeba.status;
  mutable afs_read_inode : obj:int -> Amoeba.status * afs_file;
  mutable afs_create_inode : file:afs_file -> final:bool -> Amoeba.status;
  mutable afs_delete_inode : file:afs_file -> Amoeba.status;
  mutable afs_modify_inode : file:afs_file -> Amoeba.status;
  mutable afs_read_super : unit -> afs_super * Amoeba.status;
  mutable afs_sync : unit -> Amoeba.status;
  mutable afs_stat : obj:int -> Amoeba.status * string;
  mutable afs_age : obj:int -> bool*int;
  mutable afs_touch : file:afs_file -> unit;
  mutable afs_exit: unit -> Amoeba.status;
  mutable afs_time: unit -> int;    
} 

val acquire_file : server:afs_server -> obj:int -> Amoeba.status * afs_file

val release_file :
  server:afs_server -> file:afs_file -> flag:int -> Amoeba.status

val get_freeobjnum : afs_super -> int

val afs_req_size :
  server:afs_server -> priv:Amoeba.privat -> Amoeba.status * int

val afs_req_create :
  server:afs_server ->
  priv:Amoeba.privat ->
  buf:buffer ->
  size:int -> commit:int -> Amoeba.status * Amoeba.capability

val afs_req_read :
  server:afs_server ->
  priv:Amoeba.privat ->
  buf:buffer -> off:int -> size:int -> Amoeba.status * int

val afs_req_modify :
  server:afs_server ->
  priv:Amoeba.privat ->
  buf:buffer ->
  off:int -> size:int -> commit:int -> Amoeba.status * Amoeba.capability

val afs_req_insert :
  server:afs_server ->
  priv:Amoeba.privat ->
  buf:buffer ->
  off:int -> size:int -> commit:int -> Amoeba.status * Amoeba.capability

val afs_req_delete :
  server:afs_server ->
  priv:Amoeba.privat ->
  off:int -> size:int -> commit:int -> Amoeba.status * Amoeba.capability

val afs_req_destroy :
  server:afs_server ->
  priv:Amoeba.privat ->
  Amoeba.status 

val afs_req_stat :
  server:afs_server ->
  priv:Amoeba.privat ->  
  Amoeba.status * string 

val afs_req_sync :
  server:afs_server ->
  priv:Amoeba.privat ->
  Amoeba.status

val afs_req_touch :
  server:afs_server ->
  priv:Amoeba.privat ->
  Amoeba.status

val afs_req_age :
  server:afs_server ->
  priv:Amoeba.privat ->
  Amoeba.status


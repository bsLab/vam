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
**    $VERSION:     1.48
**
**    $INFO:
**
** DNS: Directory and Name Service
** Server implementation.
**
**
**    $ENDOFINFO
**
*)



open Bytebuf




exception Deny
val dns_MAXLIVE : int

(*
** DNS operating modes
*)

type dns_mode =
    | Dnsmode_ONECOPY       (* Default file server number = 0 *)
    | Dnsmode_TWOCOPY       (* Duplicated file server mode    *)



type fs_state = 
    | FS_down 
    | FS_up   
    | FS_unknown
                    
                    
type fs_server = {
    mutable fs_cap      : Amoeba.capability array;     
    mutable fs_state    : fs_state array;       
    mutable fs_default  : int;                 
    mutable dns_mode     : dns_mode;
}

type dns_row = {
  mutable dr_name : string;
  mutable dr_time : int;
  mutable dr_columns : Amoeba.rights_bits array;
  mutable dr_capset : Capset.capset;
} 

val nildnsrow : dns_row
type dns_dir_state = 
    | DD_invalid 
    | DD_unlocked
    | DD_modified 
    | DD_locked

and dns_dir = {
  mutable dd_lock : Mutex.t;
  mutable dd_objnum : int;
  mutable dd_ncols : int;
  mutable dd_nrows : int;
  mutable dd_colnames : string array;
  mutable dd_random : Amoeba.port;
  mutable dd_rows : dns_row Dblist.dblist;
  mutable dd_state : dns_dir_state;
  mutable dd_time : int;
  mutable dd_live : int;
} 
val nildnsdir : dns_dir
type dns_super = {
  mutable dns_lock: Mutex.t;
  mutable dns_name : string;
  mutable dns_ndirs : int;
  mutable dns_nused : int;
  mutable dns_freeobjnums : int list;
  mutable dns_nextfree : int;
  mutable dns_getport : Amoeba.port;
  mutable dns_putport : Amoeba.port;
  mutable dns_checkfield : Amoeba.port;
  mutable dns_ncols : int;
  mutable dns_colnames : string array;
  mutable dns_generic_colmask : Amoeba.rights_bits array;
  mutable dns_fs_server : fs_server;
  mutable dns_block_size : int;
} 
and dns_server = {
  mutable dns_super : dns_super;
  mutable dns_read_dir : obj:int -> dns_dir * Amoeba.status;
  mutable dns_modify_dir : dir:dns_dir -> Amoeba.status;
  mutable dns_create_dir : dir:dns_dir -> Amoeba.status;
  mutable dns_delete_dir : dir:dns_dir -> Amoeba.status;
  mutable dns_delete_inode : obj:int -> Amoeba.status;
  mutable dns_read_super : unit -> dns_super * Amoeba.status;
  mutable dns_sync : unit -> Amoeba.status;
  mutable dns_stat: unit -> Amoeba.status * string;
  mutable dns_touch: dir:dns_dir -> unit;
  mutable dns_age: obj:int -> bool*int;
  mutable dns_exit: unit -> Amoeba.status;
  mutable dns_time: unit -> int;
} 
val nildnssuper: dns_super

val dns_col_bits : Amoeba.rights_bits array

val acquire_dir : server:dns_server -> obj:int -> 
                  Amoeba.status * dns_dir 

val dns_search_row : dir:dns_dir -> name:string -> dns_row option
val get_freeobjnum : dns_super -> int

val dns_create_dir : server:dns_server -> ncols:int -> 
  colnames:string array ->  Amoeba.status * dns_dir

val capset_of_dir : super:dns_super -> dir:dns_dir ->
  rights:Amoeba.rights_bits -> Capset.capset

val dns_create_super :
  name:string ->
  ndirs:int ->
  ncols:int ->
  colnames:string array -> colmask:Amoeba.rights_bits array -> dns_super

val dns_create_root : server:dns_server -> Amoeba.status * dns_dir

val dns_delete_dir : server:dns_server -> dir:dns_dir -> Amoeba.status

val dns_destroy_dir : server:dns_server -> dir:dns_dir -> Amoeba.status

val dns_create_row :
  name:string -> cols:Amoeba.rights_bits array -> cs:Capset.capset -> dns_row

val dns_append_row :
  server:dns_server -> dir:dns_dir -> row:dns_row -> unit

val dns_delete_row :
  server:dns_server ->
  dir:dns_dir -> row:dns_row  -> unit

val request_dir :
  server:dns_server ->
  priv:Amoeba.privat ->
  req:Amoeba.rights_bits -> Amoeba.status * dns_dir 

val release_dir : server:dns_server -> dir:dns_dir -> Amoeba.status

val get_dir :
  server:dns_server -> dir_cs:Capset.capset -> Amoeba.status * Amoeba.privat

val dns_restrict :
  server:dns_server ->
  cs:Capset.capset ->
  mask:Amoeba.rights_bits ->
  Amoeba.status * Capset.capset

val dns_req_lookup :
  server:dns_server ->
  priv:Amoeba.privat ->
  path:string -> Amoeba.status * Capset.capset * string

val dns_req_list :
  server:dns_server ->
  priv:Amoeba.privat ->
  firstrow:int ->
  Amoeba.status * int * int * 
  (string array) *
  (string * Amoeba.rights_bits array) list

val dns_req_append :
  server:dns_server ->
  priv:Amoeba.privat ->
  name:string -> 
  cols:Amoeba.rights_bits array ->
  capset:Capset.capset ->
  Amoeba.status 

val dns_req_create :
  server:dns_server ->
  priv:Amoeba.privat ->
  colnames:string array -> 
  Amoeba.status * Capset.capset

val dns_req_discard :
  server:dns_server ->
  priv:Amoeba.privat ->
  Amoeba.status 

val dns_req_destroy :
  server:dns_server ->
  priv:Amoeba.privat ->
  Amoeba.status 

val dns_req_chmod :
  server:dns_server ->
  priv:Amoeba.privat ->
  cols:Amoeba.rights_bits array -> 
  name:string ->
  Amoeba.status 

val dns_req_delete :
  server:dns_server ->
  priv:Amoeba.privat ->
  name:string -> 
  Amoeba.status 

val dns_req_replace :
  server:dns_server ->
  priv:Amoeba.privat ->
  name:string -> 
  newcs:Capset.capset ->
  Amoeba.status 

val dns_req_rename :
  server:dns_server ->
  priv:Amoeba.privat ->
  oldname:string -> 
  newname:string -> 
  Amoeba.status 

val dns_req_touch :
  server:dns_server ->
  priv:Amoeba.privat ->
  Amoeba.status 

val dns_req_age :
  server:dns_server ->
  priv:Amoeba.privat ->
  Amoeba.status 

val dns_req_getmasks :
  server:dns_server ->
  priv:Amoeba.privat ->
  name:string ->
  Amoeba.status * Amoeba.rights_bits array 

val dns_req_setlookup :
  server:dns_server ->
  dirs:(Capset.capset * string) list ->
  (Amoeba.status * int * Capset.capset) list 

  
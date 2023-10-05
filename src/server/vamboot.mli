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
**      BSSLAB, Dr. Stefan Bosse www.bsslab.de
**
**    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF: 
**    Free Software Foundation-Europe, GNU GPL License, Version 2
**
**    $MODIFIEDBY:  BSSLAB
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2004
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.10
**
**    $INFO:
**
** VAM boot server implementation
**	
**
**    $ENDOFINFO
**
*)

type boot_objcap =
    Boot_path of string
  | Boot_cap of Amoeba.capability
  | Boot_capstr of string
  | Boot_nil
val nilpath : boot_objcap
type boot_src =
    Unix_src of boot_objcap
  | Amoeba_src of boot_objcap
  | Fun_src of (unit -> Amoeba.status)
  | Nil_src
and boot_dst = Unix_dst | Amoeba_dst of boot_objcap | Nil_dst
and boot_stat =
    Boot_cold
  | Boot_starting
  | Boot_killing
  | Boot_up
  | Boot_down
  | Boot_executed
  | Boot_restarting
  | Boot_unknown
and boot_op =
    Boot_poll of (boot_objcap * int)
  | Boot_start
  | Boot_stop
  | Boot_restart
  | Boot_coldstart
and boot_env =
    Env_cap of (string * Amoeba.capability)
  | Env_cappath of (string * string)
  | Env_capstr of (string * string)
  | Env_str of (string * string)
  | Env_self of string
and boot_def = {
  boot_name : string;
  boot_src : boot_src;
  boot_dst : boot_dst;
  boot_args : string list;
  mutable boot_env : boot_env list;
  boot_ops : boot_op list;
  boot_deps : string list;
} 
val nildef : boot_def
type boot_type = Unix_type | Amoeba_type | Fun_type | Nil_type
and cap_env = {
  mutable boot_std_in : Amoeba.capability;
  mutable boot_std_out : Amoeba.capability;
  mutable boot_std_err : Amoeba.capability;
  mutable boot_root : Amoeba.capability;
  mutable boot_tod : Amoeba.capability;
  mutable boot_rand : Amoeba.capability;
} 
val nilenv : cap_env
type boot_obj = {
  mutable boot_def : boot_def;
  mutable boot_type : boot_type;
  mutable boot_stat : boot_stat;
  mutable boot_op : boot_op;
  mutable boot_owner : Amoeba.capability;
  mutable boot_poll_cap : Amoeba.capability;
  mutable boot_tid : int;
  mutable boot_id : int;
  mutable boot_pid : int;
  mutable boot_proc_cap : Amoeba.capability;
  mutable boot_chain : boot_obj list;
  mutable boot_chain_inv : boot_obj list;
  mutable boot_capenv : cap_env;
} 
val nilobj : boot_obj
type boot_server = {
  boot_prv_port : Amoeba.port;
  boot_pub_port : Amoeba.port;
  boot_rnd_port : Amoeba.port;
  boot_supercap : Amoeba.capability;
  boot_objs : boot_obj array;
  boot_afu : Afu_server.afu_server;
  mutable boot_dying : bool;
  mutable boot_timer : int;
  boot_sema : Sema.semaphore;
} 
and file_type = Boot_binary | Boot_bytecode | Boot_script

val boot_warn_obj : boot_obj -> string -> unit
val boot_fatal_obj : boot_obj -> string -> 'a
val boot_info_obj : boot_obj -> string -> unit
val boot_warn : string -> unit
val boot_info : string -> unit
val boot_stop_obj : boot_obj -> Amoeba.status
val boot_control_obj : boot_server -> boot_obj -> Amoeba.status
val boot_start_all : boot_server -> Amoeba.status
val boot_status : boot_server -> string * Amoeba.status
val boot_start : 'a -> boot_obj -> Amoeba.status
val boot_stop : 'a -> boot_obj -> Amoeba.status
val boot_restart : 'a -> boot_obj -> Amoeba.status
val boot_loop : server:boot_server -> Amoeba.status
val boot_wait : server:boot_server -> Amoeba.status
val boot_init : boot_def array -> boot_server * Amoeba.status

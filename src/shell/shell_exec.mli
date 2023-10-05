val out : string -> unit
val nl : unit -> unit
type file_type = Exec_binary | Exec_bytecode | Exec_script
and exec_objcap =
    Exec_path of string
  | Exec_cap of Amoeba.capability
  | Exec_capstr of string
  | Exec_nil
and exec_src = Unix_src of exec_objcap | Amoeba_src of exec_objcap | Nil_src
and exec_dst = Unix_dst | Amoeba_dst of exec_objcap | Nil_dst
and exec_type = Unix_type | Amoeba_type | Nil_type
and exec_env =
    Env_cap of (string * Amoeba.capability)
  | Env_cappath of (string * string)
  | Env_capstr of (string * string)
  | Env_str of (string * string)
and cap_env = {
  mutable exec_std_in : Amoeba.capability;
  mutable exec_std_out : Amoeba.capability;
  mutable exec_std_err : Amoeba.capability;
  mutable exec_root : Amoeba.capability;
  mutable exec_tod : Amoeba.capability;
  mutable exec_rand : Amoeba.capability;
} 
and exec_op =
    Exec_poll of (exec_objcap * int)
  | Exec_start
  | Exec_stop
  | Exec_restart
  | Exec_coldstart
and exec_stat =
    Exec_cold
  | Exec_starting
  | Exec_killing
  | Exec_up
  | Exec_down
  | Exec_executed
  | Exec_restarting
  | Exec_unknown
and exec_def = {
  mutable exec_name : string;
  mutable exec_src : exec_src;
  mutable exec_dst : exec_dst;
  mutable exec_args : string list;
  mutable exec_env : exec_env list;
  mutable exec_ops : exec_op list;
} 
and exec_obj = {
  mutable exec_def : exec_def;
  mutable exec_type : exec_type;
  mutable exec_stat : exec_stat;
  mutable exec_tid : int;
  mutable exec_op : exec_op;
  mutable exec_owner : Amoeba.capability;
  mutable exec_poll_cap : Amoeba.capability;
  mutable exec_pid : int;
  mutable exec_proc_cap : Amoeba.capability;
  mutable exec_capenv : cap_env;
  mutable exec_print : string -> unit;
} 
val unix_file_type : string -> Amoeba.status * file_type
val amoeba_file_type : Amoeba.capability -> Amoeba.status * file_type
val max_files : int
val buf_size : int
val exec_warn_obj : exec_obj -> string -> unit
val exec_fatal_obj : exec_obj -> string -> 'a
val exec_info_obj : exec_obj -> string -> unit
val op_start : exec_op list -> bool
val op_coldstart : exec_op list -> bool
val op_stop : exec_op list -> bool
val op_poll : exec_op list -> bool
val op_restart : exec_op list -> bool
val nilcap' : string
val nildef : exec_def
val newobj : unit -> exec_obj
val exec_stop_obj : exec_obj -> Amoeba.status
val exec_control_obj : exec_def -> (string -> unit) -> exec_obj

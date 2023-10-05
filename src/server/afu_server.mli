type afu_type = Unix_file | Memory_file | Not_used
and afu_file = {
  mutable afu_obj : int;
  mutable afu_type : afu_type;
  mutable afu_path : string;
  mutable afu_size : int;
  mutable afu_fd : Unix.file_descr;
  mutable afu_buf : Bytebuf.buffer;
  mutable afu_rand : Amoeba.port;
} 
val nilafu : afu_file
type afu_server = {
  mutable afu_buf_size : int;
  mutable afu_getport : Amoeba.port;
  mutable afu_putport : Amoeba.port;
  mutable afu_verbose : int;
  mutable afu_dying : bool;
  mutable afu_max_files : int;
  mutable afu_files : afu_file array;
  mutable afu_next_obj : int;
  mutable afu_lock : Mutex.t;
} 
val nilafuserver : afu_server
val info : string -> unit
val afu_server_loop : server:afu_server -> unit
val afu_open_file :
  server:afu_server -> filename:string -> Amoeba.status * afu_file
val afu_close_file : server:'a -> afu_obj:afu_file -> unit
val afu_close_all_files : server:afu_server -> unit
val afu_publ_file :
  server:afu_server -> afu_obj:afu_file -> path:string -> Amoeba.status
val afu_unpubl_file : server:'a -> path:string -> Amoeba.status
val afu_file_cap : server:afu_server -> afu_obj:afu_file -> Amoeba.capability
val afu_init : buf_size:int -> max_files:int -> afu_server
val afu_stop : server:afu_server -> Amoeba.status

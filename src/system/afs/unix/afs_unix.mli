module String :
  sig
    external length : string -> int = "%string_length"
    external get : string -> int -> char = "%string_safe_get"
    external set : string -> int -> char -> unit = "%string_safe_set"
    external create : int -> string = "create_string"
    val make : int -> char -> string
    val copy : string -> string
    val sub : string -> pos:int -> len:int -> string
    val fill : string -> pos:int -> len:int -> char -> unit
    val blit :
      src:string ->
      src_pos:int -> dst:string -> dst_pos:int -> len:int -> unit
    val concat : sep:string -> string list -> string
    val iter : f:(char -> unit) -> string -> unit
    val escaped : string -> string
    val index : string -> char -> int
    val rindex : string -> char -> int
    val index_from : string -> int -> char -> int
    val rindex_from : string -> int -> char -> int
    val contains : string -> char -> bool
    val contains_from : string -> int -> char -> bool
    val rcontains_from : string -> int -> char -> bool
    val uppercase : string -> string
    val lowercase : string -> string
    val capitalize : string -> string
    val uncapitalize : string -> string
    external unsafe_get : string -> int -> char = "%string_unsafe_get"
    external unsafe_set : string -> int -> char -> unit
      = "%string_unsafe_set"
    external unsafe_blit :
      src:string ->
      src_pos:int -> dst:string -> dst_pos:int -> len:int -> unit
      = "blit_string" "noalloc"
    external unsafe_fill : string -> pos:int -> len:int -> char -> unit
      = "fill_string" "noalloc"
  end
val server_version : string
val def_block_size : int
val sizeof_int32 : int
val sizeof_int16 : int
type inode = {
  mutable i_file_num : int;
  mutable i_disk_addr : int;
  mutable i_disk_size : int;
  mutable i_disk_res : int;
  mutable i_state : int;
  mutable i_time : int;
  mutable i_random : Amoeba.port;
} 
val nilinode : inode
val inode_SIZE : int
val buf_get_inode : buf:Bytebuf.buffer -> pos:int -> int * inode
val buf_put_inode : buf:Bytebuf.buffer -> pos:int -> inode:inode -> int
val of_state : Afs_server.afs_file_state -> int
val to_state : int -> Afs_server.afs_file_state
val def_inode_ENTRIES : int
val def_inode_SIZE : int
val def_data_ENTRIES : int
val def_data_BUFSIZE : int
val cache_GC_TIME : int
val def_res_SIZE : int
val def_lpath : string
val def_part_inode : string
val def_part_data : string
val magic_str1 : string
val magic_str2 : string
type cache_param = {
  mutable c_inode_buffers : int;
  mutable c_inode_size : int;
  mutable c_data_buffers : int;
  mutable c_data_size : int;
} 
val off_of_inode : int -> int
val to_block : int -> int -> int
val of_block : int -> int -> int
val ceil_block : int -> int -> int
val ceil_block_bytes : int -> int -> int
val floor_block : int -> int -> int
val floor_block_bytes : int -> int -> int
val create_unix_fs :
  label:string ->
  ninodes:int ->
  blocksize:int ->
  nblocks:int ->
  part_inode:string ->
  part_data:string -> overwrite:bool -> Amoeba.status * Afs_server.afs_super
val off_of_inode : int -> int
val block : int -> int -> int
val obj_of_daddr : (int * int) list ref
type afs_stats = {
  mutable op_touch : int;
  mutable op_age : int;
  mutable op_destroy : int;
  mutable op_read : int;
  mutable op_modify : int;
  mutable op_create : int;
  mutable op_commit : int;
  mutable op_sync : int;
} 
val start_unix_fs :
  part_inode:string ->
  part_data:string ->
  cache:cache_param -> Amoeba.status * Afs_server.afs_server
val admin_fs :
  part_inode:string -> part_data:string -> maxsize:int -> mode:int -> unit

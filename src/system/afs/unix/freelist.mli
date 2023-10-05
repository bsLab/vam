val version : string
type cluster_flag = Cluster_FREE | Cluster_USED | Cluster_RESERVED
and free_block = {
  mutable fb_addr : int;
  mutable fb_size : int;
  mutable fb_flag : cluster_flag;
} 
val nilfb : free_block
type free_block_list = {
  mutable body : free_block list;
  mutable tail : free_block list;
  mutable biggest : free_block;
} 
and free_blocks = {
  mutable fbs_array : free_block_list array;
  mutable fbs_range : (int * int) array;
  mutable fbs_num : int;
} 
and free_divide_mode = Free_Bottom | Free_Half | Free_Top
val print_free_list : free_blocks -> unit
val find_biggest : free_block_list -> unit
val _free_divide :
  old:free_block ->
  size:int -> mode:free_divide_mode -> free_block * free_block * free_block
val free_divide :
  old:free_block ->
  size:int -> mode:free_divide_mode -> free_block * free_block * free_block
val free_find : fl:free_blocks -> addr:int -> free_block
val free_insert : fl:free_blocks -> newc:free_block -> unit
val free_merge : fl:free_blocks -> newc:free_block -> unit
val free_append : fl:free_blocks -> addr:int -> size:int -> free_block
val free_new : fl:free_blocks -> size:int -> free_block
val free_match : fl:free_blocks -> size:int -> free_block
val free_release : fl:free_blocks -> addr:int -> unit
val free_create : n:int -> size:int -> free_blocks
val free_compact : free_blocks -> free_blocks
val free_info : free_blocks -> int * int * (int * int * int) array

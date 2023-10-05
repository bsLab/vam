val n_UNDF : Machtype.word8
val n_ABS : Machtype.word8
val n_TEXT : Machtype.word8
val n_DATA : Machtype.word8
val n_BSS : Machtype.word8
val n_COMM : Machtype.word8
val n_FN : Machtype.word8
val n_EXT : Machtype.word8
val n_TYPE : Machtype.word8
val n_STAB : Machtype.word8
val n_AM_MAGIC : Machtype.word32
val n_AM_VERSION : Machtype.word32
val n_AM_FOREIGN : Machtype.word32
type nlist = {
  mutable n_name : string;
  mutable n_strx : Machtype.word32;
  mutable n_type : Machtype.word8;
  mutable n_other : Machtype.word8;
  mutable n_desc : Machtype.word16;
  mutable n_value : Machtype.word32;
} 
val nilnlist : nlist
val buf_get_symtab :
  Bytebuf.buffer -> int -> int * nlist array * Bytebuf.buffer
val buf_put_symtab :
  buf:Bytebuf.buffer ->
  pos:int -> symtab:nlist array -> strtab:Bytebuf.buffer -> int
val print_symtab : nlist array -> unit
val resolve_symtab : nlist array -> Bytebuf.buffer -> unit
val find_sym_name : nlist array -> Machtype.word32 -> string
val find_sym_file : nlist array -> Machtype.word32 -> string
val find_sym_line : nlist array -> Machtype.word32 -> string

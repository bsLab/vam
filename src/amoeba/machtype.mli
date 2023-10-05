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
**    $INITIAL:     (C) 2003 BSSLAB
**    $CREATED:
**    $MODIFIED:
**    $VERSION:     1.02
**
**    $INFO:
**
** Machine type representation, similar to OCaML's int32 and int64
** module, but more general. Remember that OCaML integer are only
** 31/63 bit wide! The last bit is used internally. So, when the
** bit length must be guarenteed, use THIS module.
**
** TODO
**  - 64 bit arithmetic, also on 32 bit machines
**  - signed logical operators are ony correct for 32 bit values
**
**  $ENDINFO
**
*)

(*
** Elementary types
*)

type int8
and int16
and int32
and int64
and uint8
and uint16
and uint32
and uint64
and word8
and word16
and word32
and word64

(*
** Id numbers only for conversion functions
*)

type machtype_id =
    Int8
  | Int16
  | Int32
  | Int64
  | Uint8
  | Uint16
  | Uint32
  | Uint64
  | Word8
  | Word16
  | Word32
  | Word64

(*
** Type Conversion
*)

(*
** Attention: OCaML integers are only 31/63 bit wide! Use instead
** the of_str function to avoid lost of bits!
*)


external to_int : 'a -> int = "unimach_to_int"
external format : string -> 'a -> string = "unimach_format"
val to_str : 'a -> string
external to_data : 'a -> string = "unimach_to_data"
external of_data : string -> machtype_id -> 'a = "unimach_of_data"
external of_int : int -> machtype_id -> 'a = "unimach_of_int"
external of_str : string -> machtype_id -> 'a = "unimach_of_str"
val int32s : string -> int32
val int16s : string -> int16
val int8s : string -> int8
val word32s : string -> word32
val word16s : string -> word16
val word8s : string -> word8
val int32 : int -> int32
val int16 : int -> int16
val int8 : int -> int8
val word32 : int -> word32
val word16 : int -> word16
val word8 : int -> word8
val uint32s : string -> uint32
val uint16s : string -> uint16
val uint8s : string -> uint8
val uint32 : int -> uint32
val uint16 : int -> uint16
val uint8 : int -> uint8
val int : 'a -> int

(*
** Basic arithmetic operations. They can handle both, the unidata
** types and OCaML types like int or float. They replace the
** ordinary operators after this module was opended.
*)

external ( + ) : 'a -> 'a -> 'a = "unimach_add"
external ( - ) : 'a -> 'a -> 'a = "unimach_sub"
external ( * ) : 'a -> 'a -> 'a = "unimach_mul"
external ( / ) : 'a -> 'a -> 'a = "unimach_div"

(*
** Logical operators
*)

external ( land ) : 'a -> 'a -> 'a = "unimach_land"
external ( lor ) : 'a -> 'a -> 'a = "unimach_lor"
external   lnot  : 'a -> 'a = "unimach_lnot"
external ( lsl ) : 'a -> int -> 'a = "unimach_lsl"
external ( lsr ) : 'a -> int -> 'a = "unimach_lsr"
external type_id : 'a -> machtype_id = "unimach_id"
external ( < ) : 'a -> 'a -> bool = "unimach_lt"
external ( > ) : 'a -> 'a -> bool = "unimach_gt"
external ( <= ) : 'a -> 'a -> bool = "unimach_le"
external ( >= ) : 'a -> 'a -> bool = "unimach_ge"

(*
** Buffer management
*)

val buf_put_mach : buf:Bytebuf.buffer -> pos:int -> mach:'a -> int
val buf_get_mach :
  buf:Bytebuf.buffer -> pos:int -> mach:machtype_id -> int * 'a

(*
** Encode and decode from little to bigendian and vice versa...
*)

external dec_be : 'a -> 'a = "unimach_dec_be"
external dec_le : 'a -> 'a = "unimach_dec_le"
external enc_be : 'a -> 'a = "unimach_enc_be"
external enc_le : 'a -> 'a = "unimach_enc_le"

val print_machtype_int8 : int8 -> unit
val print_machtype_int16 : int16 -> unit
val print_machtype_int32 : int32 -> unit
val print_machtype_int64 : int64 -> unit
val print_machtype_uint8 : uint8 -> unit
val print_machtype_uint16 : uint16 -> unit
val print_machtype_uint32 : uint32 -> unit
val print_machtype_uint64 : uint64 -> unit
val print_machtype_word32 : word32 -> unit






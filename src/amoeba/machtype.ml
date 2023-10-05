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
**    $ENDOFINFO
**
*)



open Bytebuf

(*
** All values are stored in generic strings (unidata) in a 
** machine INDEPENDENT way! Therefore, the data section
** can be directly stored in network buffers!
**
** Byte order:
**      H | D1 | D2 ...
**
**          H: Header (type identification)
**          D1: LSB
**          
*)


(*
** Elementary types
*)

type int8
type int16
type int32
type int64
type uint8
type uint16
type uint32
type uint64
type word8
type word16
type word32
type word64


(*
** Id numbers only for conversion functions
*)

type machtype_id =    
        |   Int8 
        |   Int16
        |   Int32
        |   Int64
        |   Uint8
        |   Uint16
        |   Uint32
        |   Uint64
        |   Word8           (* = Uint8  *)
        |   Word16          (* = Uint16 *)
        |   Word32          (* = Uint32 *)
        |   Word64          (* = Uint64 *)


(*
** Type Conversion
*)

external to_int : 'a -> int 
        = "unimach_to_int"


external format : string -> 'a -> string = "unimach_format"
let to_str n = format "0x%x" n



external to_data : 'a -> string 
        = "unimach_to_data"
external of_data : string -> machtype_id -> 'a
        = "unimach_of_data"


(*
** Attention: OCaML integers are only 31/63 bit wide! Use instead
** the of_str function to avoid lost of bits!
*)

external of_int : int -> machtype_id -> 'a 
        = "unimach_of_int"

external of_str : string -> machtype_id -> 'a
        = "unimach_of_str"



let int32s s = let i:int32 = of_str s Int32 in i
let int16s s = let i:int16 = of_str s Int16 in i
let int8s s = let i:int8 = of_str s Int8 in i
let word32s s = let w:word32 = of_str s Word32 in w
let word16s s = let w:word16 = of_str s Word16 in w
let word8s s = let w:word8 = of_str s Word8 in w
let int32 d = let i:int32 = of_int d Int32 in i
let int16 d = let i:int16 = of_int d Int16 in i
let int8 d = let i:int8 = of_int d Int8 in i
let word32 d = let w:word32 = of_int d Word32 in w
let word16 d = let w:word16 = of_int d Word16 in w
let word8 d = let w:word8 = of_int d Word8 in w
let uint32s s = let u:uint32 = of_str s Uint32 in u
let uint16s s = let u:uint16 = of_str s Uint16 in u
let uint8s s = let u:uint8 = of_str s Uint8 in u
let uint32 d = let u:uint32 = of_int d Uint32 in u
let uint16 d = let u:uint16 = of_int d Uint16 in u
let uint8 d = let u:uint8 = of_int d Uint8 in u
let int u = to_int u
 



(*
** Basic arithmetic operations. They can handle both, the unidata
** types and OCaML types like int or float. They replace the
** ordinary operators after this module was opended.
*)


external ( + ): 'a -> 'a -> 'a = "unimach_add"
external ( - ): 'a -> 'a -> 'a = "unimach_sub"
external ( * ): 'a -> 'a -> 'a = "unimach_mul"
external ( / ): 'a -> 'a -> 'a = "unimach_div"


(*
** Logical operators
*)

external ( land ): 'a -> 'a -> 'a = "unimach_land"
external ( lor  ): 'a -> 'a -> 'a = "unimach_lor"
external  lnot   : 'a -> 'a = "unimach_lnot"
external ( lsl  ): 'a -> int -> 'a = "unimach_lsl"
external ( lsr  ): 'a -> int -> 'a = "unimach_lsr"


external type_id : 'a -> machtype_id = "unimach_id"
external ( < ) : 'a -> 'a -> bool = "unimach_lt"
external ( > ) : 'a -> 'a -> bool = "unimach_gt"
external ( <= ) : 'a -> 'a -> bool = "unimach_le"
external ( >= ) : 'a -> 'a -> bool = "unimach_ge"
    

(*
** Buffer management
*)

let buf_put_mach ~buf ~pos ~mach =
    let data = to_data mach in
    let len = String.length data in
    blit_sb ~src:data ~src_pos:0 
            ~dst:buf ~dst_pos:pos 
            ~len:len;
    (pos+len)
    
let buf_get_mach ~buf ~pos ~mach =
    let len = 
        match mach with
        | Int8  | Uint8 | Word8 -> 1
        | Int16 | Uint16 | Word16 -> 2 
        | Int32 | Uint32 | Word32 -> 4 
        | _ -> failwith "buf_get_mach: unsupported type";
    in
    let data = String.create len in
    blit_bs ~src:buf ~src_pos:pos
            ~dst:data ~dst_pos:0
            ~len:len;
    (pos+len),(of_data data mach)



(*                                                       
** Encode and decode from little to bigendian and vice versa...
*)    

external dec_be : 'a -> 'a = "unimach_dec_be"
external dec_le : 'a -> 'a = "unimach_dec_le"
external enc_be : 'a -> 'a = "unimach_enc_be"
external enc_le : 'a -> 'a = "unimach_enc_le"



#if 0
external abs: 'a -> 'a = "unimach_abs" ;;
external (~-): 'a -> 'a = "unimach_neg" ;;
external conv : unidata -> t -> unidata
        = "unimach_conv"
#endif


(*
** Printers
*)
open Format

let print_machtype_int8 s =
    open_hvbox 0;
    print_string ("Machinetype '" ^ (to_str s) ^ "'");
    close_box ()
let print_machtype_int16 s =
    open_hvbox 0;
    print_string ("Machinetype '" ^ (to_str s) ^ "'");
    close_box ()
let print_machtype_int32 s =
    open_hvbox 0;
    print_string ("Machinetype '" ^ (to_str s) ^ "'");
    close_box ()
let print_machtype_int64 s =
    open_hvbox 0;
    print_string ("Machinetype '" ^ (to_str s) ^ "'");
    close_box ()
let print_machtype_uint8 s =
    open_hvbox 0;
    print_string ("Machinetype '" ^ (to_str s) ^ "'");
    close_box ()
let print_machtype_uint16 s =
    open_hvbox 0;
    print_string ("Machinetype '" ^ (to_str s) ^ "'");
    close_box ()
let print_machtype_uint32 s =
    open_hvbox 0;
    print_string ("Machinetype '" ^ (to_str s) ^ "'");
    close_box ()
let print_machtype_word32 s =
    open_hvbox 0;
    print_string ("Machinetype '" ^ (to_str s) ^ "'");
    close_box ()
let print_machtype_uint64 s =
    open_hvbox 0;
    print_string ("Machinetype '" ^ (to_str s) ^ "'");
    close_box ()

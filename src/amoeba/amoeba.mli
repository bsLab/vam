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
**    $CREATED:     2001.00.00
**    $VERSION:     1.43
**
**    $INFO:
**
**  Amoeba basic specifications 
**
**  Some notes: extra care must be taken due to multi threaded programming.
**  OCaML tends to use "variables" in more than one structure. But different
**  threads need different variable storage space, for example in header
**  structures. Therfore, functions allocating new strucutures like headers
**  are outsourced to 'amoeba_ext.c'. 
**  Use always the XX_new functions to get a real fresh and unique
**  data structure.
**
**    $ENDOFINFO
**
*)


(*
** Amoeba basic specifications
*)


(*
** Real size of an Amoeba basic types in bytes, as hopefully
** used by the C libs.
*)

val port_SIZE : int
val oob_SIZE : int
val header_SIZE : int
val priv_SIZE : int
val cap_SIZE : int
val objnum_SIZE : int
val rightsbits_SIZE : int

val int16_SIZE: int
val int32_SIZE: int

val max_PATHLEN: int

(*
** Used in headers and capabilities
*)

type rights_bits = Rights_bits of int
and obj_num = Objnum of int
and command = Command of int
and errstat = Errstat of int
and status = Status of int

exception Error of status

(*
** For now the capability only holds 24 rights bits
*)

val max_objnum : obj_num

(*
** Amoeba port represented externally as a string of length
** portsize
*)

type port = Port of string

(*
** private is a caml keyword -> use privat instead
*)

and privat = {
  mutable prv_object : obj_num;
  mutable prv_rights : rights_bits;
  mutable prv_random : port;
} 

(*
** Amoeba Capability
*)

and capability = { mutable cap_port : port; mutable cap_priv : privat; } 

(*
** RPC header
*)

and header = {
  mutable h_port : port;
  mutable h_priv : privat;
  mutable h_command : command;
  mutable h_status : status;
  mutable h_offset : int;
  mutable h_size : int;
  mutable h_extra : int;
} 
(*
** Some externals
*)

external ext_hdr_new : unit -> header = "ext_hdr_new"
external ext_port_new : unit -> port = "ext_port_new"
external ext_priv_new : unit -> privat = "ext_priv_new"
external ext_cap_new : unit -> capability = "ext_cap_new"


(*
** Create a fresh port and copy the content of the original one.
*)

val port_copy : port -> port

(*
** Create a fresh private structure and copy the content of the original one.
*)

val priv_copy : privat -> privat

(*
** Create a fresh capability and copy the content of the original one.
*)

val cap_copy : capability -> capability

(*
** Zero port, capability, private
*)

val nilport : port
val nilcap : capability
val nilpriv : privat
val nilheader: header

(*
** Return a new port initially empty
*)

val port_new : unit -> port

(*
** Return a new private structure initially empty
*)

val priv_new : unit -> privat

(*
** Return a new capabiliy initially empty
*)

val cap_new : unit -> capability

(*
** Utilities to copy command and status values.
*)

val cmd_copy : command -> command
val stat_copy : status -> status

(*
** Return a new header initially empty
*)

val header_new : unit -> header

(*
** Copy all from header h1 to header h2
*)
val header_copy : header -> header -> unit

(*
** Compare two ports: return true if they are fully equal
*)

val portcmp : port -> port -> bool
val nullport : port -> bool

(*
** Utils to get and set single bytes of a port
*)

val get_portbyte : port:port -> byte:int -> int
val set_portbyte : port:port -> byte:int -> value:int -> unit

(*
** Encrpytion
*)

val one_way : port -> port

(*
** Convert a private port to a public port (get-port to put-port).
*)
val priv2pub : getport:port -> port

(*
** Private decoding and encoding
*)

val prv_all_rights : rights_bits

(*
** Operations with Rights_bits
*)

val rights_and : rights_bits -> rights_bits -> rights_bits 
val rights_or  : rights_bits -> rights_bits -> rights_bits 
val rights_xor : rights_bits -> rights_bits -> rights_bits 
val rights_not : rights_bits -> rights_bits 

(*
** Check wether the required rights [R1;R2;..] are
** present in the rights field rg. Return a boolean value.
*)

val rights_req : rights:rights_bits -> required:rights_bits list -> bool

(*
** Set rights bits [R1;R2;...]
*)

val rights_set : rights_bits list -> rights_bits

(*
** Decode a private structure.
*)
val prv_decode : prv:privat -> rand:port -> bool

(*
** Encode a private part from the object number, the rights field
** and the random port.
** Returns the created private structure.
*)

val prv_encode : obj:obj_num -> rights:rights_bits -> rand:port -> privat

(*
** Return the private object number form a private structure
*)

val prv_number : privat -> obj_num

(*
** Return the private rights field.
*)

val prv_rights : privat -> rights_bits

(*
** Return a new random port.
**
** Warning: the quality of the random ports are strongly
** related with CaML's underlying random generator. Be warned!
*)

val uniqport : unit -> port


(*
** Convert a string to a port (aka named ports).
*)

val port_of_str : string -> port

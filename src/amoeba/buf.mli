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
**    $AUTHORS:     
**    $INITIAL:     (C)
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.08
**
**    $INFO:
**
** Machine independent storing and extracting
** of Amoeba structures in and from buffers with bound checking.
**
**
**    $ENDOFINFO
**
*)


open Amoeba
open Bytebuf

(*
** Put a short int8 value into a buffer -> machine independent
*)
val buf_put_int8 : buf:buffer -> pos:int -> int8:int -> int

(*
** Put a short int16 value into a buffer -> machine dependent
*)
val buf_put_int16 : buf:buffer -> pos:int -> int16:int -> int


(*
** Put an integer variable into a buffer. Warning: OCaML's
** integers are only 31/63 bit wide!
*)
val buf_put_int32 : buf:buffer -> pos:int -> int32:int -> int


(*
** Put a string in a buffer - simple, but dont't forget to add
** an EOS char '\000' !
*)
val buf_put_string : buf:buffer -> pos:int -> str:string -> int

(*
** Put a port
*)
val buf_put_port : buf:buffer -> pos:int -> port:Amoeba.port -> int

(*
** Put a private structure
*)
val buf_put_priv : buf:buffer -> pos:int -> priv:Amoeba.privat -> int

(*
** Put a capability
*)
val buf_put_cap :
  buf:buffer -> pos:int -> cap:Amoeba.capability -> int

(*
** Put a capset
*)
val buf_put_capset : buf:buffer -> pos:int -> cs:Capset.capset -> int

(*
** Put right bits in a buffer
*)
val buf_put_right_bits : buf:buffer -> pos:int -> right:int -> int
val buf_put_rights_bits : buf:buffer -> pos:int ->
                          rights:Amoeba.rights_bits -> int

(*
** Get a short int8 value from a buffer -> machine independent
*)
val buf_get_int8 : buf:buffer -> pos:int -> int * int

(*
** Get a short int16 value from a buffer -> machine dependent
*)
val buf_get_int16 : buf:buffer -> pos:int -> int * int

(*
** Get an integer value from a buffer -> machine dependent
** Warning: OCaML's integers are only 31/63 bit wide.
*)
val buf_get_int32 : buf:buffer -> pos:int -> int * int


(*
** Strings
*)
val buf_get_string : buf:buffer -> pos:int -> int * string

(*
** Get a port
*)
val buf_get_port : buf:buffer -> pos:int -> int * Amoeba.port

(*
** Get a private structure
*)
val buf_get_priv : buf:buffer -> pos:int -> int * Amoeba.privat

(*
** Get a capability
*)
val buf_get_cap : buf:buffer -> pos:int -> int * Amoeba.capability

(*
** Extract a capset
*)
val buf_get_capset : buf:buffer -> pos:int -> int * Capset.capset

(*
** Get right bits from a buffer
*)
val buf_get_right_bits : buf:buffer -> pos:int -> int * int 
val buf_get_rights_bits : buf:buffer -> pos:int -> 
                          int * Amoeba.rights_bits 


(*
** File utils
*)

val read_cap : string -> status * capability
val write_cap : string -> capability -> status

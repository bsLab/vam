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
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2005 BSSLAB
**    $CREATED:     17.7.2005
**    $VERSION:     1.01
**
**    $INFO:
**
**  Matrix implementation. All indices starts with value 0!!!
**
**    $ENDOFINFO
**
*)

type 'a t
(*
** Matrix data type. Keep consistent with matrix.h (mtrix_type).
*)
type matrix_type =
    | M_auto
    | M_float       (* caml double          *)
    | M_complex     (* (M_float,M_float) tuples *)      
    | M_float32     (* 4 byte float         *)
    | M_float64     (* 8 byte float         *)
    | M_int         (* caml integer         *)
    | M_int8        (* machtype int8        *)
    | M_int16       (* machtype int16       *)
    | M_int32       (* machtype int32       *)
    | M_int64       (* machtype int64       *)
    | M_byte        (* caml char <=> int    *)

(*
** Create a matrix with given type and intializer all matrix entries.
**
**  create_type <size tuple> <type> <init value>
**
**  let m = create_type (10,20) M_int 0
*)

external create_type : 'a -> matrix_type -> 'b -> 'b t
    = "matrix_create"

external info : 'a t -> string
    = "matrix_info"

(* 
** Create a matrix with type determined by init value:
**  
**  create <size tuple> <init value>
**
**  let m = create (10,20,30) 0.0
*)

val create : 'a -> 'b -> 'b t

(*
** Get a value from the matrix:
**
**  get <matrix> <entry index tuple>
**
**  get m (5,5)
*)

external get : 'a t -> 'b -> 'a
    = "matrix_get"

external set : 'a t -> 'b -> 'a -> unit
    = "matrix_set"

(*
** To provide: m.{i,j,k}...
*)

module Dimn :
sig
    external get : 'a t -> 'b -> 'a
            = "matrix_get"
    external set : 'a t -> 'b -> 'a -> unit
            = "matrix_set"
end

(*
** Return size dimenion array
*)
external dim : 'a t  -> int array
    = "matrix_dim"

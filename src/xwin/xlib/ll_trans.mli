(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)




exception BrokenConnection

val read_channel: chan:Xtypes.x_channel -> 
                  off:int -> 
                  buf:string -> 
                  len:int -> unit
val write_channel: chan:Xtypes.x_channel -> 
                   off:int -> 
                   buf:string -> 
                   len:int -> unit

exception NotADigit of int

val discardInt : string -> int -> int * int
val parseDisplayName : string -> string * int * int

val openConnectionWithDisplay :
    string -> Xtypes.x_channel * string * int * (string * string)

val closeConnection: Xtypes.x_channel -> unit

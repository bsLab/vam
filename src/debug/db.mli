
val dblevel : int ref 
module Pr :
  sig
    val s : int -> string -> unit
    val ss : int -> string -> string -> unit
    val sss : int -> string -> string -> string -> unit
    val sd : int -> string -> int -> unit
    val sdd : int -> string -> int -> int -> unit
    val sddd : int -> string -> int -> int -> int -> unit
    val sdddd : int -> string -> int -> int -> int -> int -> unit
  end
val set_level : int -> unit 

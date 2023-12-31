(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: int64.ml,v 1.8 2001/12/07 13:40:52 xleroy Exp $ *)

(* Module [Int64]: 64-bit integers *)

external neg : int64 -> int64 = "%int64_neg"
external add : int64 -> int64 -> int64 = "%int64_add"
external sub : int64 -> int64 -> int64 = "%int64_sub"
external mul : int64 -> int64 -> int64 = "%int64_mul"
external div : int64 -> int64 -> int64 = "%int64_div"
external rem : int64 -> int64 -> int64 = "%int64_mod"
external logand : int64 -> int64 -> int64 = "%int64_and"
external logor : int64 -> int64 -> int64 = "%int64_or"
external logxor : int64 -> int64 -> int64 = "%int64_xor"
external shift_left : int64 -> int -> int64 = "%int64_lsl"
external shift_right : int64 -> int -> int64 = "%int64_asr"
external shift_right_logical : int64 -> int -> int64 = "%int64_lsr"
external of_int : int -> int64 = "%int64_of_int"
external to_int : int64 -> int = "%int64_to_int"
external of_float : float -> int64 = "int64_of_float"
external to_float : int64 -> float = "int64_to_float"
external of_int32 : int32 -> int64 = "%int64_of_int32"
external to_int32 : int64 -> int32 = "%int64_to_int32"
external of_nativeint : nativeint -> int64 = "%int64_of_nativeint"
external to_nativeint : int64 -> nativeint = "%int64_to_nativeint"

let zero = try of_int 0 with Invalid_argument _ -> Obj.magic Int32.zero
let one = try of_int 1 with Invalid_argument _ -> Obj.magic Int32.one
let minus_one = try of_int (-1) with Invalid_argument _ -> Obj.magic Int32.minus_one
let succ n = add n one
let pred n = sub n one
let abs n = if n >= zero then n else neg n
let min_int =
  try shift_left one 63 with Invalid_argument _ -> Obj.magic Int32.min_int
let max_int =
  try sub min_int one with Invalid_argument _ -> Obj.magic Int32.max_int
let lognot n = logxor n minus_one

external format : string -> int64 -> string = "int64_format"
let to_string n = format "%d" n

external of_string : string -> int64 = "int64_of_string"

external bits_of_float : float -> int64 = "int64_bits_of_float"
external float_of_bits : int64 -> float = "int64_float_of_bits"

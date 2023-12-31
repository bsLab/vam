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

(* $Id: char.mli,v 1.15 2001/12/07 13:40:50 xleroy Exp $ *)

(** Character operations. *)

external code : char -> int = "%identity"
(** Return the ASCII code of the argument. *)
        
val chr : int -> char
(** Return the character with the given ASCII code.
   Raise [Invalid_argument "Char.chr"] if the argument is
   outside the range 0--255. *)

val escaped : char -> string
(** Return a string representing the given character,
   with special characters escaped following the lexical conventions
   of Objective Caml. *)

val lowercase : char -> char
(** Convert the given character to its equivalent lowercase character. *)

val uppercase : char -> char
(** Convert the given character to its equivalent uppercase character. *)

(**/**)

external unsafe_chr : int -> char = "%identity"

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: matching.mli,v 1.11 2001/02/19 20:27:35 maranget Exp $ *)

(* Compilation of pattern-matching *)

open Typedtree
open Lambda

val for_function:
        Location.t -> int ref option -> lambda -> (pattern * lambda) list ->
        partial -> lambda
val for_trywith:
        lambda -> (pattern * lambda) list -> lambda
val for_let:
        Location.t -> lambda -> pattern -> lambda -> lambda
val for_multiple_match:
        Location.t -> lambda list -> (pattern * lambda) list -> partial ->
        lambda

val for_tupled_function:
        Location.t -> Ident.t list -> (pattern list * lambda) list ->
        partial -> lambda

exception Cannot_flatten

val flatten_pattern: int -> pattern -> pattern list

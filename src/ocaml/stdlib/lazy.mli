(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: lazy.mli,v 1.8 2001/12/07 13:40:53 xleroy Exp $ *)

(** Deferred computations. *)

type 'a status = 
    Delayed of (unit -> 'a) 
  | Value of 'a 
  | Exception of exn

type 'a t = 'a status ref
(** A value of type ['a Lazy.t] is a deferred computation (also called a
   suspension) that computes a result of type ['a].  The expression
   [lazy (expr)] returns a suspension that computes [expr]. **)


exception Undefined

val force : 'a t -> 'a
(** [Lazy.force x] computes the suspension [x] and returns its result.
   If the suspension was already computed, [Lazy.force x] returns the
   same value again.  If it raised an exception, the same exception is
   raised again.
   Raise [Undefined] if the evaluation of the suspension requires its
   own result.
*)


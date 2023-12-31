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

(* $Id: topdirs.mli,v 1.10 2001/07/03 11:04:10 xleroy Exp $ *)

(* The toplevel directives. *)

open Format

val dir_quit : unit -> unit
val dir_directory : string -> unit
val dir_cd : string -> unit
val dir_load : formatter -> string -> unit
val dir_use : formatter -> string -> unit
val dir_install_printer : formatter -> Longident.t -> unit
val dir_remove_printer : formatter -> Longident.t -> unit
val dir_trace : formatter -> Longident.t -> unit
val dir_untrace : formatter -> Longident.t -> unit
val dir_untrace_all : formatter -> unit -> unit

type 'a printer_type_new = Format.formatter -> 'a -> unit
type 'a printer_type_old = 'a -> unit


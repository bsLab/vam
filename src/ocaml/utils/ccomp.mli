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

(* $Id: ccomp.mli,v 1.7 1999/11/17 18:59:01 xleroy Exp $ *)

(* Compiling C files and building C libraries *)

val command: string -> int
val run_command: string -> unit
val compile_file: string -> int
val create_archive: string -> string list -> int
val expand_libname: string -> string

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

(* $Id: typemod.mli,v 1.18 2000/03/06 22:12:08 weis Exp $ *)

(* Type-checking of the module language *)

open Types
open Format

val type_module:
        Env.t -> Parsetree.module_expr -> Typedtree.module_expr
val type_structure:
        Env.t -> Parsetree.structure -> Typedtree.structure * signature * Env.t
val type_implementation:
        string -> string -> string -> Env.t -> Parsetree.structure ->
                               Typedtree.structure * Typedtree.module_coercion
val transl_signature:
        Env.t -> Parsetree.signature -> signature
val check_nongen_schemes:
        Env.t -> Typedtree.structure -> unit

type error =
    Unbound_module of Longident.t
  | Unbound_modtype of Longident.t
  | Cannot_apply of module_type
  | Not_included of Includemod.error list
  | Cannot_eliminate_dependency of module_type
  | Signature_expected
  | Structure_expected of module_type
  | With_no_component of Longident.t
  | With_mismatch of Longident.t * Includemod.error list
  | Repeated_name of string * string
  | Non_generalizable of type_expr
  | Non_generalizable_class of Ident.t * class_declaration
  | Non_generalizable_module of module_type

exception Error of Location.t * error

val report_error: formatter -> error -> unit

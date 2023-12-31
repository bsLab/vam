(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*     Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: outcometree.mli,v 1.6 2001/09/25 09:54:16 garrigue Exp $ *)

(* Module [Outcometree]: results displayed by the toplevel *)

(* These types represent messages that the toplevel displays as normal
   results or errors. The real displaying is customisable using the hooks:
      [Toploop.print_out_value]
      [Toploop.print_out_type]
      [Toploop.print_out_sig_item]
      [Toploop.print_out_phrase] *)

type out_ident =
  | Oide_apply of out_ident * out_ident
  | Oide_dot of out_ident * string
  | Oide_ident of string

type out_value =
  | Oval_array of out_value list
  | Oval_char of char
  | Oval_constr of out_ident * out_value list
  | Oval_ellipsis
  | Oval_float of float
  | Oval_int of int
  | Oval_list of out_value list
  | Oval_printer of (Format.formatter -> unit)
  | Oval_record of (out_ident * out_value) list
  | Oval_string of string
  | Oval_stuff of string
  | Oval_tuple of out_value list
  | Oval_variant of string * out_value option

type out_type =
  | Otyp_abstract
  | Otyp_alias of out_type * string
  | Otyp_arrow of string * out_type * out_type
  | Otyp_class of bool * out_ident * out_type list
  | Otyp_constr of out_ident * out_type list
  | Otyp_manifest of out_type * out_type
  | Otyp_object of (string * out_type) list * bool option
  | Otyp_record of (string * bool * out_type) list
  | Otyp_stuff of string
  | Otyp_sum of (string * out_type list) list
  | Otyp_tuple of out_type list
  | Otyp_var of bool * string
  | Otyp_variant of
      bool * out_variant * bool * (string list) option
and out_variant =
  | Ovar_fields of (string * bool * out_type list) list
  | Ovar_name of out_ident * out_type list

type out_class_type =
  | Octy_constr of out_ident * out_type list
  | Octy_fun of string * out_type * out_class_type
  | Octy_signature of out_type option * out_class_sig_item list
and out_class_sig_item =
  | Ocsg_constraint of out_type * out_type
  | Ocsg_method of string * bool * bool * out_type
  | Ocsg_value of string * bool * out_type

type out_module_type =
  | Omty_abstract
  | Omty_functor of string * out_module_type * out_module_type
  | Omty_ident of out_ident
  | Omty_signature of out_sig_item list
and out_sig_item =
  | Osig_class of bool * string * string list * out_class_type
  | Osig_class_type of bool * string * string list * out_class_type
  | Osig_exception of string * out_type list
  | Osig_modtype of string * out_module_type
  | Osig_module of string * out_module_type
  | Osig_type of out_type_decl list
  | Osig_value of string * out_type * string list
and out_type_decl =
  string * (string * (bool * bool)) list * out_type *
  (out_type * out_type) list

type out_phrase =
  | Ophr_eval of out_value * out_type
  | Ophr_signature of (out_sig_item * out_value option) list
  | Ophr_exception of (exn * out_value)

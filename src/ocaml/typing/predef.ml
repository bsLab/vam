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

(* $Id: predef.ml,v 1.21 2000/12/04 15:37:02 doligez Exp $ *)

(* Predefined type constructors (with special typing rules in typecore) *)

open Path
open Types
open Btype

let ident_int = Ident.create "int"
and ident_char = Ident.create "char"
and ident_string = Ident.create "string"
and ident_float = Ident.create "float"
and ident_bool = Ident.create "bool"
and ident_unit = Ident.create "unit"
and ident_exn = Ident.create "exn"
and ident_array = Ident.create "array"
and ident_list = Ident.create "list"
and ident_format = Ident.create "format"
and ident_option = Ident.create "option"
and ident_nativeint = Ident.create "nativeint"
and ident_int32 = Ident.create "int32"
and ident_int64 = Ident.create "int64"

let path_int = Pident ident_int
and path_char = Pident ident_char
and path_string = Pident ident_string
and path_float = Pident ident_float
and path_bool = Pident ident_bool
and path_unit = Pident ident_unit
and path_exn = Pident ident_exn
and path_array = Pident ident_array
and path_list = Pident ident_list
and path_format = Pident ident_format
and path_option = Pident ident_option
and path_nativeint = Pident ident_nativeint
and path_int32 = Pident ident_int32
and path_int64 = Pident ident_int64

let type_int = newgenty (Tconstr(path_int, [], ref Mnil))
and type_char = newgenty (Tconstr(path_char, [], ref Mnil))
and type_string = newgenty (Tconstr(path_string, [], ref Mnil))
and type_float = newgenty (Tconstr(path_float, [], ref Mnil))
and type_bool = newgenty (Tconstr(path_bool, [], ref Mnil))
and type_unit = newgenty (Tconstr(path_unit, [], ref Mnil))
and type_exn = newgenty (Tconstr(path_exn, [], ref Mnil))
and type_array t = newgenty (Tconstr(path_array, [t], ref Mnil))
and type_list t = newgenty (Tconstr(path_list, [t], ref Mnil))
and type_option t = newgenty (Tconstr(path_option, [t], ref Mnil))
and type_nativeint = newgenty (Tconstr(path_nativeint, [], ref Mnil))
and type_int32 = newgenty (Tconstr(path_int32, [], ref Mnil))
and type_int64 = newgenty (Tconstr(path_int64, [], ref Mnil))

let ident_match_failure = Ident.create "Match_failure"
and ident_out_of_memory = Ident.create "Out_of_memory"
and ident_invalid_argument = Ident.create "Invalid_argument"
and ident_failure = Ident.create "Failure"
and ident_not_found = Ident.create "Not_found"
and ident_sys_error = Ident.create "Sys_error"
and ident_end_of_file = Ident.create "End_of_file"
and ident_division_by_zero = Ident.create "Division_by_zero"
and ident_stack_overflow = Ident.create "Stack_overflow"
and ident_sys_blocked_io = Ident.create "Sys_blocked_io"
and ident_assert_failure = Ident.create "Assert_failure"

let path_match_failure = Pident ident_match_failure
and path_assert_failure = Pident ident_assert_failure

let build_initial_env add_type add_exception empty_env =
  let decl_abstr =
    {type_params = [];
     type_arity = 0;
     type_kind = Type_abstract;
     type_manifest = None;
     type_variance = []}
  and decl_bool =
    {type_params = [];
     type_arity = 0;
     type_kind = Type_variant["false",[]; "true",[]];
     type_manifest = None;
     type_variance = []}
  and decl_unit =
    {type_params = []; 
     type_arity = 0;
     type_kind = Type_variant["()",[]];
     type_manifest = None;
     type_variance = []}
  and decl_exn =
    {type_params = [];
     type_arity = 0;
     type_kind = Type_variant [];
     type_manifest = None;
     type_variance = []}
  and decl_array =
    let tvar = newgenvar() in
    {type_params = [tvar];
     type_arity = 1;
     type_kind = Type_abstract;
     type_manifest = None;
     type_variance = [true, true]}
  and decl_list =
    let tvar = newgenvar() in
    {type_params = [tvar];
     type_arity = 1;
     type_kind = Type_variant["[]", []; "::", [tvar; type_list tvar]];
     type_manifest = None;
     type_variance = [true, false]}
  and decl_format =
    {type_params = [newgenvar(); newgenvar(); newgenvar()];
     type_arity = 3;
     type_kind = Type_abstract;
     type_manifest = None;
     type_variance = [true, true; true, true; true, true]}
  and decl_option =
    let tvar = newgenvar() in
    {type_params = [tvar];
     type_arity = 1;
     type_kind = Type_variant["None", []; "Some", [tvar]];
     type_manifest = None;
     type_variance = [true, false]}
  in

  add_exception ident_match_failure
                         [newgenty (Ttuple[type_string; type_int; type_int])] (
  add_exception ident_out_of_memory [] (
  add_exception ident_stack_overflow [] (
  add_exception ident_invalid_argument [type_string] (
  add_exception ident_failure [type_string] (
  add_exception ident_not_found [] (
  add_exception ident_sys_blocked_io [] (
  add_exception ident_sys_error [type_string] (
  add_exception ident_end_of_file [] (
  add_exception ident_division_by_zero [] (
  add_exception ident_assert_failure
                         [newgenty (Ttuple[type_string; type_int; type_int])] (
  add_type ident_int64 decl_abstr (
  add_type ident_int32 decl_abstr (
  add_type ident_nativeint decl_abstr (
  add_type ident_option decl_option (
  add_type ident_format decl_format (
  add_type ident_list decl_list (
  add_type ident_array decl_array (
  add_type ident_exn decl_exn (
  add_type ident_unit decl_unit (
  add_type ident_bool decl_bool (
  add_type ident_float decl_abstr (
  add_type ident_string decl_abstr (
  add_type ident_char decl_abstr (
  add_type ident_int decl_abstr (
    empty_env)))))))))))))))))))))))))

let builtin_values =
  List.map (fun id -> Ident.make_global id; (Ident.name id, id))
      [ident_match_failure; ident_out_of_memory; ident_stack_overflow;
       ident_invalid_argument;
       ident_failure; ident_not_found; ident_sys_error; ident_end_of_file;
       ident_division_by_zero; ident_sys_blocked_io;
       ident_assert_failure ]

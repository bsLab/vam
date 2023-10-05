(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: typeopt.ml,v 1.6 2001/01/13 11:55:09 xleroy Exp $ *)

(* Auxiliaries for type-based optimizations, e.g. array kinds *)

open Misc
open Asttypes
open Primitive
open Path
open Types
open Typedtree
open Lambda

let has_base_type exp base_ty_path =
  let exp_ty =
    Ctype.expand_head exp.exp_env (Ctype.correct_levels exp.exp_type) in
  match Ctype.repr exp_ty with
    {desc = Tconstr(p, _, _)} -> Path.same p base_ty_path
  | _ -> false

let maybe_pointer exp =
  let exp_ty =
    Ctype.expand_head exp.exp_env (Ctype.correct_levels exp.exp_type) in
  match (Ctype.repr exp_ty).desc with
    Tconstr(p, args, abbrev) ->
      not (Path.same p Predef.path_int) &&
      not (Path.same p Predef.path_char) &&
      begin try
        match Env.find_type p exp.exp_env with
          {type_kind = Type_variant cstrs} ->
            List.exists (fun (name, args) -> args <> []) cstrs
        | _ -> true
      with Not_found -> true
        (* This can happen due to e.g. missing -I options,
           causing some .cmi files to be unavailable.
           Maybe we should emit a warning. *)
      end
  | _ -> true

let array_element_kind env ty =
  let ty = Ctype.repr (Ctype.expand_head env ty) in
  match ty.desc with
    Tvar ->
      Pgenarray
  | Tconstr(p, args, abbrev) ->
      if Path.same p Predef.path_int || Path.same p Predef.path_char then
        Pintarray
      else if Path.same p Predef.path_float then
        Pfloatarray
      else if Path.same p Predef.path_string
           || Path.same p Predef.path_array 
           || Path.same p Predef.path_nativeint
           || Path.same p Predef.path_int32
           || Path.same p Predef.path_int64 then
        Paddrarray
      else begin
        try
          match Env.find_type p env with
            {type_kind = Type_abstract} ->
              Pgenarray
          | {type_kind = Type_variant cstrs}
            when List.for_all (fun (name, args) -> args = []) cstrs ->
              Pintarray
          | {type_kind = _} ->
              Paddrarray
        with Not_found ->
          (* This can happen due to e.g. missing -I options,
             causing some .cmi files to be unavailable.
             Maybe we should emit a warning. *)
          Pgenarray
      end
  | _ ->
      Paddrarray

let array_kind_gen ty env =
  let array_ty = Ctype.expand_head env (Ctype.correct_levels ty) in
  match (Ctype.repr array_ty).desc with
    Tconstr(p, [elt_ty], _) when Path.same p Predef.path_array ->
      array_element_kind env elt_ty
  | _ ->
      (* This can happen with e.g. Obj.field *)
      Pgenarray

let array_kind exp = array_kind_gen exp.exp_type exp.exp_env

let array_pattern_kind pat = array_kind_gen pat.pat_type pat.pat_env

let matrix_decode_type ty tbl dfl =
  match (Ctype.repr ty).desc with
    Tconstr(Pdot(Pident mod_id, type_name, _), [], _)
    when Ident.name mod_id = "Matrix" ->
      begin try List.assoc type_name tbl with Not_found -> dfl end
  | _ ->
      dfl

let kind_table =
  ["float32_elt", Pmatrix_float32;
   "float64_elt", Pmatrix_float64;
   "int8_signed_elt", Pmatrix_sint8;
   "int8_unsigned_elt", Pmatrix_uint8;
   "int16_signed_elt", Pmatrix_sint16;
   "int16_unsigned_elt", Pmatrix_uint16;
   "int32_elt", Pmatrix_int32;
   "int64_elt", Pmatrix_int64;
   "int_elt", Pmatrix_caml_int;
   "nativeint_elt", Pmatrix_native_int]

let layout_table =
  ["c_layout", Pmatrix_c_layout;
   "fortran_layout", Pmatrix_fortran_layout]

let matrix_kind_and_layout exp =
  let ty = Ctype.repr (Ctype.expand_head exp.exp_env exp.exp_type) in
  match ty.desc with
    Tconstr(p, [caml_type; elt_type; layout_type], abbrev) ->
      (matrix_decode_type elt_type kind_table Pmatrix_unknown,
       matrix_decode_type layout_type layout_table Pmatrix_unknown_layout)
  | _ ->
      (Pmatrix_unknown, Pmatrix_unknown_layout)

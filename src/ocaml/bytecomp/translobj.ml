(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: translobj.ml,v 1.6 1999/11/17 18:57:03 xleroy Exp $ *)

open Misc
open Asttypes
open Longident
open Lambda

(* Get oo primitives identifiers *)

let oo_prim name =
  try
    transl_path
      (fst (Env.lookup_value (Ldot (Lident "Oo", name)) Env.empty))
  with Not_found ->
    fatal_error ("Primitive " ^ name ^ " not found.")

(* Collect labels *)

let used_methods = ref ([] : (string * Ident.t) list);;

let meth lab =
  try
    List.assoc lab !used_methods
  with Not_found ->
    let id = Ident.create lab in
    used_methods := (lab, id)::!used_methods;
    id

let reset_labels () =
  used_methods := []

(* Insert labels *)

let string s = Lconst (Const_base (Const_string s))

let transl_label_init expr =
  if !used_methods = [] then
    expr
  else
    let init = Ident.create "new_method" in
    let expr' =
      Llet(StrictOpt, init, oo_prim "new_method",
      List.fold_right
        (fun (lab, id) expr ->
           Llet(StrictOpt, id, Lapply(Lvar init, [string lab]), expr))
        !used_methods
        expr)
    in
    reset_labels ();
    expr'

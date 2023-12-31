(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(* Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt*)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ctype.ml,v 1.115 2001/11/23 14:28:21 garrigue Exp $ *)

(* Operations on core types *)

open Misc
open Asttypes
open Types
open Btype

(*
   Type manipulation after type inference
   ======================================
   If one wants to manipulate a type after type inference (for
   instance, during code generation or in the debugger), one must
   first make sure that the type levels are correct, using the
   function [correct_levels]. Then, this type can be correctely
   manipulated by [apply], [expand_head] and [moregeneral].
*)

(*
   General notes
   =============
   - As much sharing as possible should be kept : it makes types
     smaller and better abbreviated.
     When necessary, some sharing can be lost. Types will still be
     printed correctly (+++ TO DO...), and abbreviations defined by a
     class do not depend on sharing thanks to constrained
     abbreviations. (Of course, even if some sharing is lost, typing
     will still be correct.)
   - All nodes of a type have a level : that way, one know whether a
     node need to be duplicated or not when instantiating a type.
   - Levels of a type are decreasing (generic level being considered
     as greatest).
   - The level of a type constructor is superior to the binding
     time of its path.
   - Recursive types without limitation should be handled (even if
     there is still an occur check). This avoid treating specially the
     case for objects, for instance. Furthermore, the occur check
     policy can then be easily changed.
*)

(*
   A faire
   =======
   - Revoir affichage des types.
   - Etendre la portee d'un alias [... as 'a] a tout le type englobant.
   - #-type implementes comme de vraies abreviations.
   - Niveaux plus fins pour les identificateurs :
       Champ [global] renomme en [level];
       Niveau -1 : global
               0 : module toplevel
               1 : module contenu dans module toplevel
              ...
     En fait, incrementer le niveau a chaque fois que l'on rentre dans
     un module.

       3   4 6
        \ / /
       1 2 5
        \|/
         0

     [Subst] doit ecreter les niveaux (pour qu'un variable non
     generalisable dans un module de niveau 2 ne se retrouve pas
     generalisable lorsque l'on l'utilise au niveau 0).

   - Traitement de la trace de l'unification separe de la fonction
     [unify].
*)

(**** Errors ****)

exception Unify of (type_expr * type_expr) list

exception Tags of label * label

exception Subtype of
        (type_expr * type_expr) list * (type_expr * type_expr) list

exception Cannot_expand

exception Cannot_apply

exception Recursive_abbrev

(**** Type level management ****)

let current_level = ref 0
let nongen_level = ref 0
let global_level = ref 1
let saved_level = ref []
let saved_global_level = ref []

let init_def level = current_level := level; nongen_level := level
let begin_def () =
  saved_level := (!current_level, !nongen_level) :: !saved_level;
  incr current_level; nongen_level := !current_level
let begin_class_def () =
  saved_level := (!current_level, !nongen_level) :: !saved_level;
  incr current_level
let raise_nongen_level () =
  saved_level := (!current_level, !nongen_level) :: !saved_level;
  nongen_level := !current_level
let end_def () =
  let (cl, nl) = List.hd !saved_level in
  saved_level := List.tl !saved_level;
  current_level := cl; nongen_level := nl

let reset_global_level () =
  global_level := !current_level + 1;
  saved_global_level := []
let increase_global_level () =
  saved_global_level := !global_level :: !saved_global_level;
  global_level := !current_level
let restore_global_level () =
  match !saved_global_level with
    gl::rem -> global_level := gl; saved_global_level := rem
  | []      -> assert false

(**** Some type creators ****)

(* Re-export generic type creators *)

let newty2             = Btype.newty2
let newty desc         = newty2 !current_level desc
let new_global_ty desc = newty2 !global_level desc

let newvar ()          = newty2 !current_level Tvar
let newvar2 level      = newty2 level Tvar
let new_global_var ()  = newty2 !global_level Tvar

let newobj fields      = newty (Tobject (fields, ref None))

let newconstr path tyl = newty (Tconstr (path, tyl, ref Mnil))

let none = newty (Ttuple [])                (* Clearly ill-formed type *)

(**** Representative of a type ****)

(* Re-export repr *)
let repr = repr

(**** Type maps ****)

module TypePairs =
  Hashtbl.Make (struct
    type t = type_expr * type_expr
    let equal (t1, t1') (t2, t2') = (t1 == t2) && (t1' == t2')
    let hash (t, t') = t.id + 93 * t'.id
 end)

                  (**********************************************)
                  (*  Miscellaneous operations on object types  *)
                  (**********************************************)


(**** Object field manipulation. ****)

let object_fields ty =
  match (repr ty).desc with
    Tobject (fields, _) -> fields
  | _                   -> assert false

let flatten_fields ty =
  let rec flatten l ty =
    let ty = repr ty in
    match ty.desc with
      Tfield(s, k, ty1, ty2) ->
        flatten ((s, k, ty1)::l) ty2
    | _ ->
        (l, ty)
  in
    let (l, r) = flatten [] ty in
    (Sort.list (fun (n, _, _) (n', _, _) -> n < n') l, r)

let build_fields level =
  List.fold_right
    (fun (s, k, ty1) ty2 -> newty2 level (Tfield(s, k, ty1, ty2)))

let associate_fields fields1 fields2 =
  let rec associate p s s' =
    function
      (l, []) ->
        (List.rev p, (List.rev s) @ l, List.rev s')
    | ([], l') ->
        (List.rev p, List.rev s, (List.rev s') @ l')
    | ((n, k, t)::r, (n', k', t')::r') when n = n' ->
        associate ((n, k, t, k', t')::p) s s' (r, r')
    | ((n, k, t)::r, ((n', k', t')::_ as l')) when n < n' ->
        associate p ((n, k, t)::s) s' (r, l')
    | (((n, k, t)::r as l), (n', k', t')::r') (* when n > n' *) ->
        associate p s ((n', k', t')::s') (l, r')
  in
  associate [] [] [] (fields1, fields2)

(**** Check whether an object is open ****)

(* +++ Il faudra penser a eventuellement expanser l'abreviation *)
let rec opened_object ty =
  match (repr ty).desc with
    Tobject (t, _)     -> opened_object t
  | Tfield(_, _, _, t) -> opened_object t
  | Tvar               -> true
  | _                  -> false

(**** Close an object ****)

let close_object ty =
  let rec close ty =
    let ty = repr ty in
    match ty.desc with
      Tvar                 -> ty.desc <- Tlink (newty2 ty.level Tnil)
    | Tfield(_, _, _, ty') -> close ty'
    | _                    -> assert false
  in
  match (repr ty).desc with
    Tobject (ty, _)   -> close ty
  | _                 -> assert false

(**** Row variable of an object type ****)

let row_variable ty =
  let rec find ty =
    let ty = repr ty in
    match ty.desc with
      Tfield (_, _, _, ty) -> find ty
    | Tvar                 -> ty
    | _                    -> assert false
  in
  match (repr ty).desc with
    Tobject (fi, _) -> find fi
  | _               -> assert false

(**** Object name manipulation ****)
(* +++ Bientot obsolete *)

let set_object_name id rv params ty =
  match (repr ty).desc with
    Tobject (fi, nm) ->
      begin try
        nm := Some (Path.Pident id, rv::params)
      with Not_found ->
        ()
      end
  | _ ->
      assert false

let remove_object_name ty =
  match (repr ty).desc with
    Tobject (_, nm)   -> nm := None
  | Tconstr (_, _, _) -> ()
  | _                 -> fatal_error "Ctype.remove_object_name"

(**** Hiding of private methods ****)

let hide_private_methods ty =
  let (fl, _) = flatten_fields (object_fields ty) in
  List.iter
    (function (_, k, _) ->
       let k = field_kind_repr k in
       match k with
         Fvar r -> r := Some Fabsent
       | _      -> ())
    fl


                              (*******************************)
                              (*  Operations on class types  *)
                              (*******************************)


let rec signature_of_class_type =
  function
    Tcty_constr (_, _, cty) -> signature_of_class_type cty
  | Tcty_signature sign     -> sign
  | Tcty_fun (_, ty, cty)   -> signature_of_class_type cty

let self_type cty =
  repr (signature_of_class_type cty).cty_self

let rec class_type_arity =
  function
    Tcty_constr (_, _, cty) ->  class_type_arity cty
  | Tcty_signature _        ->  0
  | Tcty_fun (_, _, cty)    ->  1 + class_type_arity cty


                  (*******************************************)
                  (*  Miscellaneous operations on row types  *)
                  (*******************************************)

let sort_row_fields = Sort.list (fun (p,_) (q,_) -> p < q)

let merge_row_fields fi1 fi2 =
  let rec merge r1 r2 pairs fi1 fi2 =
    match fi1, fi2 with
      (l1,f1 as p1)::fi1', (l2,f2 as p2)::fi2' ->
        if l1 = l2 then merge r1 r2 ((l1,f1,f2)::pairs) fi1' fi2' else
        if l1 < l2 then merge (p1::r1) r2 pairs fi1' fi2 else
        merge r1 (p2::r2) pairs fi1 fi2'
    | [], _ -> (List.rev r1, List.rev_append r2 fi2, pairs)
    | _, [] -> (List.rev_append r1 fi1, List.rev r2, pairs)
  in
  merge [] [] [] (sort_row_fields fi1) (sort_row_fields fi2)

let rec filter_row_fields erase = function
    [] -> []
  | (l,f as p)::fi ->
      let fi = filter_row_fields erase fi in
      match row_field_repr f with
        Rabsent -> fi
      | Reither(_,_,false,e) when erase -> e := Some Rabsent; fi
      | _ -> p :: fi

                    (**************************************)
                    (*  Check genericity of type schemes  *)
                    (**************************************)


exception Non_closed

let rec closed_schema_rec ty =
  let ty = repr ty in
  if ty.level >= lowest_level then begin
    let level = ty.level in
    ty.level <- pivot_level - level;
    match ty.desc with
      Tvar when level <> generic_level ->
        raise Non_closed
    | Tfield(_, kind, t1, t2) ->
        if field_kind_repr kind = Fpresent then
          closed_schema_rec t1;
        closed_schema_rec t2
    | Tvariant row when static_row row ->
        iter_row closed_schema_rec row
    | _ ->
        iter_type_expr closed_schema_rec ty
  end

(* Return whether all variables of type [ty] are generic. *)
let closed_schema ty =
  try
    closed_schema_rec ty;
    unmark_type ty;
    true
  with Non_closed ->
    unmark_type ty;
    false

exception Non_closed of type_expr * bool

let free_variables = ref []

let rec free_vars_rec real ty =
  let ty = repr ty in
  if ty.level >= lowest_level then begin
    ty.level <- pivot_level - ty.level;
    begin match ty.desc with
      Tvar ->
        free_variables := (ty, real) :: !free_variables
(* Do not count "virtual" free variables
    | Tobject(ty, {contents = Some (_, p)}) ->
        free_vars_rec false ty; List.iter (free_vars_rec true) p
*)
    | Tobject (ty, _) ->
        free_vars_rec false ty
    | Tfield (_, _, ty1, ty2) ->
        free_vars_rec true ty1; free_vars_rec false ty2
    | Tvariant row ->
        let row = row_repr row in
        iter_row (free_vars_rec true) {row with row_bound = []};
        if not (static_row row) then
          free_variables := (row_more row, false) :: !free_variables
    | _    ->
        iter_type_expr (free_vars_rec true) ty
    end;
  end

let free_vars ty =
  free_variables := [];
  free_vars_rec true ty;
  let res = !free_variables in
  free_variables := [];
  res

let rec closed_type ty =
  match free_vars ty with
      []           -> ()
  | (v, real) :: _ -> raise (Non_closed (v, real))

let closed_parameterized_type params ty =
  List.iter mark_type params;
  try
    closed_type ty;
    List.iter unmark_type params;
    unmark_type ty;
    true
  with Non_closed _ ->
    List.iter unmark_type params;
    unmark_type ty;
    false

let closed_type_decl decl =
  try
    List.iter mark_type decl.type_params;
    begin match decl.type_kind with
      Type_abstract ->
        ()
    | Type_variant v ->
        List.iter (fun (_, tyl) -> List.iter closed_type tyl) v
    | Type_record(r, rep) ->
        List.iter (fun (_, _, ty) -> closed_type ty) r
    end;
    begin match decl.type_manifest with
      None    -> ()
    | Some ty -> closed_type ty
    end;
    unmark_type_decl decl;
    None
  with Non_closed (ty, _) ->
    unmark_type_decl decl;
    Some ty

type closed_class_failure =
    CC_Method of type_expr * bool * string * type_expr
  | CC_Value of type_expr * bool * string * type_expr

exception Failure of closed_class_failure

let closed_class params sign =
  let ty = object_fields (repr sign.cty_self) in
  let (fields, rest) = flatten_fields ty in
  List.iter mark_type params;
  mark_type rest;
  List.iter
    (fun (lab, _, ty) -> if lab = "*dummy method*" then mark_type ty)
    fields;
  try
    mark_type_node (repr sign.cty_self);
    List.iter
      (fun (lab, kind, ty) ->
        if field_kind_repr kind = Fpresent then
        try closed_type ty with Non_closed (ty0, real) ->
          raise (Failure (CC_Method (ty0, real, lab, ty))))
      fields;
    mark_type_params (repr sign.cty_self);
    List.iter unmark_type params;
    unmark_class_signature sign;
    None
  with Failure reason ->
    mark_type_params (repr sign.cty_self);
    List.iter unmark_type params;
    unmark_class_signature sign;
    Some reason


                            (**********************)
                            (*  Type duplication  *)
                            (**********************)


(* Duplicate a type, preserving only type variables *)
let duplicate_type ty =
  Subst.type_expr Subst.identity ty

(* Same, for class types *)
let duplicate_class_type ty =
  Subst.class_type Subst.identity ty


                         (*****************************)
                         (*  Type level manipulation  *)
                         (*****************************)

(*
   It would be a bit more efficient to remove abbreviation expansions
   rather than generalizing them: these expansions will usually not be
   used anymore. However, this is not possible in the general case, as
   [expand_abbrev] (via [subst]) requires these expansions to be
   preserved. Does it worth duplicating this code ?
*)
let rec iter_generalize tyl ty =
  let ty = repr ty in
  if (ty.level > !current_level) && (ty.level <> generic_level) then begin
    ty.level <- generic_level;
    begin match ty.desc with
      Tconstr (_, _, abbrev) ->
        generalize_expans tyl !abbrev
    | _ -> ()
    end;
    iter_type_expr (iter_generalize tyl) ty
  end else
    tyl := ty :: !tyl

and generalize_expans tyl =
  function
    Mnil                   -> ()
  | Mcons(_, ty, ty', rem) -> iter_generalize tyl ty;
                              iter_generalize tyl ty';
                              generalize_expans tyl rem
  | Mlink rem              -> generalize_expans tyl !rem

let rec generalize ty =
  iter_generalize (ref []) ty

(* Efficient repeated generalisation of the same type *)
let iterative_generalization min_level tyl =
  let tyl' = ref [] in
  List.iter (iter_generalize tyl') tyl;
  List.fold_right (fun ty l -> if ty.level <= min_level then l else ty::l)
    !tyl' []

let try_expand_head' = (* Forward declaration *)
  ref (fun env ty -> raise Cannot_expand)

(*
   Lower the levels of a type (assume [level] is not
   [generic_level]).
*)
(*
    The level of a type constructor must be greater than its binding
    time. That way, a type constructor cannot escape the scope of its
    definition, as would be the case in
      let x = ref []
      module M = struct type t let _ = (x : t list ref) end
    (without this constraint, the type system would actually be unsound.)
*)
let rec update_level env level ty =
  let ty = repr ty in
  if ty.level > level then begin
    begin match ty.desc with
      Tconstr(p, tl, abbrev)  when level < Path.binding_time p ->
        (* Try first to replace an abbreviation by its expansion. *)
        begin try
          ty.desc <- Tlink (!try_expand_head' env ty);
          update_level env level ty
        with Cannot_expand ->
          (* +++ Levels should be restored... *)
          raise (Unify [(ty, newvar2 level)])
        end
    | Tfield(_, k, _, _) ->
        begin match field_kind_repr k with
          Fvar _ (* {contents = None} *) -> raise (Unify [(ty, newvar2 level)])
        | _                              -> ()
        end;
        ty.level <- level;
        iter_type_expr (update_level env level) ty
    | _ ->
        ty.level <- level;
        iter_type_expr (update_level env level) ty
    end
  end

(* 
   Function [update_level] will never try to expand an abbreviation in
   this case ([current_level] is greater than the binding time of any
   type constructor path). So, it can be called with the empty
   environnement.
*)
let make_nongen ty =
  try
    update_level Env.empty !nongen_level ty
  with Unify [_, ty'] ->
    raise (Unify [ty, ty'])

(* Correct the levels of type [ty]. *)
let correct_levels ty =
  duplicate_type ty

(* Only generalize the type ty0 in ty *)
let limited_generalize ty0 ty =
  let ty0 = repr ty0 in

  let graph = Hashtbl.create 17 in
  let idx = ref lowest_level in
  let roots = ref [] in

  let rec inverse pty ty =
    let ty = repr ty in
    if (ty.level > !current_level) || (ty.level = generic_level) then begin
      decr idx;
      Hashtbl.add graph !idx (ty, ref pty);
      if (ty.level = generic_level) || (ty == ty0) then
        roots := ty :: !roots;
      ty.level <- !idx;
      iter_type_expr (inverse [ty]) ty
    end else if ty.level < lowest_level then begin
      let (_, parents) = Hashtbl.find graph ty.level in
      parents := pty @ !parents
    end

  and generalize_parents ty =
    let idx = ty.level in
    if idx <> generic_level then begin
      ty.level <- generic_level;
      List.iter generalize_parents !(snd (Hashtbl.find graph idx))
    end
  in

  inverse [] ty;
  if ty0.level < lowest_level then
    iter_type_expr (inverse []) ty0;
  List.iter generalize_parents !roots;
  Hashtbl.iter
    (fun _ (ty, _) ->
       if ty.level <> generic_level then
         ty.level <- !current_level)
    graph


                              (*******************)
                              (*  Instantiation  *)
                              (*******************)


let rec find_repr p1 =
  function
    Mnil ->
      None
  | Mcons (p2, ty, _, _) when Path.same p1 p2 ->
      Some ty
  | Mcons (_, _, _, rem) ->
      find_repr p1 rem
  | Mlink {contents = rem} ->
      find_repr p1 rem

(*
   Generic nodes are duplicated, while non-generic nodes are left
   as-is.
   During instantiation, the description of a generic node is first
   replaced by a link to a stub ([Tsubst (newvar ())]). Once the
   copy is made, it replaces the stub.
   After instantiation, the description of generic node, which was
   stored by [save_desc], must be put back, using [cleanup_types].
*)

let abbreviations = ref (ref Mnil)
  (* Abbreviation memorized. *)

let rec copy ty =
  let ty = repr ty in
  match ty.desc with
    Tsubst ty -> ty
  | _ ->
    if ty.level <> generic_level then ty else
    let desc = ty.desc in
    save_desc ty desc;
    let t = newvar() in          (* Stub *)
    ty.desc <- Tsubst t;
    t.desc <-
      begin match desc with
      | Tconstr (p, tl, _) ->
          begin match find_repr p !(!abbreviations) with
            Some ty when repr ty != t -> (* XXX Commentaire... *)
              Tlink ty
          | _ ->
          (*
             One must allocate a new reference, so that abbrevia-
             tions belonging to different branches of a type are
             independent.
             Moreover, a reference containing a [Mcons] must be
             shared, so that the memorized expansion of an abbrevi-
             ation can be released by changing the content of just
             one reference.
          *)
              Tconstr (p, List.map copy tl,
                       ref (match !(!abbreviations) with
                              Mcons _ -> Mlink !abbreviations
                            | abbrev  -> abbrev))
          end
      | Tvariant row0 ->
          let row = row_repr row0 in
          let more = repr row.row_more in
          (* We must substitute in a subtle way *)
          begin match more.desc with
            Tsubst ty2 ->
              (* This variant type has been already copied *)
              ty.desc <- Tsubst ty2; (* avoid Tlink in the new type *)
              Tlink ty2
          | _ ->
              (* If the row variable is not generic, we must keep it *)
              let keep = more.level <> generic_level in
              (* Register new type first for recursion *)
              save_desc more more.desc;
              more.desc <- ty.desc;
              (* Return a new copy *)
              let more' = if keep then more else newvar () in
              Tvariant (copy_row copy row keep more')
          end
      | _ -> copy_type_desc copy desc
      end;
    t

(**** Variants of instantiations ****)

let instance sch =
  let ty = copy sch in
  cleanup_types ();
  ty

let instance_list schl =
  let tyl = List.map copy schl in
  cleanup_types ();
  tyl

let instance_constructor cstr =
  let ty_res = copy cstr.cstr_res in
  let ty_args = List.map copy cstr.cstr_args in
  cleanup_types ();
  (ty_args, ty_res)

let instance_label lbl =
  let ty_res = copy lbl.lbl_res in
  let ty_arg = copy lbl.lbl_arg in
  cleanup_types ();
  (ty_arg, ty_res)

let instance_parameterized_type sch_args sch =
  let ty_args = List.map copy sch_args in
  let ty = copy sch in
  cleanup_types ();
  (ty_args, ty)

let instance_parameterized_type_2 sch_args sch_lst sch =
  let ty_args = List.map copy sch_args in
  let ty_lst = List.map copy sch_lst in
  let ty = copy sch in
  cleanup_types ();
  (ty_args, ty_lst, ty)

let instance_class params cty =
  let rec copy_class_type =
    function
      Tcty_constr (path, tyl, cty) ->
        Tcty_constr (path, List.map copy tyl, copy_class_type cty)
    | Tcty_signature sign ->
        Tcty_signature
          {cty_self = copy sign.cty_self;
           cty_vars =
             Vars.map (function (mut, ty) -> (mut, copy ty)) sign.cty_vars;
           cty_concr = sign.cty_concr}
    | Tcty_fun (l, ty, cty) ->
        Tcty_fun (l, copy ty, copy_class_type cty)
  in
  let params' = List.map copy params in
  let cty' = copy_class_type cty in
  cleanup_types ();
  (params', cty')

(**** Instantiation with parameter substitution ****)

let unify' = (* Forward declaration *)
  ref (fun env ty1 ty2 -> raise (Unify []))

let rec subst env level abbrev ty params args body =
  let old_level = !current_level in
  current_level := level;
  try
    let body0 = newvar () in          (* Stub *)
    begin match ty with
      None      -> ()
    | Some ({desc = Tconstr (path, _, _)} as ty) ->
        memorize_abbrev abbrev path ty body0
    | _ ->
        assert false
    end;
    abbreviations := abbrev;
    let (params', body') = instance_parameterized_type params body in
    abbreviations := ref Mnil;
    !unify' env body0 body';
    List.iter2 (!unify' env) params' args;
    current_level := old_level;
    body'
  with Unify _ as exn ->
    current_level := old_level;
    raise exn

(*
   Only the shape of the type matters, not whether is is generic or
   not. [generic_level] might be somewhat slower, but it ensures
   invariants on types are enforced (decreasing levels.), and we don't
   care about efficiency here.
*)
let apply env params body args =
  try
    subst env generic_level (ref Mnil) None params args body
  with
    Unify _ -> raise Cannot_apply


                              (****************************)
                              (*  Abbreviation expansion  *)
                              (****************************)


(* Search whether the expansion has been memorized. *)
let rec find_expans p1 =
  function
    Mnil ->
      None
  | Mcons (p2, ty0, ty, _) when Path.same p1 p2 ->
      (* assert
        begin match (repr ty).desc with
          Tconstr (p3, _, _) when Path.same p2 p3-> false
        | _ -> true
        end;
      assert (repr ty0 != repr ty); *)
      Some ty
  | Mcons (_, _, _, rem) ->
      find_expans p1 rem
  | Mlink {contents = rem} ->
      find_expans p1 rem

let previous_env = ref Env.empty

(* Expand an abbreviation. The expansion is memorized. *)
(* 
   Assume the level is greater than the path binding time of the
   expanded abbreviation.
*)
(*
   An abbreviation expansion will fail in either of these cases:
   1. The type constructor does not correspond to a manifest type.
   2. The type constructor is defined in an external file, and this
      file is not in the path (missing -I options).
   3. The type constructor is not in the "local" environment. This can
      happens when a non-generic type variable has been instantiated
      afterwards to the not yet defined type constructor. (Actually,
      this cannot happen at the moment due to the strong constraints
      between type levels and constructor binding time.)
   4. The expansion requires the expansion of another abbreviation,
      and this other expansion fails.
*)
let expand_abbrev env ty =
  (* 
     If the environnement has changed, memorized expansions might not
     be correct anymore, and so we flush the cache. This is safe but
     quite pessimistic: it would be enough to flush the cache when a
     type or module definition is overriden in the environnement.
  *)
  if env != !previous_env then begin
    cleanup_abbrev ();
    previous_env := env
  end;
  match ty with
    {desc = Tconstr (path, args, abbrev); level = level} ->
      begin match find_expans path !abbrev with
        Some ty ->
          if level <> generic_level then
            begin try
              update_level env level ty
            with Unify _ ->
              (* XXX This should not happen.
                 However, levels are not correctly restored after a
                 typing error *)
              ()
            end;
          ty
      | None ->
          let (params, body) =
            try Env.find_type_expansion path env with Not_found ->
              raise Cannot_expand
          in
          let ty' = subst env level abbrev (Some ty) params args body in
          (* Hack to name the variant type *)
          begin match repr ty' with
            {desc=Tvariant row} as ty when static_row row ->
              ty.desc <- Tvariant { row with row_name = Some (path, args) }
          | _ -> ()
          end;
          ty'
      end
  | _ ->
      assert false

(* Fully expand the head of a type. Raise an exception if the type
   cannot be expanded. *)
let rec try_expand_head env ty =
  let ty = repr ty in
  match ty.desc with
    Tconstr _ ->
      let ty' = expand_abbrev env ty in
      begin try
        try_expand_head env ty'
      with Cannot_expand ->
        repr ty'
      end
  | _ ->
      raise Cannot_expand

let _ = try_expand_head' := try_expand_head

(* Fully expand the head of a type. *)
let rec expand_head env ty =
  try try_expand_head env ty with Cannot_expand -> repr ty

(* Make sure that the type parameters of the type constructor [ty]
   respect the type constraints *)
let enforce_constraints env ty =
  match ty with
    {desc = Tconstr (path, args, abbrev); level = level} ->
      let decl = Env.find_type path env in
      ignore
        (subst env level (ref Mnil) None decl.type_params args (newvar2 level))
  | _ ->
      assert false

(* Recursively expand the head of a type.
   Also expand #-types. *)
let rec full_expand env ty =
  let ty = repr (expand_head env ty) in
  match ty.desc with
    Tobject (fi, {contents = Some (_, v::_)}) when (repr v).desc = Tvar ->
      newty2 ty.level (Tobject (fi, ref None))
  | _ ->
      ty

(*
   Check whether the abbreviation expands to a well-defined type.
   During the typing of a class, abbreviations for correspondings
   types expand to non-generic types.
*)
let generic_abbrev env path =
  try
    let (_, body) = Env.find_type_expansion path env in
    (repr body).level = generic_level
  with
    Not_found ->
      false


                              (*****************)
                              (*  Occur check  *)
                              (*****************)


exception Occur

(* The marks are already used by [expand_abbrev]... *)
let visited = ref []

let rec non_recursive_abbrev env ty0 ty =
  let ty = repr ty in
  if ty == repr ty0 then raise Recursive_abbrev;
  if not (List.memq ty !visited) then begin
    let level = ty.level in
    visited := ty :: !visited;
    match ty.desc with
      Tconstr(p, args, abbrev) ->
        begin try
          non_recursive_abbrev env ty0 (try_expand_head env ty)
        with Cannot_expand ->
          if !Clflags.recursive_types then () else
          iter_type_expr (non_recursive_abbrev env ty0) ty
        end
    | Tobject _ | Tvariant _ ->
        ()
    | _ ->
        if !Clflags.recursive_types then () else
        iter_type_expr (non_recursive_abbrev env ty0) ty
  end

let correct_abbrev env ident params ty =
  let ty0 = newgenvar () in
  visited := [];
  non_recursive_abbrev env ty0
    (subst env generic_level
       (ref (Mcons (Path.Pident ident, ty0, ty0, Mnil))) None
       [] [] ty);
  visited := []

let rec occur_rec env visited ty0 ty =
  if ty == ty0  then raise Occur;
  match ty.desc with
    Tconstr(p, tl, abbrev) ->
      begin try
        if List.memq ty visited then raise Occur;
        iter_type_expr (occur_rec env (ty::visited) ty0) ty
      with Occur -> try
        let ty' = try_expand_head env ty in
        (* Maybe we could simply make a recursive call here,
           but it seems it could make the occur check loop
           (see change in rev. 1.58) *)
        if ty' == ty0 || List.memq ty' visited then raise Occur;
        match ty'.desc with
          Tobject _ | Tvariant _ -> ()
        | _         -> iter_type_expr (occur_rec env (ty'::visited) ty0) ty'
      with Cannot_expand ->
        raise Occur
      end
  | Tobject _ | Tvariant _ ->
      ()
  | _ ->
      iter_type_expr (occur_rec env visited ty0) ty

let occur env ty0 ty =
  if not !Clflags.recursive_types then 
    try occur_rec env [] ty0 ty with Occur -> raise (Unify [])


                              (*****************)
                              (*  Unification  *)
                              (*****************)



let rec has_cached_expansion p abbrev =
  match abbrev with
    Mnil                   -> false
  | Mcons(p', _, _, rem)   -> Path.same p p' || has_cached_expansion p rem
  | Mlink rem              -> has_cached_expansion p !rem

(**** Transform error trace ****)
(* +++ Move it to some other place ? *)

let expand_trace env trace =
  List.fold_right
    (fun (t1, t2) rem ->
       (repr t1, full_expand env t1)::(repr t2, full_expand env t2)::rem)
    trace []

(**** Unification ****)

(* Return whether [t0] occurs in [ty]. Objects are also traversed. *)
let deep_occur t0 ty =
  let rec occur_rec ty =
    let ty = repr ty in
    if ty.level >= lowest_level then begin
      if ty == t0 then raise Occur;
      ty.level <- pivot_level - ty.level;
      iter_type_expr occur_rec ty
    end
  in
  try
    occur_rec ty; unmark_type ty; false
  with Occur ->
    unmark_type ty; true

(*
   1. When unifying two non-abbreviated types, one type is made a link
      to the other. When unifying an abbreviated type with a
      non-abbreviated type, the non-abbreviated type is made a link to
      the other one. When unifying to abbreviated types, these two
      types are kept distincts, but they are made to (temporally)
      expand to the same type.
   2. Abbreviations with at least one parameter are systematically
      expanded. The overhead does not seem to high, and that way
      abbreviations where some parameters does not appear in the
      expansion, such as ['a t = int], are correctly handled. In
      particular, for this example, unifying ['a t] with ['b t] keeps
      ['a] and ['b] distincts. (Is it really important ?)
   3. Unifying an abbreviation ['a t = 'a] with ['a] should not yield
      ['a t as 'a]. Indeed, the type variable would otherwise be lost.
      This problem occurs for abbreviations expanding to a type
      variable, but also to many other constrained abbreviations (for
      instance, [(< x : 'a > -> unit) t = <x : 'a>]). The solution is
      that, if an abbreviation is unified with some subpart of its
      parameters, then the parameter actually does not get
      abbreviated.  It would be possible to check whether some
      information is indeed lost, but it probably does not worth it.
*)
let rec unify env t1 t2 =
  (* First step: special cases (optimizations) *)
  if t1 == t2 then () else
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then () else

  try
    match (t1.desc, t2.desc) with
      (Tvar, Tconstr _) when deep_occur t1 t2 ->
        unify2 env t1 t2
    | (Tconstr _, Tvar) when deep_occur t2 t1 ->
        unify2 env t1 t2
    | (Tvar, _) ->
        occur env t1 t2;
        update_level env t1.level t2;
        t1.desc <- Tlink t2
    | (_, Tvar) ->
        occur env t2 t1;
        update_level env t2.level t1;
        t2.desc <- Tlink t1
    | (Tconstr (p1, [], a1), Tconstr (p2, [], a2))
          when Path.same p1 p2
            (* This optimization assumes that t1 does not expand to t2
               (and conversely), so we fall back to the general case
               when any of the types has a cached expansion. *)
            && not (has_cached_expansion p1 !a1
                 || has_cached_expansion p2 !a2) ->
        update_level env t1.level t2;
        t1.desc <- Tlink t2
    | _ ->
        unify2 env t1 t2
  with Unify trace ->
    raise (Unify ((t1, t2)::trace))

and unify2 env t1 t2 =
  (* Second step: expansion of abbreviations *)
  let t1' = expand_head env t1 in
  let t2' = expand_head env t2 in
  (* Expansion may have changed the representative of the types... *)
  let t1' = expand_head env t1' in
  let t2' = expand_head env t2' in
  if t1' == t2' then () else

  let t1 = repr t1 and t2 = repr t2 in
  if (t1 == t1') || (t2 != t2') then
    unify3 env t1 t1' t2 t2'
  else
    try unify3 env t2 t2' t1 t1' with Unify trace ->
      raise (Unify (List.map (fun (x, y) -> (y, x)) trace))

and unify3 env t1 t1' t2 t2' =
  (* Third step: truly unification *)
  (* Assumes either [t1 == t1'] or [t2 != t2'] *)
  let d1 = t1'.desc and d2 = t2'.desc in
  
  let create_recursion = (t2 != t2') && (deep_occur t1' t2) in
  occur env t1' t2;
  update_level env t1'.level t2;
  t1'.desc <- Tlink t2;

  try
    begin match (d1, d2) with
      (Tvar, _) ->
        ()
    | (_, Tvar) ->
        occur env t2' (newty d1);
        if t1 == t1' then begin
          (* The variable must be instantiated... *)
          let ty = newty2 t1'.level d1 in
          update_level env t2'.level ty;
          t2'.desc <- Tlink ty
        end else begin
          t1'.desc <- d1;
          update_level env t2'.level t1;
          t2'.desc <- Tlink t1
        end
    | (Tarrow (l1, t1, u1, c1), Tarrow (l2, t2, u2, c2)) when l1 = l2
      || !Clflags.classic && not (is_optional l1 || is_optional l2) ->
        unify env t1 t2; unify env u1 u2;
        begin match commu_repr c1, commu_repr c2 with
          Clink r, c2 -> r := c2
        | c1, Clink r -> r := c1
        | _ -> ()
        end
    | (Ttuple tl1, Ttuple tl2) ->
        unify_list env tl1 tl2
    | (Tconstr (p1, tl1, _), Tconstr (p2, tl2, _)) when Path.same p1 p2 ->
        unify_list env tl1 tl2
    | (Tobject (fi1, nm1), Tobject (fi2, _)) ->
        unify_fields env fi1 fi2;
        (* Type [t2'] may have been instantiated by [unify_fields] *)
        (* XXX One should do some kind of unification... *)
        begin match (repr t2').desc with
          Tobject (_, {contents = Some (_, va::_)})
                when (repr va).desc = Tvar ->
            ()  
        | Tobject (_, nm2) ->
            nm2 := !nm1
        | _ ->
            ()
        end
    | (Tvariant row1, Tvariant row2) ->
        unify_row env row1 row2
    | (Tfield _, Tfield _) ->           (* Actually unused *)
        unify_fields env t1' t2'
    | (Tnil, Tnil) ->
        ()
    | (_, _) ->
        raise (Unify [])
    end;

(* XXX Commentaires + changer "create_recursion" *)
    if create_recursion then begin
      match t2.desc with
        Tconstr (p, tl, abbrev) ->
          forget_abbrev abbrev p;
          let t2'' = expand_head env t2 in
          if not (closed_parameterized_type tl t2'') then
            (repr t2).desc <- Tlink (repr t2')
      | _ ->
          assert false
    end

(*
    (* 
       Can only be done afterwards, once the row variable has
       (possibly) been instantiated.
    *)
    if t1 != t1' (* && t2 != t2' *) then begin
      match (t1.desc, t2.desc) with
        (Tconstr (p, ty::_, _), _)
            when ((repr ty).desc <> Tvar)
              && weak_abbrev p
              && not (deep_occur t1 t2) ->
          update_level env t1.level t2;
          t1.desc <- Tlink t2
      | (_, Tconstr (p, ty::_, _))
            when ((repr ty).desc <> Tvar)
              && weak_abbrev p
              && not (deep_occur t2 t1) ->
          update_level env t2.level t1;
          t2.desc <- Tlink t1;
          t1'.desc <- Tlink t2'
      | _ ->
          ()
    end
*)
  with Unify trace ->
    t1'.desc <- d1;
    raise (Unify trace)

and unify_list env tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    raise (Unify []);
  List.iter2 (unify env) tl1 tl2

and unify_fields env ty1 ty2 =          (* Optimization *)
  let (fields1, rest1) = flatten_fields ty1
  and (fields2, rest2) = flatten_fields ty2 in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  let va = newvar () in
  unify env (build_fields (repr ty1).level miss1 va) rest2;
  unify env rest1 (build_fields (repr ty2).level miss2 va);
  List.iter
    (fun (n, k1, t1, k2, t2) ->
       unify_kind k1 k2;
       try unify env t1 t2 with Unify trace ->
         raise (Unify ((newty (Tfield(n, k1, t1, va)),
                        newty (Tfield(n, k2, t2, va)))::trace)))
    pairs

and unify_kind k1 k2 =
  let k1 = field_kind_repr k1 in
  let k2 = field_kind_repr k2 in
  match k1, k2 with
    (Fvar r, (Fvar _ | Fpresent))             -> r := Some k2
  | (Fpresent, Fvar r)                        -> r := Some k1
  | (Fpresent, Fpresent)                      -> ()
  | _                                         -> assert false

and unify_row env row1 row2 =
  let row1 = row_repr row1 and row2 = row_repr row2 in
  let rm1 = row_more row1 and rm2 = row_more row2 in
  if rm1 == rm2 then () else
  let r1, r2, pairs = merge_row_fields row1.row_fields row2.row_fields in
  ignore (List.fold_left
            (fun hl l ->
              let h = hash_variant l in
              try raise(Tags(l,List.assoc h hl))
              with Not_found -> (h,l)::hl)
            (List.map (fun (l,_) -> (hash_variant l, l)) row1.row_fields)
            (List.map fst r2));
  let more = newty2 (min rm1.level rm2.level) Tvar
  and closed = row1.row_closed || row2.row_closed in
  let keep switch =
    List.for_all
      (fun (_,f1,f2) ->
        let f1, f2 = switch f1 f2 in
        row_field_repr f1 = Rabsent || row_field_repr f2 <> Rabsent)
      pairs
  in
  let empty fields =
    List.for_all (fun (_,f) -> row_field_repr f = Rabsent) fields in
  if (row1.row_closed || row2.row_closed)
  && (empty r1 || row2.row_closed) && (empty r2 || row1.row_closed)
  && List.for_all
      (fun (_,f1,f2) ->
        row_field_repr f1 = Rabsent || row_field_repr f2 = Rabsent)
      pairs
  then raise (Unify []);
  let name =
    if row1.row_name <> None && (row1.row_closed || empty r2) &&
      (not row2.row_closed || keep (fun f1 f2 -> f1, f2) && empty r1)
    then row1.row_name
    else if row2.row_name <> None && (row2.row_closed || empty r1) &&
      (not row1.row_closed || keep (fun f1 f2 -> f2, f1) && empty r2)
    then row2.row_name
    else None
  in
  let bound = row1.row_bound @ row2.row_bound in
  let row0 = {row_fields = []; row_more = more; row_bound = bound;
              row_closed = closed; row_name = name} in
  let more row rest =
    let rest =
      if closed then
        filter_row_fields row.row_closed rest
      else rest in
    if rest <> [] && row.row_closed then raise (Unify []);
    let ty =
      newty2 generic_level (Tvariant {row0 with row_fields = rest}) in
    update_level env (repr row.row_more).level ty;
    ty
  in
  let md1 = rm1.desc and md2 = rm2.desc in
  begin try
    rm1.desc <- Tlink (more row1 r2);
    rm2.desc <- Tlink (more row2 r1);
    List.iter (fun (l,f1,f2) -> unify_row_field env f1 f2) pairs
  with exn ->
    rm1.desc <- md1; rm2.desc <- md2; raise exn
  end

and unify_row_field env f1 f2 =
  let f1 = row_field_repr f1 and f2 = row_field_repr f2 in
  if f1 == f2 then () else
  match f1, f2 with
    Rpresent(Some t1), Rpresent(Some t2) -> unify env t1 t2
  | Rpresent None, Rpresent None -> ()
  | Reither(c1, tl1, m1, e1), Reither(c2, tl2, m2, e2) ->
      if e1 == e2 then () else
      if m1 || m2 then begin
        match tl1 @ tl2 with [] -> ()
        | t1 :: tl ->
            if c1 || c2 then raise (Unify []);
            List.iter (unify env t1) tl
      end;
      let tl1 = List.map repr tl1 and tl2 = List.map repr tl2 in
      let rec remq tl = function [] -> []
        | ty :: tl' ->
            if List.memq ty tl then remq tl tl' else ty :: remq tl tl'
      in
      let tl2' = remq tl2 tl1 and tl1' = remq tl1 tl2 in
      let e = ref None in
      let f1 = Reither(c1 || c2, tl1', m1 || m2, e)
      and f2 = Reither(c1 || c2, tl2', m1 || m2, e) in
      e1 := Some f1; e2 := Some f2
  | Reither(false, tl, _, e1), Rpresent(Some t2) ->
      e1 := Some f2;
      (try List.iter (fun t1 -> unify env t1 t2) tl
      with exn -> e1 := None; raise exn)
  | Rpresent(Some t1), Reither(false, tl, _, e2) ->
      e2 := Some f1;
      (try List.iter (unify env t1) tl
      with exn -> e2 := None; raise exn)
  | Reither(true, [], _, e1), Rpresent None -> e1 := Some f2
  | Rpresent None, Reither(true, [], _, e2) -> e2 := Some f1
  | Reither(_, _, false, e1), Rabsent -> e1 := Some f2
  | Rabsent, Reither(_, _, false, e2) -> e2 := Some f1
  | Rabsent, Rabsent -> ()
  | _ -> raise (Unify [])

let unify env ty1 ty2 =
  try
    unify env ty1 ty2
  with Unify trace ->
    raise (Unify (expand_trace env trace))

let _ = unify' := unify

(**** Special cases of unification ****)

(*
   Unify [t] and [l:'a -> 'b]. Return ['a] and ['b].
   In label mode, label mismatch is accepted when
   (1) the requested label is ""
   (2) the original label is not optional
*)
let rec filter_arrow env t l =
  let t = expand_head env t in
  match t.desc with
    Tvar ->
      let t1 = newvar () and t2 = newvar () in
      let t' = newty (Tarrow (l, t1, t2, Cok)) in
      update_level env t.level t';
      t.desc <- Tlink t';
      (t1, t2)
  | Tarrow(l', t1, t2, _)
    when l = l' || !Clflags.classic && l = "" && not (is_optional l') ->
      (t1, t2)
  | _ ->
      raise (Unify [])

(* Used by [filter_method]. *)
let rec filter_method_field env name priv ty =
  let ty = repr ty in
  match ty.desc with
    Tvar ->
      let level = ty.level in
      let ty1 = newvar2 level and ty2 = newvar2 level in
      let ty' = newty2 level (Tfield (name,
                                      begin match priv with
                                        Private -> Fvar (ref None)
                                      | Public  -> Fpresent
                                      end,
                                      ty1, ty2))
      in
      ty.desc <- Tlink ty';
      ty1
  | Tfield(n, kind, ty1, ty2) ->
      let kind = field_kind_repr kind in
      if (n = name) && (kind <> Fabsent) then begin
        if priv = Public then
          unify_kind kind Fpresent;
        ty1
      end else
        filter_method_field env name priv ty2
  | _ ->
      raise (Unify [])

(* Unify [ty] and [< name : 'a; .. >]. Return ['a]. *)
let rec filter_method env name priv ty =
  let ty = expand_head env ty in
  match ty.desc with
    Tvar ->
      let ty1 = newvar () in
      let ty' = newobj ty1 in
      update_level env ty.level ty';
      ty.desc <- Tlink ty';
      filter_method_field env name priv ty1
  | Tobject(f, _) ->
      filter_method_field env name priv f
  | _ ->
      raise (Unify [])

let check_filter_method env name priv ty =
  ignore(filter_method env name priv ty)

let filter_self_method env lab priv meths ty =
  let ty' = filter_method env lab priv ty in
  try
    Meths.find lab !meths
  with Not_found ->
    let pair = (Ident.create lab, ty') in
    meths := Meths.add lab pair !meths;
    pair


                        (***********************************)
                        (*  Matching between type schemes  *)
                        (***********************************)

(*
   Update the level of [ty]. First check that the levels of generic
   variables from the subject are not lowered.
*)
let moregen_occur env level ty = 
  let rec occur ty =
    let ty = repr ty in
    if ty.level > level then begin
      if ty.desc = Tvar && ty.level >= generic_level - 1 then raise Occur;
      ty.level <- pivot_level - ty.level;
      match ty.desc with
        Tvariant row when static_row row ->
          iter_row occur row
      | _ ->
          iter_type_expr occur ty
    end
  in
  begin try
    occur ty; unmark_type ty
  with Occur ->
    unmark_type ty; raise (Unify [])
  end;
  update_level env level ty

let rec moregen inst_nongen type_pairs env t1 t2 =
  if t1 == t2 then () else
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then () else

  try
    match (t1.desc, t2.desc) with
      (Tvar, _) when if inst_nongen then t1.level <> generic_level - 1
                                    else t1.level =  generic_level ->
        moregen_occur env t1.level t2;
        occur env t1 t2;
        t1.desc <- Tlink t2
    | (Tconstr (p1, [], _), Tconstr (p2, [], _)) when Path.same p1 p2 ->
        ()
    | _ ->
        let t1' = expand_head env t1 in
        let t2' = expand_head env t2 in
        (* Expansion may have changed the representative of the types... *)
        let t1' = repr t1' and t2' = repr t2' in
        if t1' == t2' then () else
        begin try
          TypePairs.find type_pairs (t1', t2')
        with Not_found ->
          TypePairs.add type_pairs (t1', t2') ();
          match (t1'.desc, t2'.desc) with
            (Tvar, _) when if inst_nongen then t1'.level <> generic_level - 1
                                          else t1'.level =  generic_level ->
              moregen_occur env t1'.level t2;
              t1'.desc <- Tlink t2
          | (Tarrow (l1, t1, u1, _), Tarrow (l2, t2, u2, _)) when l1 = l2
            || !Clflags.classic && not (is_optional l1 || is_optional l2) ->
              moregen inst_nongen type_pairs env t1 t2;
              moregen inst_nongen type_pairs env u1 u2
          | (Ttuple tl1, Ttuple tl2) ->
              moregen_list inst_nongen type_pairs env tl1 tl2
          | (Tconstr (p1, tl1, _), Tconstr (p2, tl2, _))
                when Path.same p1 p2 ->
              moregen_list inst_nongen type_pairs env tl1 tl2
          | (Tvariant row1, Tvariant row2) ->
              moregen_row inst_nongen type_pairs env row1 row2
          | (Tobject (fi1, nm1), Tobject (fi2, nm2)) ->
              moregen_fields inst_nongen type_pairs env fi1 fi2
          | (Tfield _, Tfield _) ->           (* Actually unused *)
              moregen_fields inst_nongen type_pairs env t1' t2'
          | (Tfield (_, kind, _, t1''), _)
                when field_kind_repr kind = Fabsent ->
              moregen inst_nongen type_pairs env t1'' t2'
          | (_, Tfield (_, kind, _, t2''))
                when field_kind_repr kind = Fabsent ->
              moregen inst_nongen type_pairs env t1' t2''
          | (Tnil, Tnil) ->
              ()
          | (_, _) ->
              raise (Unify [])
        end
  with Unify trace ->
    raise (Unify ((t1, t2)::trace))

and moregen_list inst_nongen type_pairs env tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    raise (Unify []);
  List.iter2 (moregen inst_nongen type_pairs env) tl1 tl2

and moregen_fields inst_nongen type_pairs env ty1 ty2 =
  let (fields1, rest1) = flatten_fields ty1
  and (fields2, rest2) = flatten_fields ty2 in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  if miss1 <> [] then raise (Unify []);
  moregen inst_nongen type_pairs env rest1
    (build_fields (repr ty2).level miss2 rest2);
  List.iter
    (fun (n, k1, t1, k2, t2) ->
       moregen_kind k1 k2;
       try moregen inst_nongen type_pairs env t1 t2 with Unify trace ->
         raise (Unify ((newty (Tfield(n, k1, t1, rest2)),
                        newty (Tfield(n, k2, t2, rest2)))::trace)))
    pairs

and moregen_kind k1 k2 =
  let k1 = field_kind_repr k1 in
  let k2 = field_kind_repr k2 in
  match k1, k2 with
    (Fvar r, (Fvar _ | Fpresent))  -> r := Some k2
  | (Fpresent, Fpresent)           -> ()
  | _                              -> raise (Unify [])

and moregen_row inst_nongen type_pairs env row1 row2 =
  let row1 = row_repr row1 and row2 = row_repr row2 in
  let r1, r2, pairs = merge_row_fields row1.row_fields row2.row_fields in
  let r1, r2 =
    if row2.row_closed then
      filter_row_fields true r1, filter_row_fields false r2
    else r1, r2
  in
  if r1 <> [] || row1.row_closed && (not row2.row_closed || r2 <> []) 
  then raise (Unify []);
  let rm1 = repr row1.row_more and rm2 = repr row2.row_more in
  let ext =
    if not (static_row row2) then moregen_occur env rm1.level rm2;
    if r2 = [] then rm2 else
    let ty = newty2 generic_level (Tvariant{row2 with row_fields = r2}) in
    moregen_occur env rm1.level ty;
    ty
  in
  if ext != rm1 then rm1.desc <- Tlink ext;
  List.iter
    (fun (l,f1,f2) ->
      let f1 = row_field_repr f1 and f2 = row_field_repr f2 in
      if f1 == f2 then () else
      match f1, f2 with
        Rpresent(Some t1), Rpresent(Some t2) ->
          moregen inst_nongen type_pairs env t1 t2
      | Rpresent None, Rpresent None -> ()
      | Reither(false, tl1, _, e1), Rpresent(Some t2) ->
          e1 := Some f2;
          List.iter (fun t1 -> moregen inst_nongen type_pairs env t1 t2) tl1
      | Reither(c1, tl1, _, e1), Reither(c2, tl2, m2, e2) ->
          if c1 && not c2 then raise(Unify []);
          e1 := Some (Reither (c2, [], m2, e2));
          if List.length tl1 = List.length tl2 then
            List.iter2 (moregen inst_nongen type_pairs env) tl1 tl2
          else begin match tl2 with
            t2 :: _ ->
              List.iter (fun t1 -> moregen inst_nongen type_pairs env t1 t2)
                tl1
          | [] ->
              if tl1 <> [] then raise (Unify [])
          end
      | Reither(true, [], _, e1), Rpresent None -> e1 := Some f2
      | Reither(_, _, _, e1), Rabsent -> e1 := Some f2
      | Rabsent, Rabsent -> ()
      | _ -> raise (Unify []))
    pairs

(*
   Non-generic variable can be instanciated only if [inst_nongen] is
   true. So, [inst_nongen] should be set to false if the subject might
   contain non-generic variables (and we do not want them to be
   instanciated).
   Usually, the subject is given by the user, and the pattern
   is unimportant.  So, no need to propagate abbreviations.
*)
let moregeneral env inst_nongen pat_sch subj_sch =
  let old_level = !current_level in
  current_level := generic_level - 1;
  (*
     Generic variables are first duplicated with [instance].  So,
     their levels are lowered to [generic_level - 1].  The subject is
     then copied with [duplicate_type].  That way, its levels won't be
     changed.
  *)
  let subj = duplicate_type (instance subj_sch) in
  current_level := generic_level;
  (* Duplicate generic variables *)
  let patt = instance pat_sch in
  let res =
    try moregen inst_nongen (TypePairs.create 13) env patt subj; true with
      Unify _ -> false
  in
  current_level := old_level;
  res


                 (*********************************************)
                 (*  Equivalence between parameterized types  *)
                 (*********************************************)


let rec eqtype rename type_pairs subst env t1 t2 =
  if t1 == t2 then () else
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then () else

  try
    match (t1.desc, t2.desc) with
      (Tvar, Tvar) when rename ->
        begin try
          if List.assq t1 !subst != t2 then raise (Unify [])
        with Not_found ->
          subst := (t1, t2) :: !subst
        end
    | (Tconstr (p1, [], _), Tconstr (p2, [], _)) when Path.same p1 p2 ->
        ()
    | _ ->
        let t1' = expand_head env t1 in
        let t2' = expand_head env t2 in
        (* Expansion may have changed the representative of the types... *)
        let t1' = repr t1' and t2' = repr t2' in
        if t1' == t2' then () else
        begin try
          TypePairs.find type_pairs (t1', t2')
        with Not_found ->
          TypePairs.add type_pairs (t1', t2') ();
          match (t1'.desc, t2'.desc) with
            (Tvar, Tvar) when rename ->
              begin try
                if List.assq t1' !subst != t2' then raise (Unify [])
              with Not_found ->
                subst := (t1', t2') :: !subst
              end
          | (Tarrow (l1, t1, u1, _), Tarrow (l2, t2, u2, _)) when l1 = l2
            || !Clflags.classic && not (is_optional l1 || is_optional l2) ->
              eqtype rename type_pairs subst env t1 t2;
              eqtype rename type_pairs subst env u1 u2;
          | (Ttuple tl1, Ttuple tl2) ->
              eqtype_list rename type_pairs subst env tl1 tl2
          | (Tconstr (p1, tl1, _), Tconstr (p2, tl2, _))
                when Path.same p1 p2 ->
              eqtype_list rename type_pairs subst env tl1 tl2
          | (Tvariant row1, Tvariant row2) ->
              eqtype_row rename type_pairs subst env row1 row2
          | (Tobject (fi1, nm1), Tobject (fi2, nm2)) ->
              eqtype_fields rename type_pairs subst env fi1 fi2
          | (Tfield _, Tfield _) ->       (* Actually unused *)
              eqtype_fields rename type_pairs subst env t1' t2'
          | (Tfield (_, kind, _, t1''), _)
                when field_kind_repr kind = Fabsent ->
              eqtype rename type_pairs subst env t1'' t2'
          | (_, Tfield (_, kind, _, t2''))
                when field_kind_repr kind = Fabsent ->
              eqtype rename type_pairs subst env t1' t2''
          | (Tnil, Tnil) ->
              ()
          | (_, _) ->
              raise (Unify [])
        end
  with Unify trace ->
    raise (Unify ((t1, t2)::trace))

and eqtype_list rename type_pairs subst env tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    raise (Unify []);
  List.iter2 (eqtype rename type_pairs subst env) tl1 tl2

and eqtype_fields rename type_pairs subst env ty1 ty2 =
  let (fields1, rest1) = flatten_fields ty1
  and (fields2, rest2) = flatten_fields ty2 in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  eqtype rename type_pairs subst env rest1 rest2;
  if (miss1 <> []) || (miss2 <> []) then raise (Unify []);
  List.iter
    (function (n, k1, t1, k2, t2) ->
       eqtype_kind k1 k2;
       try eqtype rename type_pairs subst env t1 t2 with Unify trace ->
         raise (Unify ((newty (Tfield(n, k1, t1, rest2)),
                        newty (Tfield(n, k2, t2, rest2)))::trace)))
    pairs

and eqtype_kind k1 k2 =
  let k1 = field_kind_repr k1 in
  let k2 = field_kind_repr k2 in
  match k1, k2 with
    (Fvar _, Fvar _)
  | (Fpresent, Fpresent) -> ()
  | _                    -> raise (Unify [])

and eqtype_row rename type_pairs subst env row1 row2 =
  let row1 = row_repr row1 and row2 = row_repr row2 in
  let r1, r2, pairs = merge_row_fields row1.row_fields row2.row_fields in
  if row1.row_closed <> row2.row_closed
  || not row1.row_closed && (r1 <> [] || r2 <> [])
  || filter_row_fields false (r1 @ r2) <> []
  then raise (Unify []);
  if not (static_row row1) then
    eqtype rename type_pairs subst env row1.row_more row2.row_more;
  List.iter
    (fun (_,f1,f2) ->
      match row_field_repr f1, row_field_repr f2 with
        Rpresent(Some t1), Rpresent(Some t2) ->
          eqtype rename type_pairs subst env t1 t2
      | Reither(true, [], _, _), Reither(true, [], _, _) ->
          ()
      | Reither(false, t1::tl1, _, _), Reither(false, t2::tl2, _, _) ->
          eqtype rename type_pairs subst env t1 t2;
          if List.length tl1 = List.length tl2 then
            (* if same length allow different types (meaning?) *)
            List.iter2 (eqtype rename type_pairs subst env) tl1 tl2
          else begin
            (* otherwise everything must be equal *)
            List.iter (eqtype rename type_pairs subst env t1) tl2;
            List.iter (fun t1 -> eqtype rename type_pairs subst env t1 t2) tl1
          end
      | Rpresent None, Rpresent None -> ()
      | Rabsent, Rabsent -> ()
      | _ -> raise (Unify []))
    pairs
     

(* Two modes: with or without renaming of variables *)
let equal env rename tyl1 tyl2 =
  try
    eqtype_list rename (TypePairs.create 11) (ref []) env tyl1 tyl2; true
  with
    Unify _ -> false


                          (*************************)
                          (*  Class type matching  *)
                          (*************************)


type class_match_failure =
    CM_Virtual_class
  | CM_Parameter_arity_mismatch of int * int
  | CM_Type_parameter_mismatch of (type_expr * type_expr) list
  | CM_Class_type_mismatch of class_type * class_type
  | CM_Parameter_mismatch of (type_expr * type_expr) list
  | CM_Val_type_mismatch of string * (type_expr * type_expr) list
  | CM_Meth_type_mismatch of string * (type_expr * type_expr) list
  | CM_Non_mutable_value of string
  | CM_Missing_value of string
  | CM_Missing_method of string
  | CM_Hide_public of string
  | CM_Hide_virtual of string
  | CM_Public_method of string
  | CM_Private_method of string
  | CM_Virtual_method of string

exception Failure of class_match_failure list

let rec moregen_clty trace type_pairs env cty1 cty2 =
  try
    match cty1, cty2 with
      Tcty_constr (_, _, cty1), _ ->
        moregen_clty true type_pairs env cty1 cty2
    | _, Tcty_constr (_, _, cty2) ->
        moregen_clty true type_pairs env cty1 cty2
    | Tcty_fun (l1, ty1, cty1'), Tcty_fun (l2, ty2, cty2') when l1 = l2 ->
        begin try moregen true type_pairs env ty1 ty2 with Unify trace ->
          raise (Failure [CM_Parameter_mismatch (expand_trace env trace)])
        end;
        moregen_clty false type_pairs env cty1' cty2'
    | Tcty_signature sign1, Tcty_signature sign2 ->
        let ty1 = object_fields (repr sign1.cty_self) in
        let ty2 = object_fields (repr sign2.cty_self) in
        let (fields1, rest1) = flatten_fields ty1
        and (fields2, rest2) = flatten_fields ty2 in
        let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
        List.iter
          (fun (lab, k1, t1, k2, t2) ->
            begin try moregen true type_pairs env t1 t2 with Unify trace ->
              raise (Failure [CM_Meth_type_mismatch
                                 (lab, expand_trace env trace)])
           end)
        pairs;
      Vars.iter
        (fun lab (mut, ty) ->
           let (mut', ty') = Vars.find lab sign1.cty_vars in
           try moregen true type_pairs env ty' ty with Unify trace ->
             raise (Failure [CM_Val_type_mismatch
                                (lab, expand_trace env trace)]))
        sign2.cty_vars
  | _ ->
      raise (Failure [])
  with
    Failure error when trace ->
      raise (Failure (CM_Class_type_mismatch (cty1, cty2)::error))

let match_class_types env pat_sch subj_sch =
  let type_pairs = TypePairs.create 53 in
  let old_level = !current_level in
  current_level := generic_level - 1;
  (*
     Generic variables are first duplicated with [instance].  So,
     their levels are lowered to [generic_level - 1].  The subject is
     then copied with [duplicate_type].  That way, its levels won't be
     changed.
  *)
  let (_, subj_inst) = instance_class [] subj_sch in
  let subj = duplicate_class_type subj_inst in
  current_level := generic_level;
  (* Duplicate generic variables *)
  let (_, patt) = instance_class [] pat_sch in
  let res =
    let sign1 = signature_of_class_type patt in
    let sign2 = signature_of_class_type subj in
    let t1 = repr sign1.cty_self in
    let t2 = repr sign2.cty_self in
    TypePairs.add type_pairs (t1, t2) ();
    let (fields1, rest1) = flatten_fields (object_fields t1)
    and (fields2, rest2) = flatten_fields (object_fields t2) in
    let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
    let error =
      List.fold_right
        (fun (lab, k, _) err ->
           let err =
             let k = field_kind_repr k in
             begin match k with
               Fvar r -> r := Some Fabsent; err
             | _      ->                    CM_Hide_public lab::err
             end
           in
           if Concr.mem lab sign1.cty_concr then err
           else CM_Hide_virtual lab::err)
        miss1 []
    in
    let missing_method = List.map (fun (m, _, _) -> m) miss2 in
    let error =
      (List.map (fun m -> CM_Missing_method m) missing_method) @ error
    in
    (* Always succeeds *)
    moregen true type_pairs env rest1 rest2;
    let error =
      List.fold_right
        (fun (lab, k1, t1, k2, t2) err ->
           try moregen_kind k1 k2; err with
             Unify _ -> CM_Public_method lab::err)
        pairs error
    in
    let error =
      Vars.fold
        (fun lab (mut, ty) err ->
          try
            let (mut', ty') = Vars.find lab sign1.cty_vars in
            if mut = Mutable && mut' <> Mutable then
              CM_Non_mutable_value lab::err
            else
              err
          with Not_found ->
            CM_Missing_value lab::err)
        sign2.cty_vars error
    in
    let error =
      List.fold_right
        (fun e l ->
           if List.mem e missing_method then l else CM_Virtual_method e::l)
        (Concr.elements (Concr.diff sign2.cty_concr sign1.cty_concr))
        error
    in
    match error with
      [] ->
        begin try
          moregen_clty true type_pairs env patt subj;
          []
        with
          Failure r -> r
        end
    | error ->
        CM_Class_type_mismatch (patt, subj)::error
  in
  current_level := old_level;
  res

let rec equal_clty trace type_pairs subst env cty1 cty2 =
  try
    match cty1, cty2 with
      Tcty_constr (_, _, cty1), Tcty_constr (_, _, cty2) ->
        equal_clty true type_pairs subst env cty1 cty2
    | Tcty_constr (_, _, cty1), _ ->
        equal_clty true type_pairs subst env cty1 cty2
    | _, Tcty_constr (_, _, cty2) ->
        equal_clty true type_pairs subst env cty1 cty2
    | Tcty_fun (l1, ty1, cty1'), Tcty_fun (l2, ty2, cty2') when l1 = l2 ->
        begin try eqtype true type_pairs subst env ty1 ty2 with Unify trace ->
          raise (Failure [CM_Parameter_mismatch (expand_trace env trace)])
        end;
        equal_clty false type_pairs subst env cty1' cty2'
    | Tcty_signature sign1, Tcty_signature sign2 ->
        let ty1 = object_fields (repr sign1.cty_self) in
        let ty2 = object_fields (repr sign2.cty_self) in
        let (fields1, rest1) = flatten_fields ty1
        and (fields2, rest2) = flatten_fields ty2 in
        let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
        List.iter
          (fun (lab, k1, t1, k2, t2) ->
             begin try eqtype true type_pairs subst env t1 t2 with
               Unify trace ->
                 raise (Failure [CM_Meth_type_mismatch
                                    (lab, expand_trace env trace)])
             end)
          pairs;
        Vars.iter
          (fun lab (mut, ty) ->
             let (mut', ty') = Vars.find lab sign1.cty_vars in
             try eqtype true type_pairs subst env ty ty' with Unify trace ->
               raise (Failure [CM_Val_type_mismatch
                                  (lab, expand_trace env trace)]))
          sign2.cty_vars
    | _ ->
        raise
          (Failure (if trace then []
                    else [CM_Class_type_mismatch (cty1, cty2)]))
  with
    Failure error when trace ->
      raise (Failure (CM_Class_type_mismatch (cty1, cty2)::error))

(* XXX On pourrait autoriser l'instantiation du type des parametres... *)
(* XXX Correct ? (variables de type dans parametres et corps de classe *)
let match_class_declarations env patt_params patt_type subj_params subj_type =
  let type_pairs = TypePairs.create 53 in
  let subst = ref [] in
  let sign1 = signature_of_class_type patt_type in
  let sign2 = signature_of_class_type subj_type in
  let t1 = repr sign1.cty_self in
  let t2 = repr sign2.cty_self in
  TypePairs.add type_pairs (t1, t2) ();
  let (fields1, rest1) = flatten_fields (object_fields t1)
  and (fields2, rest2) = flatten_fields (object_fields t2) in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  let error =
    List.fold_right
      (fun (lab, k, _) err ->
        let err =
          let k = field_kind_repr k in
          begin match k with
            Fvar r -> err
          | _      -> CM_Hide_public lab::err
          end
        in
        if Concr.mem lab sign1.cty_concr then err
        else CM_Hide_virtual lab::err)
      miss1 []
  in
  let missing_method = List.map (fun (m, _, _) -> m) miss2 in
  let error =
    (List.map (fun m -> CM_Missing_method m) missing_method) @ error
  in
  (* Always succeeds *)
  eqtype true type_pairs subst env rest1 rest2;
  let error =
    List.fold_right
      (fun (lab, k1, t1, k2, t2) err ->
        let k1 = field_kind_repr k1 in
        let k2 = field_kind_repr k2 in
        match k1, k2 with
          (Fvar _, Fvar _)
        | (Fpresent, Fpresent) -> err
        | (Fvar _, Fpresent)   -> CM_Private_method lab::err
        | (Fpresent, Fvar _)  -> CM_Public_method lab::err
        | _                    -> assert false)
      pairs error
  in
  let error =
    Vars.fold
      (fun lab (mut, ty) err ->
         try
           let (mut', ty') = Vars.find lab sign1.cty_vars in
           if mut = Mutable && mut' <> Mutable then
             CM_Non_mutable_value lab::err
           else
             err
         with Not_found ->
           CM_Missing_value lab::err)
      sign2.cty_vars error
  in
  let error =
    List.fold_right
      (fun e l ->
        if List.mem e missing_method then l else CM_Virtual_method e::l)
      (Concr.elements (Concr.diff sign2.cty_concr sign1.cty_concr))
      error
  in
  match error with
    [] ->
      begin try
        let lp = List.length patt_params in
        let ls = List.length subj_params in
        if lp  <> ls then
          raise (Failure [CM_Parameter_arity_mismatch (lp, ls)]);
        List.iter2 (fun p s ->
          try eqtype true type_pairs subst env p s with Unify trace ->
            raise (Failure [CM_Type_parameter_mismatch
                               (expand_trace env trace)]))
          patt_params subj_params;
        equal_clty false type_pairs subst env patt_type subj_type;
        []
      with
        Failure r -> r
      end
  | error ->
      error


                              (***************)
                              (*  Subtyping  *)
                              (***************)


(**** Build a subtype of a given type. ****)

let rec filter_firsts pred = function
    [] -> []
  | a :: r as l -> if pred a then l else filter_firsts pred r

(* build_subtype:
   [visited] traces traversed object and variant types
   [loops] is a mapping from variables to variables, to reproduce
     positive loops in a class type
   [posi] true if the current variance is positive
   [onlyloop] does not open types, only build loops
     true on the left side of an arrow, for efficiency reasons *)

let rec build_subtype env visited loops posi onlyloop t =
  let t = repr t in
  let loops =
    if onlyloop then filter_firsts (fun (ty, _) -> deep_occur ty t) loops
    else loops
  in
  if onlyloop && loops = [] then (t, false) else
  match t.desc with
    Tvar ->
      if posi then
        try
          (List.assq t loops, true)
        with Not_found ->
          (t, false)
      else
        (t, false)
  | Tarrow(l, t1, t2, _) ->
      if List.memq t visited then (t, false) else
      let visited = t :: visited in
      let (t1', c1) = build_subtype env visited loops (not posi) true t1 in
      (* let (t1', c1) = (t1, false) in *)
      let (t2', c2) = build_subtype env visited loops posi onlyloop t2 in
      if c1 || c2 then (newty (Tarrow(l, t1', t2', Cok)), true)
      else (t, false)
  | Ttuple tlist ->
      if List.memq t visited then (t, false) else
      let visited = t :: visited in
      let tlist' =
        List.map (build_subtype env visited loops posi onlyloop) tlist
      in
      if List.exists snd tlist' then
        (newty (Ttuple (List.map fst tlist')), true)
      else (t, false)
  | Tconstr(p, tl, abbrev) when not onlyloop && generic_abbrev env p ->
      let t' = repr (expand_abbrev env t) in
      begin try match t'.desc with
        Tobject _ when posi ->
          if List.memq t' visited then (t, false) else
          let rec lid_of_path sharp = function
              Path.Pident id ->
                Longident.Lident (sharp ^ Ident.name id)
            | Path.Pdot (p1, s, _) ->
                Longident.Ldot (lid_of_path "" p1, sharp ^ s)
            | Path.Papply (p1, p2) ->
                Longident.Lapply (lid_of_path sharp p1, lid_of_path "" p2)
          in
          let path, cl_abbr = Env.lookup_type (lid_of_path "#" p) env in
          let body =
            match cl_abbr.type_manifest with Some ty -> ty
            | None -> assert false in
          let ty =
            subst env t'.level abbrev None cl_abbr.type_params tl body in
          let ty = repr ty in
          let ty1 =
            match ty.desc with
              Tobject(ty1,{contents=Some(p',_)}) when Path.same p p' -> ty1
            | _ -> raise Not_found
          in
          ty.desc <- Tvar;
          let t'' = newvar () in
          let visited = t' :: visited and loops = (ty, t'') :: loops in
          let (ty1', _) = build_subtype env visited loops posi onlyloop ty1 in
          assert (t''.desc = Tvar);
          t''.desc <- Tobject (ty1', ref None);
          (try unify env ty t with Unify _ -> assert false);
          (t'', true)
      | _ -> raise Not_found
      with Not_found -> build_subtype env visited loops posi onlyloop t'
      end
  | Tconstr(p, tl, abbrev) ->
      begin try
        let decl = Env.find_type p env in
        let tl' =
          List.map2
            (fun (co,cn) t ->
              if cn then
                if co then (t, false)
                else build_subtype env visited loops (not posi) onlyloop t
              else
                if co then build_subtype env visited loops posi onlyloop t
                else (newvar(), true))
            decl.type_variance tl
        in
        if List.exists snd tl' then
          (newconstr p (List.map fst tl'), true)
        else
          (t, false)
      with Not_found ->
        (t, false)
      end
  | Tvariant row ->
      if List.memq t visited then (t, false) else
      let visited = t :: visited in
      let row = row_repr row in
      if not (static_row row) then (t, false) else
      let bound = ref row.row_bound in
      let fields =
        List.map
          (fun (l,f as orig) -> match row_field_repr f with
            Rpresent None ->
              if posi then
                (l, Reither(true, [], false, ref None)), false
              else
                orig, false
          | Rpresent(Some t) ->
              let (t', c) =
                build_subtype env visited loops posi onlyloop t in
              if posi && not onlyloop then begin
                bound := t' :: !bound;
                (l, Reither(false, [t'], false, ref None)), c
              end else
                (l, Rpresent(Some t')), c
          | _ -> assert false)
          (filter_row_fields false row.row_fields)
      in
      if posi && fields = [] then (t, false) else
      let row =
        if posi then
          {row_fields = List.map fst fields; row_more = newvar();
           row_bound = !bound; row_closed = true;
           row_name = if List.exists snd fields then None else row.row_name }
        else
          {row_fields = List.map fst fields; row_more = newvar ();
           row_bound = !bound; row_closed = false; row_name = None}
      in
      (newty (Tvariant row), true)
  | Tobject (t1, _) when opened_object t1 ->
      (t, false)
  | Tobject (t1, _) ->
      if List.memq t visited then (t, false) else
      let (t1', _) =
        build_subtype env (t :: visited) loops posi onlyloop t1 in
      (newty (Tobject (t1', ref None)), true)
  | Tfield(s, _, t1, t2) (* Always present *) ->
      let (t1', c1) = build_subtype env visited loops posi onlyloop t1 in
      let (t2', c2) = build_subtype env visited loops posi onlyloop t2 in
      if c1 || c2 then
        (newty (Tfield(s, Fpresent, t1', t2')), true)
      else
        (t, false)
  | Tnil ->
      if posi && not onlyloop then
        let v = newvar () in
        (v, true)
      else
        (t, false)
  | Tsubst _ | Tlink _ ->
      assert false

let enlarge_type env ty =
  let (ty', _) = build_subtype env [] [] true false ty in
  ty'

(**** Check whether a type is a subtype of another type. ****)

(*
    During the traversal, a trace of visited types is maintained. It
    is printed in case of error.
    Constraints (pairs of types that must be equals) are accumulated
    rather than being enforced straight. Indeed, the result would
    otherwise depend on the order in which these constraints are
    enforced.
    A function enforcing these constraints is returned. That way, type
    variables can be bound to their actual values before this function
    is called (see Typecore).
    Only well-defined abbreviations are expanded (hence the tests
    [generic_abbrev ...]).
*)

let subtypes = TypePairs.create 17

let subtype_error env trace =
  raise (Subtype (expand_trace env (List.rev trace), []))

let rec subtype_rec env trace t1 t2 cstrs =
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then [] else
  
  begin try
    TypePairs.find subtypes (t1, t2);
    cstrs
  with Not_found ->
    TypePairs.add subtypes (t1, t2) ();
    match (t1.desc, t2.desc) with
      (Tvar, _) | (_, Tvar) ->
        (trace, t1, t2)::cstrs
    | (Tarrow(l1, t1, u1, _), Tarrow(l2, t2, u2, _)) when l1 = l2
      || !Clflags.classic && not (is_optional l1 || is_optional l2) ->
        let cstrs = subtype_rec env ((t2, t1)::trace) t2 t1 cstrs in
        subtype_rec env ((u1, u2)::trace) u1 u2 cstrs
    | (Ttuple tl1, Ttuple tl2) ->
        subtype_list env trace tl1 tl2 cstrs
    | (Tconstr(p1, [], _), Tconstr(p2, [], _)) when Path.same p1 p2 ->
        cstrs
    | (Tconstr(p1, tl1, abbrev1), _) when generic_abbrev env p1 ->
        subtype_rec env trace (expand_abbrev env t1) t2 cstrs
    | (_, Tconstr(p2, tl2, abbrev2)) when generic_abbrev env p2 ->
        subtype_rec env trace t1 (expand_abbrev env t2) cstrs
    | (Tconstr(p1, tl1, _), Tconstr(p2, tl2, _)) when Path.same p1 p2 ->
        begin try
          let decl = Env.find_type p1 env in
          List.fold_left2
            (fun cstrs (co, cn) (t1, t2) ->
              if co then
                if cn then
                  (trace, newty2 t1.level (Ttuple[t1]),
                   newty2 t2.level (Ttuple[t2])) :: cstrs 
                else subtype_rec env ((t1, t2)::trace) t1 t2 cstrs
              else
                if cn then subtype_rec env ((t2, t1)::trace) t2 t1 cstrs
                else cstrs)
            cstrs decl.type_variance (List.combine tl1 tl2)
        with Not_found ->
          (trace, t1, t2)::cstrs
        end
    | (Tobject (f1, _), Tobject (f2, _))
              when opened_object f1 && opened_object f2 ->
        (* Same row variable implies same object. *)
        (trace, t1, t2)::cstrs
    | (Tobject (f1, _), Tobject (f2, _)) ->
        subtype_fields env trace f1 f2 cstrs
    | (Tvariant row1, Tvariant row2) ->
        let row1 = row_repr row1 and row2 = row_repr row2 in
        begin try
          if not row1.row_closed then raise Exit;
          let r1, r2, pairs =
            merge_row_fields row1.row_fields row2.row_fields in
          if filter_row_fields false r1 <> [] then raise Exit;
          List.fold_left
            (fun cstrs (_,f1,f2) ->
              match row_field_repr f1, row_field_repr f2 with
                (Rpresent None|Reither(true,_,_,_)), Rpresent None ->
                  cstrs
              | Rpresent(Some t1), Rpresent(Some t2) ->
                  subtype_rec env ((t1, t2)::trace) t1 t2 cstrs
              | Reither(false, t1::_, _, _), Rpresent(Some t2) ->
                  subtype_rec env ((t1, t2)::trace) t1 t2 cstrs
              | Rabsent, _ -> cstrs
              | _ -> raise Exit)
            cstrs pairs
        with Exit ->
          (trace, t1, t2)::cstrs
        end
    | (_, _) ->
        (trace, t1, t2)::cstrs
  end

and subtype_list env trace tl1 tl2 cstrs =
  if List.length tl1 <> List.length tl2 then
    subtype_error env trace;
  List.fold_left2
    (fun cstrs t1 t2 -> subtype_rec env ((t1, t2)::trace) t1 t2 cstrs)
    cstrs tl1 tl2

and subtype_fields env trace ty1 ty2 cstrs =
  let (fields1, rest1) = flatten_fields ty1 in
  let (fields2, rest2) = flatten_fields ty2 in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  [(trace, rest1, build_fields (repr ty2).level miss2 (newvar ()))]
    @
  begin match rest2.desc with
    Tnil   -> []
  | _      -> [(trace, build_fields (repr ty1).level miss1 rest1, rest2)]
  end
    @
  (List.fold_left
     (fun cstrs (_, k1, t1, k2, t2) ->
        (* Theses fields are always present *)
        subtype_rec env ((t1, t2)::trace) t1 t2 cstrs)
     cstrs pairs)

let subtype env ty1 ty2 =
  TypePairs.clear subtypes;
  (* Build constraint set. *)
  let cstrs = subtype_rec env [(ty1, ty2)] ty1 ty2 [] in
  TypePairs.clear subtypes;
  (* Enforce constraints. *)
  function () ->
    List.iter
      (function (trace0, t1, t2) ->
         try unify env t1 t2 with Unify trace ->
           raise (Subtype (expand_trace env (List.rev trace0),
                           List.tl (List.tl trace))))
      (List.rev cstrs)

                              (*******************)
                              (*  Miscellaneous  *)
                              (*******************)


let unalias ty =
  let ty = repr ty in
  match ty.desc with
    Tvar ->
      ty
  | Tvariant row ->
      let row = row_repr row in
      let more = row.row_more in
      newty2 ty.level
        (Tvariant {row with row_more = newty2 more.level more.desc})
  | _ ->
      newty2 ty.level ty.desc

let unroll_abbrev id tl ty =
  let ty = repr ty in
  if (ty.desc = Tvar) || (List.exists (deep_occur ty) tl) then
    ty
  else
    let ty' = newty2 ty.level ty.desc in
    ty.desc <- Tlink (newty2 ty.level
                             (Tconstr (Path.Pident id, tl, ref Mnil)));
    ty'

(* Return the arity (as for curried functions) of the given type. *)
let rec arity ty =
  match (repr ty).desc with
    Tarrow(_, t1, t2, _) -> 1 + arity t2
  | _ -> 0

(* Check whether an abbreviation expands to itself. *)
let rec cyclic_abbrev env id ty =
  let ty = repr ty in
  match ty.desc with
    Tconstr (Path.Pident id', _, _) when Ident.same id id' ->
      true
  | Tconstr (p, tl, abbrev) ->
      begin try
        cyclic_abbrev env id (try_expand_head env ty)
      with Cannot_expand ->
        false
      end
  | _ ->
      false


(* Normalize a type before printing, saving... *)
let rec normalize_type_rec env ty =
  let ty = repr ty in
  if ty.level >= lowest_level then begin
    mark_type_node ty;
    begin match ty.desc with
    | Tvariant row ->
      let row = row_repr row in
      let fields = List.map
          (fun (l,f) ->
            let f = row_field_repr f in l,
            match f with Reither(b, ty::(_::_ as tyl), m, e) ->
              let tyl' =
                List.fold_left
                  (fun tyl ty ->
                    if List.exists (fun ty' -> equal env false [ty] [ty']) tyl
                    then tyl else ty::tyl)
                  [ty] tyl
              in
              if List.length tyl' < List.length tyl + 1 then
                let f = Reither(b, List.rev tyl', m, ref None) in
                e := Some f;
                f
              else f
            | _ -> f)
          row.row_fields
      and bound = List.fold_left
          (fun tyl ty -> if List.memq ty tyl then tyl else ty :: tyl)
          [] (List.map repr row.row_bound)
      in ty.desc <- Tvariant {row with row_fields = fields; row_bound = bound}
    | Tobject (_, nm) ->
        begin match !nm with
        | None -> ()
        | Some (n, v :: l) ->
            let v' = repr v in
            begin match v'.desc with
            | Tvar -> if v' != v then nm := Some (n, v' :: l)
            | Tnil -> ty.desc <- Tconstr (n, l, ref Mnil)
            | _ -> nm := None
            end
        | _ ->
            fatal_error "Ctype.normalize_type_rec"
        end
    | _ -> ()
    end;
    iter_type_expr (normalize_type_rec env) ty
  end

let normalize_type env ty =
  normalize_type_rec env ty;
  unmark_type ty
      

                              (*************************)
                              (*  Remove dependencies  *)
                              (*************************)


(*
   Variables are left unchanged. Other type nodes are duplicated, with
   levels set to generic level.
   During copying, the description of a (non-variable) node is first
   replaced by a link to a stub ([Tsubst (newgenvar ())]).
   Once the copy is made, it replaces the stub.
   After copying, the description of node, which was stored by
   [save_desc], must be put back, using [cleanup_types].
*)

let rec nondep_type_rec env id ty =
  let ty = repr ty in
  match ty.desc with
    Tvar     -> ty
  | Tsubst ty -> ty
  | _ ->
    let desc = ty.desc in
    save_desc ty desc;
    let ty' = newgenvar () in        (* Stub *)
    ty.desc <- Tsubst ty';
    ty'.desc <-
      begin match desc with
      | Tconstr(p, tl, abbrev) ->
          if Path.isfree id p then
            begin try
              Tlink (nondep_type_rec env id
                       (expand_abbrev env (newty2 ty.level desc)))
              (*
                 The [Tlink] is important. The expanded type may be a
                 variable, or may not be completely copied yet
                 (recursive type), so one cannot just take its
                 description.
               *)
            with Cannot_expand ->
              raise Not_found
            end
          else
            Tconstr(p, List.map (nondep_type_rec env id) tl, ref Mnil)
      | Tobject (t1, name) ->
          Tobject (nondep_type_rec env id t1,
                 ref (match !name with
                        None -> None
                      | Some (p, tl) ->
                          if Path.isfree id p then None
                          else Some (p, List.map (nondep_type_rec env id) tl)))
      | Tvariant row ->
          let row = row_repr row in
          let more = repr row.row_more in
          (* We must substitute in a subtle way *)
          begin match more.desc with
            Tsubst ty2 ->
              (* This variant type has been already copied *)
              ty.desc <- Tsubst ty2; (* avoid Tlink in the new type *)
              Tlink ty2
          | _ ->
              let static = static_row row in
              (* Register new type first for recursion *)
              save_desc more more.desc;
              more.desc <- ty.desc;
              let more' = if static then newgenvar () else more in
              (* Return a new copy *)
              let row = copy_row (nondep_type_rec env id) row true more' in
              match row.row_name with
                Some (p, tl) when Path.isfree id p ->
                  Tvariant {row with row_name = None}
              | _ -> Tvariant row
          end
      | _ -> copy_type_desc (nondep_type_rec env id) desc
      end;
    ty'

let nondep_type env id ty =
  try
    let ty' = nondep_type_rec env id ty in
    cleanup_types ();
    unmark_type ty';
    ty'
  with Not_found ->
    cleanup_types ();
    raise Not_found

(* Preserve sharing inside type declarations. *)
let nondep_type_decl env mid id is_covariant decl =
  try
    let params = List.map (nondep_type_rec env mid) decl.type_params in
    let decl =
      { type_params = params;
        type_arity = decl.type_arity;
        type_kind =
          begin try
            match decl.type_kind with
              Type_abstract ->
                Type_abstract
            | Type_variant cstrs ->
                Type_variant(List.map
                  (fun (c, tl) -> (c, List.map (nondep_type_rec env mid) tl))
                  cstrs)
            | Type_record(lbls, rep) ->
                Type_record(
                  List.map
                    (fun (c, mut, t) -> (c, mut, nondep_type_rec env mid t))
                    lbls,
                  rep)
          with Not_found when is_covariant ->
            Type_abstract
          end;
        type_manifest =
          begin try
            match decl.type_manifest with
              None -> None
            | Some ty ->
                Some (unroll_abbrev id params (nondep_type_rec env mid ty))
          with Not_found when is_covariant ->
            None
          end;
        type_variance = decl.type_variance;
      }
    in
    cleanup_types ();
    List.iter unmark_type decl.type_params;
    begin match decl.type_kind with
      Type_abstract -> ()
    | Type_variant cstrs ->
        List.iter (fun (c, tl) -> List.iter unmark_type tl) cstrs
    | Type_record(lbls, rep) ->
        List.iter (fun (c, mut, t) -> unmark_type t) lbls
    end;
    begin match decl.type_manifest with
      None    -> ()
    | Some ty -> unmark_type ty
    end;
    decl
  with Not_found ->
    cleanup_types ();
    raise Not_found

(* Preserve sharing inside class types. *)
let nondep_class_signature env id sign =
  { cty_self = nondep_type_rec env id sign.cty_self;
    cty_vars =
      Vars.map (function (m, t) -> (m, nondep_type_rec env id t))
        sign.cty_vars;
    cty_concr = sign.cty_concr }

let rec nondep_class_type env id =
  function
    Tcty_constr (p, _, cty) when Path.isfree id p ->
      nondep_class_type env id cty
  | Tcty_constr (p, tyl, cty) ->
      Tcty_constr (p, List.map (nondep_type_rec env id) tyl,
                   nondep_class_type env id cty)
  | Tcty_signature sign ->
      Tcty_signature (nondep_class_signature env id sign)
  | Tcty_fun (l, ty, cty) ->
      Tcty_fun (l, nondep_type_rec env id ty, nondep_class_type env id cty)

let nondep_class_declaration env id decl =
  assert (not (Path.isfree id decl.cty_path));
  let decl =
    { cty_params = List.map (nondep_type_rec env id) decl.cty_params;
      cty_type = nondep_class_type env id decl.cty_type;
      cty_path = decl.cty_path;
      cty_new =
        begin match decl.cty_new with
          None    -> None
        | Some ty -> Some (nondep_type_rec env id ty)
        end }
  in
  cleanup_types ();
  List.iter unmark_type decl.cty_params;
  unmark_class_type decl.cty_type;
  begin match decl.cty_new with
    None    -> ()
  | Some ty -> unmark_type ty
  end;
  decl

let nondep_cltype_declaration env id decl =
  assert (not (Path.isfree id decl.clty_path));
  let decl =
    { clty_params = List.map (nondep_type_rec env id) decl.clty_params;
      clty_type = nondep_class_type env id decl.clty_type;
      clty_path = decl.clty_path }
  in
  cleanup_types ();
  List.iter unmark_type decl.clty_params;
  unmark_class_type decl.clty_type;
  decl

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

(* $Id: genprintval.ml,v 1.26 2001/09/15 16:34:59 ddr Exp $ *)

(* To print values *)

open Misc
open Format
open Longident
open Path
open Types
open Outcometree

module type OBJ =
  sig
    type t
    val obj : t -> 'a
    val is_block : t -> bool
    val tag : t -> int
    val size : t -> int
    val field : t -> int -> t
  end

module type EVALPATH =
  sig
    type value
    val eval_path: Path.t -> value
    exception Error
    val same_value: value -> value -> bool
  end

module type S =
  sig
    type t
    val install_printer :
          Path.t -> Types.type_expr -> (formatter -> t -> unit) -> unit
    val remove_printer : Path.t -> unit
    val outval_of_untyped_exception : t -> Outcometree.out_value
    val outval_of_value :
          int -> int ->
          (int -> t -> Types.type_expr -> Outcometree.out_value option) ->
          Env.t -> t -> type_expr -> Outcometree.out_value
    val print_outval : formatter -> Outcometree.out_value -> unit
  end

module Make(O : OBJ)(EVP : EVALPATH with type value = O.t) = struct

    type t = O.t

    (* Given an exception value, we cannot recover its type,
       hence we cannot print its arguments in general.
       Here, we do a feeble attempt to print
       integer, string and float arguments... *)
    let outval_of_untyped_exception_args obj start_offset =
      if O.size obj > start_offset then begin
        let list = ref [] in
        for i = start_offset to O.size obj - 1 do
          let arg = O.field obj i in
          if not (O.is_block arg) then
            list := Oval_int (O.obj arg : int) :: !list
               (* Note: this could be a char or a constant constructor... *)
          else if O.tag arg = Obj.string_tag then
            list :=
              Oval_string (String.escaped (O.obj arg : string)) :: !list
          else if O.tag arg = Obj.double_tag then
            list := Oval_float (O.obj arg : float) :: !list
          else
            list := Oval_constr (Oide_ident "_", []) :: !list
        done;
        List.rev !list
      end
      else []

    let outval_of_untyped_exception bucket =
      let name = (O.obj(O.field(O.field bucket 0) 0) : string) in
      let args =
        if (name = "Match_failure" || name = "Assert_failure")
        && O.size bucket = 2
        && O.tag(O.field bucket 1) = 0
        then outval_of_untyped_exception_args (O.field bucket 1) 0
        else outval_of_untyped_exception_args bucket 1 in
      Oval_constr (Oide_ident name, args)

    (* The user-defined printers. Also used for some builtin types. *)

    let printers = ref ([
      Pident(Ident.create "print_int"), Predef.type_int,
        (fun x -> Oval_int (O.obj x : int));
      Pident(Ident.create "print_float"), Predef.type_float,
        (fun x -> Oval_float (O.obj x : float));
      Pident(Ident.create "print_char"), Predef.type_char,
        (fun x -> Oval_char (O.obj x : char));
      Pident(Ident.create "print_string"), Predef.type_string,
        (fun x -> Oval_string (O.obj x : string));
      Pident(Ident.create "print_int32"), Predef.type_int32,
        (fun x -> Oval_string (Int32.to_string (O.obj x : int32)));
      Pident(Ident.create "print_nativeint"), Predef.type_nativeint,
        (fun x -> Oval_string (Nativeint.to_string (O.obj x : nativeint)));
      Pident(Ident.create "print_int64"), Predef.type_int64,
        (fun x -> Oval_string (Int64.to_string (O.obj x : int64)))
    ] : (Path.t * type_expr * (O.t -> Outcometree.out_value)) list)

    let install_printer path ty fn =
      let print_val ppf obj =
        try fn ppf obj with
        | exn ->
           fprintf ppf "<printer %a raised an exception>" Printtyp.path path in
      let printer obj = Oval_printer (fun ppf -> print_val ppf obj) in
      printers := (path, ty, printer) :: !printers

    let remove_printer path =
      let rec remove = function
      | [] -> raise Not_found
      | (p, ty, fn as printer) :: rem ->
          if Path.same p path then rem else printer :: remove rem in
      printers := remove !printers

    let find_printer env ty =
      let rec find = function
      | [] -> raise Not_found
      | (name, sch, printer) :: remainder ->
          if Ctype.moregeneral env false sch ty
          then printer
          else find remainder
      in find !printers

    (* Print a constructor or label, giving it the same prefix as the type
       it comes from. Attempt to omit the prefix if the type comes from
       a module that has been opened. *)

    let tree_of_qualified lookup_fun env ty_path name =
      match ty_path with
      | Pident id ->
          Oide_ident name
      | Pdot(p, s, pos) ->
          if try
               match (lookup_fun (Lident name) env).desc with
               | Tconstr(ty_path', _, _) -> Path.same ty_path ty_path'
               | _ -> false
             with Not_found -> false
          then Oide_ident name
          else Oide_dot (Printtyp.tree_of_path p, name)
      | Papply(p1, p2) ->
          Printtyp.tree_of_path ty_path

    let tree_of_constr =
      tree_of_qualified
        (fun lid env -> (Env.lookup_constructor lid env).cstr_res)

    and tree_of_label =
      tree_of_qualified (fun lid env -> (Env.lookup_label lid env).lbl_res)

    (* An abstract type *)

    let abstract_type =
      Ctype.newty (Tconstr (Pident (Ident.create "abstract"), [], ref Mnil))

    (* The main printing function *)

    let outval_of_value max_steps max_depth check_depth env obj ty =

      let printer_steps = ref max_steps in

      let rec tree_of_val depth obj ty =
        decr printer_steps;
        if !printer_steps < 0 || depth < 0 then Oval_ellipsis
        else begin
        try
          find_printer env ty obj
        with Not_found ->
          match (Ctype.repr ty).desc with
          | Tvar ->
              Oval_stuff "<poly>"
          | Tarrow(_, ty1, ty2, _) ->
              Oval_stuff "<fun>"
          | Ttuple(ty_list) ->
              Oval_tuple (tree_of_val_list 0 depth obj ty_list)
          | Tconstr(path, [], _) when Path.same path Predef.path_exn ->
              tree_of_exception depth obj
          | Tconstr(path, [ty_arg], _)
            when Path.same path Predef.path_list ->
              if O.is_block obj then
                match check_depth depth obj ty with
                  Some x -> x
                | None ->
                    let rec tree_of_conses tree_list obj =
                      if !printer_steps < 0 || depth < 0 then
                        Oval_ellipsis :: tree_list
                      else if O.is_block obj then
                        let tree =
                          tree_of_val (depth - 1) (O.field obj 0) ty_arg in
                        let next_obj = O.field obj 1 in
                        tree_of_conses (tree :: tree_list) next_obj
                      else tree_list
                    in
                    Oval_list (List.rev (tree_of_conses [] obj))
              else
                Oval_list []
          | Tconstr(path, [ty_arg], _)
            when Path.same path Predef.path_array ->
              let length = O.size obj in
              if length > 0 then
                match check_depth depth obj ty with
                  Some x -> x
                | None ->
                    let rec tree_of_items tree_list i =
                      if !printer_steps < 0 || depth < 0 then
                        Oval_ellipsis :: tree_list
                      else if i < length then
                        let tree =
                          tree_of_val (depth - 1) (O.field obj i) ty_arg in
                        tree_of_items (tree :: tree_list) (i + 1)
                      else tree_list
                    in
                    Oval_array (List.rev (tree_of_items [] 0))
              else
                Oval_array []
          | Tconstr(path, ty_list, _) ->
              begin try
                let decl = Env.find_type path env in
                match decl with
                | {type_kind = Type_abstract; type_manifest = None} ->
                    Oval_stuff "<abstr>"
                | {type_kind = Type_abstract; type_manifest = Some body} ->
                    tree_of_val depth obj
                      (try Ctype.apply env decl.type_params body ty_list with
                         Ctype.Cannot_apply -> abstract_type)
                | {type_kind = Type_variant constr_list} ->
                    let tag =
                      if O.is_block obj
                      then Cstr_block(O.tag obj)
                      else Cstr_constant(O.obj obj) in
                    let (constr_name, constr_args) =
                      Datarepr.find_constr_by_tag tag constr_list in
                    let ty_args =
                      List.map
                        (function ty ->
                           try Ctype.apply env decl.type_params ty ty_list with
                             Ctype.Cannot_apply -> abstract_type)
                        constr_args in
                    tree_of_constr_with_args (tree_of_constr env path)
                                           constr_name 0 depth obj ty_args
                | {type_kind = Type_record(lbl_list, rep)} ->
                    match check_depth depth obj ty with
                      Some x -> x
                    | None ->
                        let rec tree_of_fields pos = function
                          | [] -> []
                          | (lbl_name, _, lbl_arg) :: remainder ->
                              let ty_arg =
                                try
                                  Ctype.apply env decl.type_params lbl_arg
                                    ty_list
                                with
                                  Ctype.Cannot_apply -> abstract_type in
                              let lid = tree_of_label env path lbl_name in
                              let v =
                                tree_of_val (depth - 1) (O.field obj pos)
                                  ty_arg
                              in
                              (lid, v) :: tree_of_fields (pos + 1) remainder
                        in
                        Oval_record (tree_of_fields 0 lbl_list)
              with
                Not_found ->                (* raised by Env.find_type *)
                  Oval_stuff "<abstr>"
              | Datarepr.Constr_not_found -> (* raised by find_constr_by_tag *)
                  Oval_stuff "<unknown constructor>"
              end
          | Tvariant row ->
              let row = Btype.row_repr row in
              if O.is_block obj then
                let tag : int = O.obj (O.field obj 0) in
                let rec find = function
                  | (l, f) :: fields ->
                      if Btype.hash_variant l = tag then
                        match Btype.row_field_repr f with
                        | Rpresent(Some ty) ->
                            let args =
                              tree_of_val (depth - 1) (O.field obj 1) ty in
                            Oval_variant (l, Some args)
                        | _ -> find fields
                      else find fields
                  | [] -> Oval_stuff "<variant>" in
                find row.row_fields
              else
                let tag : int = O.obj obj in
                let rec find = function
                  | (l, _) :: fields ->
                      if Btype.hash_variant l = tag then
                        Oval_variant (l, None)
                      else find fields
                  | [] -> Oval_stuff "<variant>" in
                find row.row_fields
          | Tobject (_, _) ->
              Oval_stuff "<obj>"
          | Tsubst ty ->
              tree_of_val (depth - 1) obj ty
          | Tfield(_, _, _, _) | Tnil | Tlink _ ->
              fatal_error "Printval.outval_of_value"
        end

      and tree_of_val_list start depth obj ty_list =
        let rec tree_list i = function
          | [] -> []
          | ty :: ty_list ->
              let tree = tree_of_val (depth - 1) (O.field obj i) ty in
              tree :: tree_list (i + 1) ty_list in
      tree_list start ty_list

      and tree_of_constr_with_args
             tree_of_cstr cstr_name start depth obj ty_args =
        let lid = tree_of_cstr cstr_name in
        let args = tree_of_val_list start depth obj ty_args in
        Oval_constr (lid, args)

    and tree_of_exception depth bucket =
      let name = (O.obj(O.field(O.field bucket 0) 0) : string) in
      let lid = Longident.parse name in
      try
        (* Attempt to recover the constructor description for the exn
           from its name *)
        let cstr = Env.lookup_constructor lid env in
        let path =
          match cstr.cstr_tag with
            Cstr_exception p -> p | _ -> raise Not_found in
        (* Make sure this is the right exception and not an homonym,
           by evaluating the exception found and comparing with the
           identifier contained in the exception bucket *)
        if not (EVP.same_value (O.field bucket 0) (EVP.eval_path path))
        then raise Not_found;
        tree_of_constr_with_args
           (fun x -> Oide_ident x) name 1 depth bucket cstr.cstr_args
      with Not_found | EVP.Error ->
        match check_depth depth obj ty with
          Some x -> x
        | None -> outval_of_untyped_exception obj

    in tree_of_val max_depth obj ty

    exception Ellipsis

    let cautious f ppf arg = try f ppf arg with Ellipsis -> fprintf ppf "..."

    let print_outval ppf tree =
      let rec print_ident ppf =
        function
          Oide_ident s -> fprintf ppf "%s" s
        | Oide_dot (id, s) -> fprintf ppf "%a.%s" print_ident id s
        | Oide_apply (id1, id2) ->
            fprintf ppf "%a(%a)" print_ident id1 print_ident id2
      in
      let rec print_tree ppf =
        function
          Oval_tuple tree_list ->
            fprintf ppf "@[%a@]" (print_tree_list print_tree_1 ",") tree_list
        | tree -> print_tree_1 ppf tree
      and print_tree_1 ppf =
        function
          Oval_constr (name, [param]) ->
            fprintf ppf "@[<1>%a@ %a@]" print_ident name print_simple_tree
              param
        | Oval_constr (name, (_ :: _ as params)) ->
            fprintf ppf "@[<1>%a@ (%a)@]" print_ident name
              (print_tree_list print_tree_1 ",") params
        | Oval_variant (name, Some param) ->
            fprintf ppf "@[<2>`%s@ %a@]" name print_simple_tree param
        | tree -> print_simple_tree ppf tree
      and print_simple_tree ppf =
        function
          Oval_int i -> fprintf ppf "%i" i
        | Oval_float f -> fprintf ppf "%.12g" f
        | Oval_char c -> fprintf ppf "'%s'" (Char.escaped c)
        | Oval_string s ->
            (* String.escaped may raise [Invalid_argument "String.create"]
               if the escaped string is longer than [Sys.max_string_length] *)
            begin try
              fprintf ppf "\"%s\"" (String.escaped s)
            with Invalid_argument "String.create" ->
              fprintf ppf "<huge string>"
            end
        | Oval_list tl ->
            fprintf ppf "@[<1>[%a]@]" (print_tree_list print_tree_1 ";") tl
        | Oval_array tl ->
            fprintf ppf "@[<2>[|%a|]@]" (print_tree_list print_tree_1 ";") tl
        | Oval_constr (name, []) -> print_ident ppf name
        | Oval_variant (name, None) -> fprintf ppf "`%s" name
        | Oval_stuff s -> fprintf ppf "%s" s
        | Oval_record fel ->
            fprintf ppf "@[<1>{%a}@]" (cautious (print_fields true)) fel
        | Oval_ellipsis -> raise Ellipsis
        | Oval_printer f -> f ppf
        | tree -> fprintf ppf "@[<1>(%a)@]" (cautious print_tree) tree
      and print_fields first ppf =
        function
          [] -> ()
        | (name, tree) :: fields ->
            if not first then fprintf ppf ";@ ";
            fprintf ppf "@[<1>%a@ =@ %a@]" print_ident name
              (cautious print_tree) tree;
            print_fields false ppf fields
      and print_tree_list print_item sep ppf tree_list =
        let rec print_list first ppf =
          function
            [] -> ()
          | tree :: tree_list ->
              if not first then fprintf ppf "%s@ " sep;
              print_item ppf tree;
              print_list false ppf tree_list
        in
        cautious (print_list true) ppf tree_list
      in
      cautious print_tree ppf tree

end

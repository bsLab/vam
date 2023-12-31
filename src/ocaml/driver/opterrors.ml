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

(* $Id: opterrors.ml,v 1.14 2001/06/15 19:09:45 doligez Exp $ *)

(* WARNING: if you change something in this file, you must look at
   errors.ml to see if you need to make the same changes there.
*)

open Format

(* Report an error *)

let report_error ppf exn =
  let report ppf = function
  | Lexer.Error(err, start, stop) ->
      Location.print ppf {Location.loc_start = start;
                          Location.loc_end = stop;
                          Location.loc_ghost = false};
      Lexer.report_error ppf err
  | Syntaxerr.Error err ->
      Syntaxerr.report_error ppf err
  | Env.Error err ->
      Env.report_error ppf err
  | Ctype.Tags(l, l') -> fprintf ppf
      "In this program,@ variant constructors@ `%s and `%s@ \
       have the same hash value." l l'
  | Typecore.Error(loc, err) ->
      Location.print ppf loc; Typecore.report_error ppf err
  | Typetexp.Error(loc, err) ->
      Location.print ppf loc; Typetexp.report_error ppf err
  | Typedecl.Error(loc, err) ->
      Location.print ppf loc; Typedecl.report_error ppf err
  | Includemod.Error err ->
      Includemod.report_error ppf err
  | Typemod.Error(loc, err) ->
      Location.print ppf loc; Typemod.report_error ppf err
  | Translcore.Error(loc, err) ->
      Location.print ppf loc; Translcore.report_error ppf err
  | Compilenv.Error code ->
      Compilenv.report_error ppf code
  | Asmgen.Error code ->
      Asmgen.report_error ppf code
  | Asmlink.Error code ->
      Asmlink.report_error ppf code
  | Asmlibrarian.Error code ->
      Asmlibrarian.report_error ppf code
  | Sys_error msg ->
      fprintf ppf "I/O error: %s" msg
  | Typeclass.Error(loc, err) ->
      Location.print ppf loc; Typeclass.report_error ppf err
  | Translclass.Error(loc, err) ->
      Location.print ppf loc; Translclass.report_error ppf err
  | Warnings.Errors (n) ->
      fprintf ppf "@.Error: %d error-enabled warnings occurred." n
  | x -> fprintf ppf "@]"; raise x in

  fprintf ppf "@[%a@]@." report exn

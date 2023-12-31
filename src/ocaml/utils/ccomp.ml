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

(* $Id: ccomp.ml,v 1.11 2000/12/28 13:07:41 weis Exp $ *)

(* Compiling C files and building C libraries *)

let command cmdline =
  if !Clflags.verbose then begin
    prerr_string "+ ";
    prerr_string cmdline;
    prerr_newline()
  end;
  Sys.command cmdline

let run_command cmdline = ignore(command cmdline)

let quote = Filename.quote;;

let compile_file name =
  match Sys.os_type with
  | "MacOS" ->
     let qname = quote name in
     let includes = Config.standard_library :: !Clflags.include_dirs in
     let args =
       Printf.sprintf " %s %s -i %s"
         (String.concat " " (List.rev_map quote !Clflags.ccopts))
         (String.concat "," (List.rev_map quote includes))
         qname
     in
     run_command ("sc " ^ args ^ " -o " ^ qname ^ ".o");
     command ("mrc " ^ args ^ " -o " ^ qname ^ ".x")
  | _ ->
     command
       (Printf.sprintf
         "%s -c %s %s -I%s %s"
         !Clflags.c_compiler
         (String.concat " " (List.rev !Clflags.ccopts))
         (String.concat " "
           (List.rev_map (fun dir -> "-I" ^ dir) !Clflags.include_dirs))
         Config.standard_library
         name)
;;

let create_archive archive file_list =
  Misc.remove_file archive;
  match Config.system with
    "win32" ->
      command(Printf.sprintf "lib /nologo /debugtype:cv /out:%s %s"
                                 archive (String.concat " " file_list))
  | _ ->
      let r1 =
        command(Printf.sprintf "ar rc %s %s"
                                   archive (String.concat " " file_list)) in
      if r1 <> 0 || String.length Config.ranlib = 0
      then r1
      else command(Config.ranlib ^ " " ^ archive)

let expand_libname name =
  if String.length name < 2 || String.sub name 0 2 <> "-l"
  then name
  else begin
    let libname =
      "lib" ^ String.sub name 2 (String.length name - 2) ^ Config.ext_lib in
    try
      Misc.find_in_path !Config.load_path libname
    with Not_found ->
      libname
  end

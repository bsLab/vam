(*
**      ==================================
**      OOOO   OOOO OOOO  O      O   OOOO
**      O   O  O    O     O     O O  O   O
**      O   O  O    O     O     O O  O   O
**      OOOO   OOOO OOOO  O     OOO  OOOO
**      O   O     O    O  O    O   O O   O
**      O   O     O    O  O    O   O O   O
**      OOOO   OOOO OOOO  OOOO O   O OOOO
**      ================================== 
**      BSSLAB, Dr. Stefan Bosse sci@bsslab.de
**
**    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF: 
**    Free Software Foundation-Europe, GNU GPL License, Version 2
**
**    $MODIFIEDBY:  BSSLAB
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2003 BSSLAB
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.05
**
** Module [Vamtoploop]: Replacement for the standard toplevel module
** The old interpreter is still used.
**
**    $INFO:
**
**
*)


(*
** Additional include paths: ocaml-XX/parsing, ocaml-XX/driver 
*)

open StdLabels

open Path
open Toploop
open String
open Lexing
open Format
open Config
open Errors
open Location

external print_backtrace_mode : int -> unit = "print_backtrace_mode"
external terminal_readline: string -> string = "terminal_readline"

let vam_version = "2.0 (Build date "^(__DATE__)^")"

let got_eof = ref false
let first_line = ref true

(*
** Discard everything already in a lexer buffer 
*)

let vam_empty_lexbuf lb =
  let l = String.length lb.lex_buffer in
  lb.lex_abs_pos <- (-l);
  lb.lex_curr_pos <- l


(*
** Get the stuff from the user
*)


let vam_refill_lexbuf buffer len =
  if !got_eof then (got_eof := false; 0) else begin
    let prompt = 
      if !first_line then "[] "
      else "   "
    in

    (*
    output_string stdout prompt; flush stdout;
    *)

    first_line := false;
    let i = ref 0 in
    try
      let tempbuf = terminal_readline (prompt) in 
      while true do
        if !i >= len then raise Exit;
        (* let c = input_char stdin in *)
        let c = String.get tempbuf !i in 
        buffer.[!i] <- c;
        incr i;
        if c = '\n' then raise Exit; 
        if !i = (String.length tempbuf) then
        begin
            buffer.[!i] <- '\n';
            incr i;
            raise Exit;
        end
      done;
      !i
    with
    | Exit -> !i
  end

(*
** Use the standard formatter for output
*)

let ppf = Format.std_formatter

exception PPerror


(*
** Main loop
*)

let vam_loop () = 
    (*
    ** Who are we ? [VAM,VUM,XVAM,PSILAB]
    *)
    
    let my = if ((Str.string_match 
                    (Str.regexp ".*xvam.byte")
                    (Sys.argv.(0)) 0) = true) 
             then "XVAM" 
             else if ((Str.string_match 
                    (Str.regexp ".*vam.byte")
                    (Sys.argv.(0)) 0) = true) 
             then "VAM"
             else if ((Str.string_match 
                    (Str.regexp ".*vum.byte")
                    (Sys.argv.(0)) 0) = true) 
             then "VUM"
             else if ((Str.string_match 
                    (Str.regexp ".*psilab.byte")
                    (Sys.argv.(0)) 0) = true) 
             then "PSILAB"
             else
                failwith "Unknown system"
    in    
               
 
    (* Welcome message *)

    fprintf ppf "                   ===========@.";
    fprintf ppf "                   %s system @." my;

    fprintf ppf "                   ===========@.@.";
    fprintf ppf "                  [Version %s]@.@." vam_version;
    fprintf ppf "            Written by Stefan Bosse (sci@@bsslab.de)@.";
    fprintf ppf "            (c) 2003-2005 by BSSLAB@.@.";

    fprintf ppf "Use 'help \"help\"' or 'help \"intro\"' to get more informations.@.@.";    


    (*
    ** Add whatever -I options have been specified on the command line,
    ** but keep the directories that user code linked in with ocamlmktop
    ** may have added to load_path. 
    *)

    fprintf ppf "Loading initial environment...";print_newline ();
     
    load_path := "" :: (List.rev !Clflags.include_dirs @ !load_path);

    (* Load Pervasives module *)
    toplevel_env := Compile.initial_env();

    (*
    ** Init toplevel system: open all modules ... 
    *)

    let initfile =
        match my with
        | "VAM" -> "vam_init.ml" ;
        | "VUM" -> "vum_init.ml" ;
        | "XVAM" -> "xvam_init.ml";
        | "PSILAB" -> "psilab_init.ml";
        | _ -> failwith "Unknown system";
    in

    if (use_silently ppf initfile) = false then
        failwith (" loading "^initfile^" failed.\n");

    fprintf ppf "Ready.@.@.";

    let lb = Lexing.from_function vam_refill_lexbuf in
    Location.input_name := "";
    Location.input_lexbuf := Some lb;
    while true do
        try
            vam_empty_lexbuf lb;
            Location.reset();
            first_line := true;
            let phr =  try !parse_toplevel_phrase lb with Exit 
                    -> raise PPerror in
            ignore(execute_phrase true ppf phr);
        with
        | End_of_file -> raise Exit
        | Sys.Break -> fprintf ppf "Interrupted.@."
        | PPerror -> ()
        | x -> Errors.report_error ppf x
    done


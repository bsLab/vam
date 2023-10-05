(*
** A small portable version of GNU's readline with history support.
** It doesen't need any special terminal capabilities except backspace.
*)



(*
** EOF char
*)

val eof_char: char ref 

(*
** Main function:
**
** readline "prompt"
**
** Editing the line with arrow keys, backspace and the Nano Emacs
** key bindings.
*)

val readline: string -> string

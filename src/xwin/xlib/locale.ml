(*
** Special local national support
*)

open KeyBind

(*
** German keyboard extension. Remove if not needed or change to your
** local keyboard symbol extension. See keyBind.ml for details.
*)
let _ = locale_keysyms := !locale_keysyms @ germanSyms 


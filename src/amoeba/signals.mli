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
**    $AUTHORS:     
**    $INITIAL:     (C)
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.02
**
**    $INFO:
**
**  Amoeba signals
**
**    $ENDOFINFO
**
*)


(*
** Light-weighted signals
*)
val sig_TRANS : int
val sig_INT : int

external am_sig_catch : int ->  int
    = "am_sig_catch"
external am_sig_await : int -> int
    = "am_sig_await"


val sig_catch : signum:int -> handler:(int -> unit) -> unit


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
**    $VERSION:     1.03
**
**    $INFO:
**
**  Amoeba thread signals
**
**    $ENDOFINFO
**
*)
open Amoeba
open Thread


(*
** Light-weighted signals
*)
let sig_TRANS = 1
let sig_INT = 3


external am_sig_catch : int ->  int
    = "am_sig_catch"
external am_sig_await : int -> int
    = "am_sig_await"


let sig_handler signum stnum handler =
    while(true)
    do
        let _ = am_sig_await stnum in
#ifdef DEBUG
        Db.Pr.sdd 1 "sig_handler, signum, stnum" signum stnum;
#endif
        handler signum;
    done

(*
** Install signal handler for ONE thread.
*)

let sig_catch ~signum ~handler =
    let stnum = am_sig_catch signum in
    ignore(thread_create 
                (fun () -> sig_handler signum stnum handler) ());



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
**    $INITIAL:     (C) INRIA
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.01
**
**    $INFO:
**
**                                                                     
**                           xlib for Ocaml                            
**                                                                     
**       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       
**                                                                     
**  Copyright 1998 Institut National de Recherche en Informatique et   
**  Automatique.  Distributed only by permission.                      
**
** 
**  Modified and restructured by 
**
**        Stefan Bosse
**        sbosse@physik.uni-bremen.de
**
**  Last modified:	18/10/01
**
**  Changes:	-- more comments (in english)
**              -- pthreads support 
**              -- Thread support changed
**
**
**  $ENDINFO
**
*)
                                                                     

open Thread
open Concur

type 'a value =
    NoValue
  | Value of 'a
  | Exc of exn

type 'a t =
    {
      mutex: Mutex.t;
      condition: Thread.thread_event;
      mutable contenu: 'a value
    }

let create () =
  { mutex = Thread.mu_create ();
    condition = Thread.thread_create_event ();
    contenu = NoValue
  }

let signal_value event v =
  Thread.mu_lock event.mutex;
  event.contenu <- Value v;
  Thread.mu_unlock event.mutex;
  Thread.thread_wakeup event.condition


let signal_exception event e =
  Thread.mu_lock event.mutex;
  event.contenu <- Exc e;
  Thread.mu_unlock event.mutex;
  Thread.thread_wakeup event.condition

let wait event =
  Thread.mu_lock event.mutex;
  (*  Log.printf "<J>%s" ""; *)
  while event.contenu = NoValue do
    Thread.mu_unlock event.mutex;
    Thread.await event.condition;
    Thread.mu_lock event.mutex;
  done;

  let contenu = event.contenu  in
  (*  Log.printf "</J>%s" ""; *)
  event.contenu <- NoValue;
  Thread.mu_unlock event.mutex;

  match contenu with
    NoValue -> assert false
  | Value v -> 
      v
  | Exc e -> raise e
      
let check event =  event.contenu = NoValue 

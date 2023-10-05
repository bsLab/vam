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
**    $INITIAL:     (C) 2003 BSSLAB
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.04
**
**    $INFO:
**
** Portable thread module
** with mutex and semaphore support and thread synchronisation.
**
**
**
**
** Uses the native Amoeba C thread interface:
**
**      thread_init ()
** int  thread_newthread(func,stsize,param,size)
** int  thread_exit()
** int  thread_id ()
** int  thread_await(ev,to)
** int  thread_await_lock(ev,to,mulock)
** void thread_wakeup(ev)
** void thread_switch()
** int  thread_delay()
** void mu_lock()
** int  mu_trylock()
** void mu_unlock()
**    $ENDOFINFO
**
*)

open Mutex

(*
** Thread synchronisation event handler 
*)
type thread_event

(*
** Create a new thread
*)

val thread_create:     func:('a -> 'b) -> arg:'a -> int

(*
** A thread terminates...
*)

val thread_exit:       unit -> unit

(*
** Return unique integer thread id number
*)

val thread_id:         unit -> int

(*
** Give other threads a chance to run
*)

val thread_switch:     unit -> unit 

(*
** Thread synchronisation: a new thread event handler
*)

external thread_create_event :
    unit -> thread_event 
    = "caml_event_new" 


(*
** A thread wants to wait for an event
*)

external thread_await:
    thread_event -> int -> bool
    = "caml_thread_await"

(*
** A thread wants to wait for an event. Additionaly, a protection
** mutex must be locked by the client thread (and therefore protect his
** critical region) before calling this function. The await_lock
** function will unlock the mutex than.
*)
  
external thread_await_lock:
    thread_event -> int -> Mutex.t -> bool
    = "caml_thread_await_lock"


(*
** Raise event ev and wakeup the next thread waiting for this event.
** If no thread is already waiting, increment pending variable, so the
** wakeup is not lost.
*)

external thread_wakeup:
    thread_event -> unit
    = "caml_event_wakeup"

val await : thread_event -> unit 



(*
** Delay a thread for ## * time_unit secs
*)

type time_unit = 
    | SEC
    | MILLISEC
    | MICROSEC

val thread_delay: int -> time_unit -> bool

(*
** Delay for ## sec/millisec/microsec
*)

val delay : int -> unit
val thread_sdelay: int -> unit

val thread_mdelay: int -> unit
val thread_udelay: int -> unit



external caml_thread_unit_delay:
    int -> int -> bool
    = "caml_thread_unit_delay"


(*
** Mutual exclusion support
*)

val mu_create:     unit -> Mutex.t
val mu_lock:       Mutex.t -> unit
val mu_trylock:    Mutex.t -> bool
val mu_unlock:     Mutex.t -> unit


(*
** Debugging support
*)

external thread_debuginfo:
    bool -> string
    = "caml_thread_debuginfo"

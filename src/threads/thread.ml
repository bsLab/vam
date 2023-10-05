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
**    $VERSION:     1.03
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


(*
** Internal stuff, directly interfacing to amoeba_thr.c
*)

type t

external thread_initialize : unit -> unit = "caml_thread_initialize"
external thread_new : (unit -> unit) -> t = "caml_thread_new"
external caml_thread_unit_delay: (int -> int -> bool) = 
            "caml_thread_unit_delay" 

external yield : unit -> unit = "caml_thread_yield"
external self : unit -> t = "caml_thread_self"
external id : t -> int = "caml_thread_id"
external join : t -> unit = "caml_thread_join"


external print_backtrace: unit -> unit = "print_backtrace" 
external print_backtrace_mode: int -> unit = "print_backtrace_mode" 

type time_unit = 
    | SEC
    | MILLISEC
    | MICROSEC


(* 
** For new, make sure the function passed to thread_new never
** raises an exception. 
*)

exception Thread_exit

let create fn arg =

  print_backtrace_mode 1;

  thread_new
    (fun () ->
      try
        fn arg; ()
      with Thread_exit -> ()
         | exn ->
             (*
             ** Print the backtrace un an uncaught exception for this
             ** thread.
             *)

             print_backtrace ();

             Printf.eprintf "Uncaught exception in thread %d: %s\n"
                            (id(self())) (Printexc.to_string exn);
             flush stderr
    )

let exit () = raise Thread_exit

(*
** Thread.kill is currently not implemented due to problems with
** cleanup handlers on several platforms 
*)

let kill th = invalid_arg "Thread.kill: not implemented"

(*
** Preemption 
*)

let preempt signal = yield()

(*
** Initialization of the scheduler and the underlying amoeba thread package.
*)

let _ =
  (*ignore(Sys.signal Sys.sigvtalrm (Sys.Signal_handle preempt));*)
  thread_initialize()

(*
** Wait functions 
*)

let thread_delay time uni =
    match uni with
    | SEC -> caml_thread_unit_delay time 1000000;
    | MILLISEC -> caml_thread_unit_delay time 1000;
    | MICROSEC -> caml_thread_unit_delay time 1

let delay time = 
    ignore(thread_delay time SEC)





(*************************************************************)
(************** The public user interface ********************) 
(*************************************************************)

(*
** Event type used by await/wakeup
*)

type thread_event


(*
** Create a new thread
*)

let thread_create ~func:func ~arg:arg  =
        let thrnew =  (create func arg) in
        let id = id thrnew in
        yield ();
        id
(*
** A thread terminates...
*)

let thread_exit    =
        exit

(*
** Integer thread id number
*)

let thread_id  ()  =
        let thrself = self () in
        id thrself

(*
** Give other threads a chance to run
*)

let thread_switch  =
        yield 


(*
** Delay the thread for ## secs
*)

let thread_sdelay del =
        ignore(thread_delay del SEC)

(*
** Delay the thread for ## milli secs
*)

let thread_mdelay del =
        ignore(thread_delay del MILLISEC)

(*
** Delay the thread for ## micro secs
*)

let thread_udelay del =
        ignore(thread_delay del MICROSEC)


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

let await ev =
    ignore(thread_await ev 0)

    
(*
** A thread wants to wait for an event. Additionaly, a protection
** mutex must be locked by the client thread (and therefore protect his
** critical region) before calling this function. The await_lock
** function will unlock the mutex then.
*)

external thread_await_lock :
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

(*
** Create a new locking mutex
*)

let mu_create =
        Mutex.create

(*
** Lock a mutex
*)

let mu_lock   =
        Mutex.lock


(*
** N.I.
*)

let mu_trylock =
        Mutex.try_lock

(*
** Unlock a locked mutex
*)

let mu_unlock   =
        Mutex.unlock



(*
** Debugging support
** A true argument enables full stack tracing.
*)

external thread_debuginfo: 
    bool -> string
    = "caml_thread_debuginfo" 

(*
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
**  Last modified: 03/05/02
**
**  Changes:	-- more comments (in english)
**              -- Pthreads support
**              -- Thread support changed
**              -- X connection shutdown now sync'ed
**
*)
                                                                     

(*
** Xlib thread synchronisation
*)

val xlib_wait : Thread.thread_event
val xlib_mutex : Mutex.t
val shutdown_done: Thread.thread_event

val display_open: int ref

(*
** Xlib message and event thread management
*)

val new_listener : (unit -> unit) -> Xtypes.display -> unit
val add_timer : float -> (unit -> unit) -> unit
val iterator : int ref -> unit
val poll : unit -> bool




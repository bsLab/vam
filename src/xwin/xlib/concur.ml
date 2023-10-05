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
**    $AUTHORS:     Fabrice Le Fessant, Stefan Bosse
**    $INITIAL:     (C) 1998 INRIA
**    $CREATED:     ?
**    $VERSION:     1.04
**
**    $INFO:
**
**
**
**    $ENDOFINFO
**
*)




open Thread
open Xtypes
open Os
open Xos

open Unix

(* must be os independent ! *)
let delay = Unix.sleep

let actions = ref 0
let mu_actions = Thread.mu_create ()
let cond_actions = Thread.thread_create_event ()
let shutdown_done = Thread.thread_create_event ()

(*
** Record all opened displays
*)

let display_open = ref 0


(*
** Thread synchronisation:
**
** 1. Global X event handler used between X message listener and handler
** 2. Generic Xlib mutex lock to protect critical sections
**
*)

let xlib_wait = Thread.thread_create_event ()
let xlib_mutex = Thread.mu_create ()

open Printf
open Ll_trans

(*
** Start a new listener thread
*)

let new_listener f (dsp:display) =


        let tid = Thread.thread_create 
            (fun display ->
                while true do
                    
                    (*
                    ** Because the wakeup is not guaranteed if there
                    ** is no thread awaiting, use actions to avoid
                    ** loosing events. See iterator.
                    *)

                    Thread.mu_lock mu_actions;
                    incr actions;
                    Thread.mu_unlock mu_actions;

                    (*
                    ** Send an wakeup event
                    *)

                    Thread.thread_wakeup cond_actions;
                    
                    if (display.dpy_conn_status = Connection_closed ) then
                    begin

                        (*
                        ** Wakeup closeDisplay which is waiting for us.
                        *)
                        Thread.thread_wakeup shutdown_done;

                        (*
                        ** Shutdown the X server connection
                        *)

                        Ll_trans.closeConnection display.comm_channel;
                        Thread.thread_exit ();
                    end;



                    (try

                        (*
                        ** Call the reader function
                        *)

                        f ()

                    with 
                        | Exit ->
                            (* Connection closed ! *)

                            Thread.mu_lock mu_actions;
                            incr actions;
                            Thread.mu_unlock mu_actions;
                            Thread.thread_wakeup cond_actions;    

                            Thread.thread_exit ();
                        | Unix.Unix_error (err,s1,s2) ->
                        begin
                            match err with
                            | EAGAIN -> 
                                (* Failure ? 
                                print_string "X listener thread: EAGAIN ";
                                print_string (s1^" "^s2);
                                print_newline ();
                                *)
                                delay 1;
                            | _ ->
                            begin
                                (* Failure *)
                                print_string "X listener thread: ";
                                print_string (s1^" "^s2);
                                print_newline ();

                                Thread.mu_lock mu_actions;
                                incr actions;
                                Thread.mu_unlock mu_actions;
                                Thread.thread_wakeup cond_actions;

                                Thread.thread_exit ();

                            end;
                        end;
                        | e -> 
                        begin
                                (* Failure *)
                                print_string "X listener thread: ";
                                print_string (Printexc.to_string e);
                                print_newline ();

                                Thread.mu_lock mu_actions;
                                incr actions;
                                Thread.mu_unlock mu_actions;
                                Thread.thread_wakeup cond_actions;

                                Thread.thread_exit ();
                        end;
                    );
                done
            ) dsp in

        (*
        ** Activate the new listener thread
        *)
        Thread.thread_switch ();
        ()

(*
** Add a new timer job - simple
*)

let add_timer time f =
          let _ = Thread.thread_create 
                    (fun _ -> 
                      Thread.thread_mdelay 
                         (int_of_float (time *. 1000.0)); 
                     f ()) () 
          in ()

(*
** Wait for events pushed by the listener threads. Called from
** eloop functions.
*)      

let iterator lst_it =

        Thread.mu_lock mu_actions;

        (*
        ** Exit the event loop if the last display was closed...
        *)
        if (!display_open = 0) then
        begin
            Thread.mu_unlock mu_actions;
            Thread.mu_unlock xlib_mutex;
            raise Exit;
        end;

        (*
        ** Wait for X events (incoming messages)
        *)


        (*
        ** Because the wakeup is not guaranteed if there
        ** is no thread awaiting, use actions to avoid
        ** loosing events.
        *)


        while !actions = !lst_it do
            Thread.mu_unlock mu_actions;
            Thread.await cond_actions;
            Thread.mu_lock mu_actions;
        done;

        lst_it := !actions;
        Thread.mu_unlock mu_actions

    
      
(* now we don't poll *)
let poll () = false


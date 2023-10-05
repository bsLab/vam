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
**  Last modified:  27/02/02
**
**  Changes:	-- more comments (in english)
**              -- pthreads support
**              -- Thread support changed
**
*)

(*
** Event loop module
*)                                                                     


open Thread
open Concur
open Xtypes

let debug = ref false

(*
** Event module locking
*)

let mutex = Thread.mu_create ()
 
(*
** Event loop display handler
*)
 
type display = 
{
    mutable dis_rejected : bool;
    dis_display : Xtypes.display;
    dis_handlers : (xevent -> unit) Wintbl.t;
    dis_defhandler : (xevent -> unit);
    mutable dis_last_event : event;
    mutable dis_last_time : time;
}

let exit_eloop = ref false

(*
** All managed displays
*)
let displays = ref []
  
let after_event_hooks = ref []
let after_events_hooks = ref []
  
(*
** Add event hooks performed after an event
*)

let add_after_event_hook f = after_event_hooks := f:: !after_event_hooks
let add_after_events_hook f = after_events_hooks := f:: !after_events_hooks


(*
** Timer events
*)
  
let event_time = ref currentTime
let last_event = ref (KeymapNotifyEvent { Xkeymap.keys = "" })
let button = ref 0
  
let update_event_time ev =
  last_event := ev.ev_event;
  try
    let time = 
      match ev.ev_event with
      | KeyPressEvent e         ->  e.Xkey.time
      | KeyReleaseEvent e       ->  e.Xkey.time
      | ButtonPressEvent e      ->  e.Xbutton.time
      | ButtonReleaseEvent e    ->  e.Xbutton.time
      | MotionNotifyEvent e     ->  e.Xmotion.time
      | EnterNotifyEvent e      ->  e.Xcrossing.time
      | LeaveNotifyEvent e      ->  e.Xcrossing.time
      | PropertyNotifyEvent e   ->  e.Xproperty.time
      | SelectionClearEvent e   ->  e.Xselectionclear.time
      | SelectionRequestEvent e ->  e.Xselectionrequest.time
      | SelectionNotifyEvent e  ->  e.Xselection.time
      | _ -> raise Not_found
    in
    event_time := time;
  with
    _ -> ()
  
let update_time dis ev =
  update_event_time ev;
  dis.dis_last_event <- ev.ev_event;
  dis.dis_last_time <- !event_time

(*
** Handle one event
*)

let handle_event drepr ev =
  let handler = 
    try
      Wintbl.find drepr.dis_handlers ev.ev_window
    with
      Not_found ->
        if !debug then
          Printf.printf "Eloop.default for %d\n" 
            (window_to_id ev.ev_window);
        drepr.dis_defhandler
  in
  update_time drepr ev;

  (* Call the event handler function *)
  (try handler ev with _ -> ());

  (* and iterate the list of all after event hook handlers *)
  List.iter (fun f -> try f () with _ -> ()) !after_event_hooks
  
let lst_it = ref 0

let rec one_display no list =
  match list with
  | [] -> no

  | drepr :: tail ->
      if (drepr.dis_rejected ||
          Xlib.nextEventWait drepr.dis_display) then 
        one_display (no+1) tail
      else 
      begin
          (try
              let ev = Xlib.nextEvent drepr.dis_display in
              Mutex.lock mutex;
              handle_event drepr ev;
              Mutex.unlock mutex;
            with
              e -> Mutex.unlock mutex
          );
          one_display no tail
      end
        
let rec one_loop num =
  let len = List.length !displays in
  let no = one_display 0 !displays in
  if no = len then
    ( Mutex.lock mutex;
      List.iter (fun f -> try f () with _ -> ()) !after_events_hooks;
      Mutex.unlock mutex;
      num = 0)
  else
    one_loop (num+1)

(*
** Handle all events
*)
    
let handle_events wait = 
  if wait then Concur.iterator lst_it;
  one_loop 0

(*
** Main event loop
*)

let rec event_loop () =
  let _ = handle_events false in
  Concur.iterator lst_it;
  event_loop ()
      
let exit () = exit_eloop := true

(*
** Add a new display to our event handler list
*)
  
let add_display dpy def_handler = 
  let rec iter = function 
      [] -> 
        let dis = {
            dis_rejected = false;
            dis_display = dpy;
            dis_handlers = Wintbl.create 117;
            dis_defhandler = def_handler;
            dis_last_time = currentTime;
            dis_last_event = (KeymapNotifyEvent { Xkeymap.keys = "" });
          } in
        displays := dis :: !displays;
        dis
    | dis :: tail ->
        if dis.dis_display == dpy then dis else iter tail
  in
  iter !displays

  
let list_removeq list ele =
  List.fold_left (fun list e ->
                    if e == ele then list
                    else e ::  list) [] list

(*
** Remove a display from our list
*)
  
let remove_display dis =
  displays := list_removeq !displays dis
  
(*
** Return the Xtypes display 
*)

let display dis = dis.dis_display

(*
** Add a window to our window table
*)
  
let add_window dis win handler =
  if dis.dis_rejected then failwith "Event_loop.add_window: display closed";
  if !debug then Printf.printf "Eloop.add_window %d\n" (window_to_id win);

  try
    let old_handler = Wintbl.find dis.dis_handlers win in
    Wintbl.add dis.dis_handlers win 
        (fun ev ->
            (try old_handler ev with _ -> ());
            (try handler ev with _ -> ())  
        )
  with _ -> 
  Wintbl.add dis.dis_handlers win handler

(*
** Remove a window 
*)
  
let remove_window dis win = 
  if !debug then Printf.printf "Eloop.remove_window %d\n" (window_to_id win);
  Wintbl.remove dis.dis_handlers win

(*
** Do we know about this window ?
*)
  
let known_window dis win =
  try
    let _ = Wintbl.find dis.dis_handlers win in
    true
  with
    _ -> false

(*
** Add a timer function; called one time after time is expired 
*)

let add_timer dis time f = Concur.add_timer time f
  
let last_time dis = dis.dis_last_time
let last_event dis = dis.dis_last_event


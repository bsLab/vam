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
**      BSSLAB, Dr. Stefan Bosse www.bsslab.de
**
**    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF: 
**    Free Software Foundation-Europe, GNU GPL License, Version 2
**
**    $MODIFIEDBY:  BSSLAB
**    $AUTHORS:     Fabrice Le Fessant, 
**                  Stefan Bosse
**    $INITIAL:     Derived from wXlib / INRIA, 2005 BSSLAB
**    $CREATED:     10.5.2005
**    $VERSION:     1.03
**
**    $INFO:
**
**  VXlib basic widget.
**
**    $ENDOFINFO
**
*)


open Xtypes
open VX_types
open VX_common
open VX_screen

let exit_exn = Exit
  
class virtual t window screen =
    object (self)
    inherit VX_screen.t screen 
  
    val w = window
  
    method actions = w.w_actions
    
    method background = w.w_background
    method foreground = w.w_foreground
  
    method window = 
        assert (not (w.w_window == noWindow));
        w.w_window

    method win =
        w
  
    (*
    ** Handle keyboard events
    *)
    method handle_key () =
        try
            let modifiers = !modifiers_event in
            let modmask = lnot modifiers in
            let key = !key_sym in
            List.iter (fun (binding, action ) ->
                match binding with
                | Key(k,m) ->
                    if k = anyKey && m = anyModifier then
                        (action (); raise exit_exn) else
                    if k = anyKey && m land modifiers = m then
                        (action (); raise exit_exn) else
                    if k = key && m land modifiers = m then
                        (action (); raise exit_exn) else
                    if k = key && m = anyModifier then
                        (action (); raise exit_exn)
                | GrabbedKey(k,m) ->
                    if k = anyKey && m = anyModifier then
                        (action (); raise exit_exn) else
                    if k = anyKey && m land modifiers = m then
                        (action (); raise exit_exn) else
                    if k = key && m land modifiers = m then
                        (action (); raise exit_exn) else
                    if k = key && m = anyModifier then
                        (action (); raise exit_exn)
                | _ -> ()
            ) w.w_actions
        with
            Exit -> ()
  
    (*
    ** Handle mouse button events
    *) 
    method handle_button () =
        try
            let button = !button_event in
            let modifiers = !modifiers_event in      
            let modmask = lnot modifiers in
            List.iter (fun binding ->
                match binding with
                | Button(b, m), action ->
                    if b = anyButton && m = anyModifier then
                        (action (); raise exit_exn) else
                    if b = anyButton && m land modifiers = m then
                        (action (); raise exit_exn) else
                    if b = button && m land modifiers = m then
                        (action (); raise exit_exn) else
                    if b = button && m = anyModifier then
                        (action (); raise exit_exn) 
                | GrabbedButton(b, m), action ->
                    if b = anyButton && m = anyModifier then
                        (action (); raise exit_exn) else
                    if b = anyButton && m land modifiers = m then
                        (action (); raise exit_exn) else
                    if b = button && m land modifiers = m then
                        (action (); raise exit_exn) else
                    if b = button && m = anyModifier then
                        (action (); raise exit_exn) 
                | _ -> ()
            ) w.w_actions;
        with
            Exit ->  ()

    (*
    ** Handle all X-Windows events
    *)  
    method xevents ev =
        begin
            match ev.ev_event with
            | EnterNotifyEvent e when not_grab_related e.Xcrossing.mode ->
                w.w_enter_window ()
            | LeaveNotifyEvent e when not_grab_related e.Xcrossing.mode ->
                w.w_leave_window ()
            | ButtonPressEvent e -> 
                mouse_x_event := e.Xbutton.x_event;
                mouse_y_event := e.Xbutton.y_event;
                button_event := e.Xbutton.detail; 
                modifiers_event := e.Xbutton.state;
                w.w_button_press ();
                last_click := e;
            | KeyPressEvent e ->           
                let modifiers = e.Xkey.state in
                let s = self#screen in
                let (s,keysym,_) = KeyBind.lookupString s.s_display 
                                                        ev.ev_event in
                key_string := s;
                key_sym := keysym;
                modifiers_event := e.Xkey.state;
                w.w_key_press ();
            | KeyReleaseEvent e ->           
                let modifiers = e.Xkey.state in
                let s = self#screen in
                let (s,keysym,_) = KeyBind.lookupString s.s_display 
                                                        ev.ev_event in
                key_string := s;
                key_sym := keysym;
                modifiers_event := e.Xkey.state;
                w.w_key_release ();
            | ExposeEvent e -> 
                w.w_refresh_timestamp <- -1;
                self#wait_refresh false 
                                  e.Xexpose.x 
                                  e.Xexpose.y 
                                  e.Xexpose.width 
                                  e.Xexpose.height;
            | ButtonReleaseEvent e -> 
                mouse_x_event := e.Xbutton.x_event;
                mouse_y_event := e.Xbutton.y_event;          
                button_event := e.Xbutton.detail;           
                w.w_button_released ();
            | MotionNotifyEvent e ->
                mouse_x_event := e.Xmotion.x_event;
                mouse_y_event := e.Xmotion.y_event;
                w.w_button_motion ();
                w.w_pointer_motion ();
            | FocusInEvent e -> 
                w.w_focus_in ()
            | FocusOutEvent e -> 
                w.w_focus_out ()
            | _ -> ()
        end;
        (* self#update *)
    
    method update_top_size = ()
    method width = let g = w.w_geometry in g.width + 2 * g.border 
    method height = let g = w.w_geometry in g.height + 2 * g.border 
  
    method root_coordinates =
        let me = X.translateCoordinates s.s_display 
                                        w.w_window 
                                        s.s_screen.scr_root 0 0 in
        me.me_x, me.me_y
    
    val mutable myname = "base"
    method name = myname
    method set_name newname = myname <- newname

    method container = (self :> container)
end          

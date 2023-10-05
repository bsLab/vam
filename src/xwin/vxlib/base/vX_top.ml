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
**    $AUTHORS:     Fabrice Le Fessant, 
**                  Stefan Bosse
**    $INITIAL:     Derived from wXlib / INRIA, 2005 BSSLAB
**    $CREATED:     12.5.2005
**    $VERSION:     1.04
**
**    $INFO:
**
**  VXlib top level widget.
**
**    $ENDOFINFO
**
*)

open Xtypes
open VX_types
open VX_common

class orig (root : VX_root.t) pos attributes =
  
    object (self)
    inherit VX_object.t root#container
        (match pos with
         | None -> attributes; 
         | Some (x,y) -> (Position (x, y)):: attributes) 
        as super
  
    val mutable resized = false
  
    initializer 
        match pos with
        | None -> 
            w.w_mask <- StructureNotifyMask :: w.w_mask;
            w.w_override <- false
        | Some (x,y) ->
            w.w_override <- true;
  
    val mutable widget = None
  
    method top = self#container
    method container_add (wob : contained) = 
        wob#set_parent self#container;
        if w.w_shown then
        begin
            (match widget with 
             | None -> (); 
             | Some widget -> widget#hide);
            wob#show;
            widget <- Some wob;
            self#wait_resize;
        end
        else
            widget <- Some wob;
  
    method update_top_size = self#update_size
  
    method update_size =
        (*
        ** FIRST, compute the size requested by the widget 
        *)
        let g = w.w_geometry in

        let width, height = 
            match widget with
            | None -> 1,1
            | Some widget -> 
                let sz = widget#size_request in
                let width = (min  sz.max_width
                                (max sz.requested_width sz.min_width))
                                    + 2 * (w.w_ipad_x) in
                let height = (min sz.max_height
                                (max sz.requested_height sz.min_height))
                                    + 2 * (w.w_ipad_y) in
            width, height
            in
        (*
        ** SECOND, conform to own size hints 
        *)
        let tsz = szhints in
        let width = min tsz.max_width (max tsz.min_width width) in
        let height = min tsz.max_height (max tsz.min_height height) in
    
        (*
        ** THIRD, conform to screen size and out current window
        ** size (user resized window ?)!!!  
        *)
        let width = max g.width (min width s.s_screen.scr_width) in
        let height = max g.height (min height s.s_screen.scr_height) in
    
        (*
        ** FIFTH, allocate the new size 
        *)
        __(self#size_allocate 0 0 width height);
  
    method size_allocate x y dx dy =
        (*
        ** do not use super#size_allocate to avoid moving the window. 
        *)
        let accepted = ref true in
    
        let g = w.w_geometry in
        let modified = w.w_size_modified || 
                       not (g.width = dx && g.height = dy) in
        w.w_size_modified <- false;
        if modified then
        begin
            let wg = w.w_geometry in
            let s = self#screen in
            wg.width <- dx;
            wg.height <- dy;
            if not (w.w_window == noWindow) then
                if not resized then
                    Xlib.resizeWindow s.s_display w.w_window dx dy
                else
                    resized <- false;
            (*
            ** Allocate the widget size 
            *)
            match widget with 
            | None -> ();
            | Some widget ->
                let sz = widget#size_request in
                let ipx = w.w_ipad_x in
                let ipy = w.w_ipad_y in
                accepted := widget#size_allocate ipx ipy (dx-2*ipx) (dy-2*ipy)
        end;  
        !accepted

    method realize =
        super#realize;
        match widget with 
        | None -> (); 
        | Some wob -> wob#realize
  
    method destroy =
        (match widget with 
         | None -> (); 
         | Some wob -> wob#destroy);
        super#destroy;
  
    method show =
        if not w.w_shown then
        begin
            if w.w_window == noWindow then 
            begin
                self#update_top_size;
                self#realize;
            end;
            (match widget with 
             | None -> (); 
             | Some wob -> wob#show);
            X.mapWindow s.s_display w.w_window;
            w.w_shown <- true;
        end;
        Xlib.raiseWindow s.s_display w.w_window
  
    method change_position x y  = __(self#configure [Position (x,y)])
  
    method name = "top"
  
    method grab_pointer = 
        X.grabPointer s.s_display w.w_window true [ButtonReleaseMask]
                      GrabModeAsync GrabModeAsync noConfineTo 
                      Xtypes.noCursor currentTime;
  
    method xevents ev =
        (match ev.ev_event with
         | ConfigureNotifyEvent e when not ev.ev_sent -> 
            (*
            ** Here, we must be very careful. Several resize commands may have
            ** been issued, and we must not change the size unless it is a 
            ** user choice. 
            *)

            (*
            ** let g = w.w_geometry in
            ** g.x <- -1;
            ** g.width <- e.Xconfigure.width;
            ** g.height <- e.Xconfigure.height;
            ** self#wait_resize
            *)
            let g = w.w_geometry in
            if g.width <> e.Xconfigure.width ||
               g.height <> e.Xconfigure.height then
            begin
                resized <- true;
                __(self#size_allocate 0 0 e.Xconfigure.width 
                                          e.Xconfigure.height);
            end;
      
         | _ -> ());
        super#xevents ev
  
    method wait_resize =
        if not w.w_size_modified then
        begin
            w.w_size_modified <- true;
            s.s_wait_resize <- self#to_resize :: s.s_wait_resize
        end
end

class t = orig
  
class outtop root window_id attributes = object
    inherit orig root None attributes
    
    method realize = ()
end
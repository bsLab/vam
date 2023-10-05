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
**    $INITIAL:     (C) INRIA
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.03
**
**    $INFO:
**
**    $ENDOFINFO
**
*)

(*
** Changes [@SB]:
**     -- Default attributes changed 
**     -- Mouse button action changed
**          Mouse button 1 pressed above or left from 
**          scrollbar mark: adjust page down
**          Below or right from scrollbar mark: adjust page up
**          Mouse button 1 held down in scrollbar mark: motion
**          adjustment.
**
*)

open Xtypes
open WX_types

class t parent sens (adj : WX_adjust.t) attributes =
  
  object (self)
  inherit WX_object.t parent 
    ((match sens with
        Horizontal ->  ExpandX  true;
      | Vertical->     ExpandY true) ::
    [ 
      MinWidth 18; MinHeight 18; 
      Relief ReliefRaised; 
      Foreground "gray60"; Background "gray80" 
    ] @ attributes) as super
  
  val mutable subwin = noWindow
  
  val mutable button = None
  method timer t b f =
    Eloop.add_timer s.s_eloop t (fun _ ->
        match button with
          Some button when b = button -> 
            self#timer !autorepeat_rate b f; f ();
        | _ -> ()
    )
  
  val mutable activated = false
  initializer 
    adj#add_subject self#update_adjustement;
    self#configure [Bindings [
        Button (1,0), (fun _ -> 
            let adj = adj in
            let g = w.w_geometry in

            button <- Some 1;            

            (*
            ** Mouse button 1 pressed above or left from 
            ** scrollbar mark: adjust page down
            ** Below or right from scrollbar mark: adjust page up
            ** Mouse button 1 held down in scrollbar mark: motion
            ** adjustment.
            *)
            let pos, total = match sens with
                  Vertical -> !mouse_y_event, g.height
                | Horizontal -> !mouse_x_event, g.width
            in

            match sens with
            | Vertical ->
                let y = adj#get_pos total in
                let dy = adj#get_page g.height in
                if ( y + dy < pos ) then
                    adj#page_down
                else if ( y > pos ) then
                    adj#page_up
                else
                begin
                    (* Motion adjustment *)
                    activated <- true;
                    self#configure [Relief ReliefSunken];
                end;
            | Horizontal ->
                let x = adj#get_pos total in
                let dx = adj#get_page g.width in
                if ( x + dx < pos ) then
                    adj#page_down
                else if ( x > pos ) then
                    adj#page_up
                else
                begin
                    (* Motion adjustment *)
                    activated <- true;
                    self#configure [Relief ReliefSunken];
                end;

            );

        ButtonMotion, (fun _ -> 
            if activated then
            begin
              let adj = adj in
              let g = w.w_geometry in
              let pos, total = match sens with
                  Vertical -> !mouse_y_event, g.height
                | Horizontal -> !mouse_x_event, g.width
              in
              match sens with
              | Vertical ->
                  let dy = adj#get_page g.height in
                  adj#set_pos (pos-(dy/2)) total;
              | Horizontal ->
                  let dx = adj#get_page g.width in
                  adj#set_pos (pos-(dx/2)) total;
            
            end
            );

        ButtonReleased, (fun _ -> 
            activated <- false;
            self#configure [Relief ReliefRaised];
            button <- None;);


(*

        Button (2,0), (fun _ -> 
            X.changeActivePointerGrab s.s_display 
              [ButtonMotionMask; ButtonReleaseMask;
              ButtonPressMask; OwnerGrabButtonMask]
              Xtypes.noCursor !Eloop.event_time;
            activated <- true;
            let adj = adj in
            let g = w.w_geometry in
            let pos, total = match sens with
                Vertical -> !mouse_y_event, g.height
              | Horizontal -> !mouse_x_event, g.width
            in
            adj#set_pos pos total);
*)


(*
        Button (3,0), (fun _ -> 
            let adj = adj in
            let g = w.w_geometry in
            self#timer !autorepeat_delay 3 (fun _ -> adj#page_down);
            button <- Some 3;            
            adj#page_down)
*)

      ]; Cursor (FontCursor 
          (if sens = Vertical then XC.xc_sb_v_double_arrow else
            XC.xc_sb_h_double_arrow))]
  
  val adj = adj
  val sens = sens
  
  method size_request = 
    let sz = szhints in
    if not w.w_size_modified || sz.comp_timestamp = s.s_timestamp then sz else
      begin
        sz.comp_timestamp <- s.s_timestamp;
        sz.requested_height <- max sz.min_height sz.requested_height;
        sz.requested_width <- max sz.min_width sz.requested_width;
        sz
      end
  
  method refresh =
    if s.s_timestamp > w.w_refresh_timestamp && not (w.w_window == noWindow) then
      begin
        super#refresh;
        if not (w.w_window == noWindow) then
          let adj = adj in
          let g = w.w_geometry in
          if subwin == noWindow then
            begin
              subwin <- X.createWindow s.s_display w.w_window
                2 2 16 16 copyDepthFromParent InputOutput copyVisualFromParent
                0 [CWBackPixel w.w_foreground.c_pixel];
              X.mapWindow s.s_display subwin
            end;
          match sens with   
            Vertical -> 
              let y = adj#get_pos g.height  in
              let dy = adj#get_page g.height in
              Xlib.moveResizeWindow s.s_display subwin 
                2 y (g.width - 4) dy
          | Horizontal ->
              let y = adj#get_pos g.width in
              let dy = adj#get_page g.width in
              Xlib.moveResizeWindow s.s_display subwin
                y 2 dy (g.height - 4)
      end
  
  method update_adjustement () = 
    self#wait_refresh false 0 0 0 0
    
  method name = "scrollbar"
end

class h parent adj attributes =
  object
  inherit t parent Horizontal adj attributes
end

class v parent adj attributes =
  object
  inherit t parent Vertical adj attributes 
end

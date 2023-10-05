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
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2005 BSSLAB
**    $CREATED:     21.6.2005
**    $VERSION:     1.13
**
**    $INFO:
**
**
**  A slider consists of 3 buttons:
**
**
**      |<| ----------- |====| -------------- |>|       
**
**  A left and right scroll button, and a middle drag button.
**
**  A slider can have horizontal or vertical alignement.
**
**  Sliders can be used either for scrolling viewport areas  (only
**  a part of a widget window is displayed on screen), or for adjustung
**  values. In the first case, an object window is divided in pages.
**  Only one page is displayed.
**
**  class h,v:
**
**  The following parameters are used (derived from viewport terminology):
**
**  1.) object size in arbitrary units. It's the full size of
**      an object displayed only partly in a viewport window.
**  2.) the view size in same arbitrary units. It's the portion of
**      object window currently displayed. The object window is broken 
**      up into pages.
**  3.) page size: if the user clicks on the up/down buttons, the
**      scrollbar moves one page (fraction of object size).
**
**
** The width of the drag button is defined with:
**
**  bwidth [pixel] = view_size/object_size * (width of slider window - n)
**  
**  n: left and right buts sum width
**
**
** A.) Normalized value 'pos' of the silder is in range 0.0 - 1.0
**
**
**  pos=0.0: drag button on the left side
**  pos=1.0: drag button on the right side
**
**  The normalized position value is changed in page_size/object_size
**  steps (with up/down buttons).
** 
** B.) Viewport page position
**
**      vp = object_offset + pos * (object_size - view_size)
**
**  The last value is the start position of the last page!
**  The first value is the start position of the first page!
**
** For A.) an ActionFU handler can be installed. each time the slider 
** position changes, this handler is called with the current normalized
** float position value. Always called if slider position changes. Can be
** used for example to update a text label field displaying the current
** value.
**
** For B.) an ActionIU handler can be installed with the current integer page
** start position. Always called if slider position changes. Needed for
** viewport implementation.
**
**
** C.) an ActionUU handler which is called only if one of the modify buttons
**     was released - that means the final value was choosen 
**
**  class view_h,view_v:
**
**  Viewport slider
**  Parameters: object_size,view_size,page_size
**  Action handlers: ActionIU, ActionUU
**
**  class val_h,val_v:
**
**  Parameter value adjustment - controlling a variable value
**  Action handler: ActionFU, ActionIU, ActionUU
**
**  Parameters: minimal and maximal value,step_large,step_small
** 
**  The step_small value is applied to drag button movement, the step_large
**  to the up and down buttons. The drag button width is set default to 
**  1/10 of the slider width.
**  
**
**  Vertical slider:
**
**          Down !!!
**          |
**          X
**          |
**          Up  !!!
**
**    $ENDOFINFO
**
*)



open Xtypes
open VX_types
open VX_common
open Printf
open GCCache
open Math 
open Thread 

let warn str =
    print_string ("Warning: "^str);
    print_newline ()


let def_size = 18


class orig kind parent attributes =
    object (self)
    (*
    ** Default text font: generic text and symbol of same size
    *)
    val mutable text_font = parent#font_make Helvetica Roman 12 true;
    val mutable text_font_symbol = parent#font_make Symbol Roman 12 true;

    inherit VX_object.t parent attributes as super

    val mutable but_size = def_size


    (*
    **  1.) object length in arbitrary integer units. It's the full size of
    **      an object displayed only partly in a viewport window.
    *)
    val mutable object_size = 100

    (*
    ** An arbitrary offset added to object position
    *)
    val mutable object_offset = 0

    (*
    **  2.) the page length in same arbitrary units. It's the portion of
    **      window currently displayed. The object window is broken up into
    **      pages.
    *)

    val mutable view_size = 10

    (*
    ** Scroll step width with up/down buttons
    *)
    val mutable page_size = 10

    (*
    ** Current normalized slider position 0.0 - 1.0
    *)
    val mutable pos = 0.0
    val mutable lastpos = 0.0

    (*
    ** The three buttons
    *)
    val mutable but_down = {(default_frame 1) with
                            f_bbox = bbox_of_xywh 0 0 def_size def_size;
                            f_type = ShadowRaised;} 
    val mutable but_up = {(default_frame 1) with
                            f_bbox = bbox_of_xywh 0 0 def_size def_size;
                            f_type = ShadowRaised;} 
    val mutable but_drag = {(default_frame 1) with
                            f_bbox = bbox_of_xywh 0 0 def_size def_size;
                            f_type = ShadowRaised;} 
    val mutable sym_down = 
        match kind with
        | Horizontal -> default_symbol S_LEFT
        | Vertical -> default_symbol S_UP       (* !!! *)

    val mutable sym_up = 
        match kind with
        | Horizontal -> default_symbol S_RIGHT
        | Vertical -> default_symbol S_DOWN     (* !!! *)


    (*
    ** Activated slider movement ? (Button pressed in drag button area)
    *)
    val mutable drag_active = false
    val mutable up_active = false
    val mutable down_active = false


    (*
    ** We have two kinds of action handlers for slider movement:
    **
    **  1.) A function with current normalized position value 'pos' 
    **      (value slider),
    **  2.) and a function with a page position 
    **      (viewport slider):
    **
    **      p = object_offset + pos * (object_size - view_size)
    **
    **  3.) and a handler called each time the user releases one of the
    **      buttons
    *)
 
    val mutable action_f = (fun _ -> ())    (* float version *)
    val mutable action_i = (fun _ -> ())    (* integer version *)


    (*
    ** Both
    *)
    val mutable action_done = (fun _ -> ())
    

    val mutable last_pos = (0,0)

    initializer 
        self#set_name "slider";
        __(self#configure [Cursor (FontCursor XC.xc_hand2)]@attributes);
        __(self#configure [Bindings [
          (*
          ** Keyboard and mouse user interaction.
          *)
          ButtonPress,(fun _ ->
              let x,y= !mouse_x_event,
                       !mouse_y_event in
                let within = within_bbox but_drag.f_bbox x y in
                if within && not drag_active then
                begin
                    drag_active <- true;
                    self#update_but 3 false;
                end;  
                let within = within_bbox but_up.f_bbox x y in
                if within && not up_active then
                begin
                    up_active <- true;
                    self#update_but 2 false;
                    self#page_up;
                end;  
                let within = within_bbox but_down.f_bbox x y in
                if within && not down_active then
                begin
                    down_active <- true;
                    self#update_but 1 false;
                    self#page_down;
                end;  
            );
          ButtonReleased,(fun _ ->
                if drag_active then
                begin
                    drag_active <- false;
                    self#update_but 3 false;
                    self#action;
                end;  
                if up_active then
                begin
                    up_active <- false;
                    self#update_but 2 false;
                    self#action;
                end;  
                if down_active then
                begin
                    down_active <- false;
                    self#update_but 1 false;
                    self#action;
                end;  
                
            );
            (*
            ** Drag button slider motion
            *)
            ButtonMotion, (fun _ -> 
                let g = w.w_geometry in
                if drag_active then
                begin
                  let x,y= !mouse_x_event,
                           !mouse_y_event in
                  let x',y' = last_pos in
                  if last_pos <> (x,y) then
                  begin
                    last_pos <- (x,y);
            
                    match kind with
                    | Horizontal when (x <> x')->
                        let _,_,bw,bh = bbox_to_xywh but_drag.f_bbox in
                        let bbox = bbox_of_xywh but_down.f_bbox.x2
                                                but_drag.f_bbox.y1
                                                (but_up.f_bbox.x1-
                                                 but_down.f_bbox.x2)
                                                g.height in
                                            
(*                        let within = within_bbox bbox x y in  
*) let within = true in
                        if within then
                        begin
                            let s0,_,sw,_ = bbox_to_xywh bbox in
                            let dw = bw/2 + w.w_ipad_x/2 in 
                            let s0,sw = s0+dw,sw-dw*2 in
                            let newpos = (i2f (x-s0)) / (i2f sw) in
                            self#set_pos newpos;
                            self#action;
                        end;
                    | Vertical when (y <> y')->
                        let _,_,bw,bh = bbox_to_xywh but_drag.f_bbox in
                        let bbox = bbox_of_xywh 0
                                                but_down.f_bbox.y2
                                                g.width
                                                (but_up.f_bbox.y1-
                                                 but_down.f_bbox.y2)
                                                in
                                            
(*                        let within = within_bbox bbox x y in  
*) let within = true in
                        if within then
                        begin
                            let _,s0,_,sh = bbox_to_xywh bbox in
                            let dh = bh/2 + w.w_ipad_y/2 in 
                            let s0,sh = s0+dh,sh-dh*2 in
                            let newpos = (i2f (y-s0)) / (i2f sh) in
                            self#set_pos newpos;
                            self#action;
                        end;
                  end;
                end;
            );
(*
            LeaveWindow,(fun _ ->
                if drag_active then 
                begin
                    drag_active <- false;
                    self#update_but 3 false;
                    self#action;
                end;
                if up_active then 
                begin
                    up_active <- false;
                    self#update_but 2 false;
                    self#action;
                end;
                if down_active then 
                begin
                    down_active <- false;
                    self#update_but 1 false;
                    self#action;
                end;
            );
*)
        ]])

    (*
    ** Handle action callback functions
    *)
    method action =
        let page_pos = object_offset +
                       f2i (pos * (i2f (object_size - view_size))) in
        action_f pos;
        action_i page_pos;
        let scroll_done =   (not up_active) &&
                            (not down_active) &&
                            (not drag_active) in
        if scroll_done then
            action_done ()


    method configure attrs =
        (*
        ** Check for width/height attributes and adjust
        ** button size if necessary.
        *)
        List.iter (fun attr ->
            match attr with
            | Width n ->
            begin
                match kind with
                | Vertical ->
                    (*
                    ** Button size = width - padding
                    *)
                    let n = n - 2 * w.w_ipad_x in

                    let bx,by,bw,bh = bbox_to_xywh but_up.f_bbox in
                    but_up.f_bbox <- bbox_of_xywh bx by n n;
                    let bx,by,bw,bh = bbox_to_xywh but_down.f_bbox in
                    but_down.f_bbox <- bbox_of_xywh bx by n n;
                    let bx,by,bw,bh = bbox_to_xywh but_drag.f_bbox in
                    but_drag.f_bbox <- bbox_of_xywh bx by n bh;
                    but_size <- n;     
                | Horizontal -> ()
            end;
            | Height n ->
            begin
                match kind with
                | Horizontal ->
                    (*
                    ** Button size = height - padding
                    *)
                    let n = n - 2 * w.w_ipad_y in

                    let bx,by,bw,bh = bbox_to_xywh but_up.f_bbox in
                    but_up.f_bbox <- bbox_of_xywh bx by n n;
                    let bx,by,bw,bh = bbox_to_xywh but_down.f_bbox in
                    but_down.f_bbox <- bbox_of_xywh bx by n n;
                    let bx,by,bw,bh = bbox_to_xywh but_drag.f_bbox in
                    but_drag.f_bbox <- bbox_of_xywh bx by bw n;
                    but_size <- n;     
                | Vertical -> ()
            end;
            | _ -> ();
            ) attributes;
        let remains = super#configure attrs in
        List.iter (fun attr ->
            match attr with
            | ActionFU f -> action_f <- f;
            | ActionIU i -> action_i <- i;
            | ActionUU u -> action_done <- u;
            | But attrs ->
                List.iter (fun attr ->
                match attr with
                | Frame t ->
                    but_up.f_type <- t;
                    but_down.f_type <- t;
                    but_drag.f_type <- t;
                | Color c ->
                begin
                    let col = parent#color_make c true in
                    match but_down.f_type with
                    | ReliefRaised
                    | ReliefSunken ->
                        let bg =  w.w_background in
                        let ig = col in
                        let fg = parent#getShadow col in
                        let hg = parent#getHilite col in
                        but_up.f_background <- bg;
                        but_up.f_fillground <- ig;
                        but_up.f_foreground <- fg;
                        but_up.f_auxiliary <- hg;
                        but_down.f_background <- bg;
                        but_down.f_fillground <- ig;
                        but_down.f_foreground <- fg;
                        but_down.f_auxiliary <- hg;
                        but_drag.f_background <- bg;
                        but_drag.f_fillground <- ig;
                        but_drag.f_foreground <- fg;
                        but_drag.f_auxiliary <- hg;
                    | _ ->
                        let bg =  w.w_background in
                        let ig = col in
                        but_up.f_background <- bg;
                        but_up.f_fillground <- ig;
                        but_down.f_background <- bg;
                        but_down.f_fillground <- ig;
                        but_drag.f_background <- bg;
                        but_drag.f_fillground <- ig;
                end;
                | Size n ->
                    let bx,by,bw,bh = bbox_to_xywh but_up.f_bbox in
                    but_up.f_bbox <- bbox_of_xywh bx by n n;
                    let bx,by,bw,bh = bbox_to_xywh but_down.f_bbox in
                    but_down.f_bbox <- bbox_of_xywh bx by n n;
                    let bx,by,bw,bh = bbox_to_xywh but_drag.f_bbox in
                    but_drag.f_bbox <- bbox_of_xywh bx by bw n;
                    but_size <- n;     

                | _ -> warn_attr "VX_slider" attr self#name;
                ) attrs;

            | _ -> warn_attr "VX_slider" attr self#name;
            ) remains;
        []

    method size_request =
        let sz = szhints in
        let frame = frame_size w.w_frame in
        if not w.w_size_modified || 
           sz.comp_timestamp = s.s_timestamp then sz
        else 
        begin
Db.Pr.s 10 "VX_slider#size_request";

            sz.comp_timestamp <- s.s_timestamp;
            match kind with
            | Horizontal ->
                let min_width = 2 * w.w_ipad_x + 2 * but_size + 20 in
                let min_height =  but_size + 2 * w.w_ipad_y in
    
                sz.requested_width <- 
                    min sz.max_width
                        (max min_width
                             sz.min_width);
                
                sz.requested_height <- 
                        min sz.max_height
                            (max min_height
                                 sz.min_height);
    
                sz
            | Vertical ->
                let min_height = 2 * w.w_ipad_y + 2 * but_size + 20 in
                let min_width =  but_size + 2 * w.w_ipad_x in
    
                sz.requested_width <- 
                    min sz.max_width
                        (max min_width
                             sz.min_width);
                
                sz.requested_height <- 
                        min sz.max_height
                            (max min_height
                                 sz.min_height);
    
                sz
        end

    

    (*
    ** Update only one button. 1:down, 2:up, 3:drag
    *)
    method update_but butn clear =
        let view_size = i2f view_size in
        let object_size = i2f object_size in
        let page_size = i2f page_size in

        let gcs = s.s_gcs in
        let dpy = s.s_display in
        let win = w.w_window in

        let fg = w.w_foreground.c_pixel in
        let bg =  w.w_background.c_pixel in
        let g = w.w_geometry in
        let width = g.width in
        let height = g.height in


        (*
        ** Recalculate bounding boxes of buttons and symbols.
        *)
        if not (win == noWindow) then
        match kind with
        | Horizontal ->
        begin
            let offered = width - 2 * w.w_ipad_x in
            match butn with
            | 1 ->  
                let _,_,bw1,bh1 =  bbox_to_xywh but_down.f_bbox in
                let x0,y0 = w.w_ipad_x,w.w_ipad_y in
                but_down.f_bbox <- bbox_of_xywh x0 y0 bw1 bh1;

                but_down.f_type <- (
                    match but_down.f_type with
                    | ShadowSunken
                    | ShadowRaised -> if down_active 
                                        then ShadowSunken
                                        else ShadowRaised;
                    | ReliefSunken
                    | ReliefRaised -> if down_active 
                                        then ReliefSunken
                                        else ReliefRaised;
                    | _ -> but_down.f_type;
                    );
                let bframe,bframe_off =
                        frame_size but_down,
                        frame_offset but_down in
                sym_down.sym_bbox <- bbox_of_xywh (x0+bframe_off) 
                                                  (y0+bframe_off) 
                                                  (bw1-bframe) 
                                                  (bh1-bframe);
                drawFrame s.s_display w gcs but_down true;
                drawSymbol s.s_display w gcs sym_down;

            | 2 ->
                let _,_,bw2,bh2 =  bbox_to_xywh but_up.f_bbox in
                let x0,y0 = w.w_ipad_x + offered - bw2,
                            w.w_ipad_y in
                but_up.f_bbox <- bbox_of_xywh x0 y0 bw2 bh2;
                but_up.f_type <- (
                    match but_up.f_type with
                    | ShadowSunken
                    | ShadowRaised -> if up_active 
                                        then ShadowSunken
                                        else ShadowRaised;
                    | ReliefSunken
                    | ReliefRaised -> if up_active 
                                        then ReliefSunken
                                        else ReliefRaised;
                    | _ -> but_up.f_type;
                    );
    
                let bframe,bframe_off =
                        frame_size but_up,
                        frame_offset but_up in
                sym_up.sym_bbox <-   bbox_of_xywh (x0+bframe_off) 
                                                  (y0+bframe_off) 
                                                  (bw2-bframe) 
                                                  (bh2-bframe);
                drawFrame s.s_display w gcs but_up true;
                drawSymbol s.s_display w gcs sym_up;

            | 3 ->
                let _,_,bw1,bh1 =  bbox_to_xywh but_down.f_bbox in
                let _,_,bw2,bh2 =  bbox_to_xywh but_up.f_bbox in

                if clear then
                begin
                    (*
                    ** Clear entire area. Needed after parameter
                    ** update.
                    *)
                    let gc' = GCCache.get_fg_bg gcs bg bg in
                    let b1 = but_down.f_bbox in
                    let b2 = but_up.f_bbox in
                    let rect = [
                            b1.x2,
                            b1.y1,
                            (b2.x1-b1.x2-1),
                            bh1
                        ] in
                    X.polyFillRectangle dpy win gc' rect;
                    
                end;

                (*
                ** Size of  drag button depends on fraction page/object len
                *)
                let slide_len = i2f (offered - bw1 - bw2 - w.w_ipad_x) in
                let drag_width = fmax 6.0
                                    ((view_size / object_size) * slide_len) in

                let pad = w.w_ipad_x/2 in
                let _,_,_,bh =  bbox_to_xywh but_drag.f_bbox in
                let x0,y0 = but_down.f_bbox.x2 + 1 + pad + 
                            (f2i (pos * (slide_len - drag_width))),
                            w.w_ipad_y in
                but_drag.f_bbox <- bbox_of_xywh x0 y0 (f2i drag_width) bh;
                but_drag.f_type <- (
                    match but_drag.f_type with
                    | ShadowSunken
                    | ShadowRaised -> if drag_active 
                                        then ShadowSunken
                                        else ShadowRaised;
                    | ReliefSunken
                    | ReliefRaised -> if drag_active 
                                        then ReliefSunken
                                        else ReliefRaised;
                    | _ -> but_drag.f_type;
                    );

                (*
                ** Clear first area around drag button
                *)
                let gc' = GCCache.get_fg_bg gcs bg bg in
                let bx,by,bw,bh = bbox_to_xywh but_drag.f_bbox in
                let pad = w.w_ipad_x/2 in
                let rect = [
                        bx-pad,by,bw+2*pad,bh
                    ] in
                X.polyFillRectangle dpy win gc' rect;

                (*
                ** Now draw drag button...
                *)
                drawFrame s.s_display w gcs but_drag true;

                (*
                ** Draw cable
                *)

                (*
                ** left side
                *)
                let lw = 3 in
                let x1,x2,y =
                    but_down.f_bbox.x2 + pad,
                    but_drag.f_bbox.x1 - pad,                    
                    height/2 in

                let gc = GCCache.get_fg_bg_lw gcs fg bg lw in
                if x2 > (x1+2) then
                    X.polyLine dpy win gc Origin [x1,y;x2,y];
                (*
                ** Right side
                *)
                let lw = 3 in
                let x1,x2,y =
                    but_drag.f_bbox.x2 + 1 + pad,
                    but_up.f_bbox.x1 - pad,                    
                    height/2 in

                let gc = GCCache.get_fg_bg_lw gcs fg bg lw in
                if x2 > (x1+2) then
                    X.polyLine dpy win gc Origin [x1,y;x2,y];
            | _ -> progerr "wrong button number";
        end;
        | Vertical ->
        begin
            let offered = height - 2 * w.w_ipad_y in
            match butn with
            | 1 ->  
                let _,_,bw1,bh1 =  bbox_to_xywh but_down.f_bbox in

                let x0,y0 = w.w_ipad_x,
                            w.w_ipad_y in
                but_down.f_bbox <- bbox_of_xywh x0 y0 bw1 bh1;

                but_down.f_type <- (
                    match but_down.f_type with
                    | ShadowSunken
                    | ShadowRaised -> if down_active 
                                        then ShadowSunken
                                        else ShadowRaised;
                    | ReliefSunken
                    | ReliefRaised -> if down_active 
                                        then ReliefSunken
                                        else ReliefRaised;
                    | _ -> but_down.f_type;
                    );
                let bframe,bframe_off =
                        frame_size but_down,
                        frame_offset but_down in
                sym_down.sym_bbox <- bbox_of_xywh (x0+bframe_off) 
                                                  (y0+bframe_off) 
                                                  (bw1-bframe) 
                                                  (bh1-bframe);
                drawFrame s.s_display w gcs but_down true;
                drawSymbol s.s_display w gcs sym_down;

            | 2 ->
                let _,_,bw2,bh2 =  bbox_to_xywh but_up.f_bbox in
                let x0,y0 = w.w_ipad_x,
                            height - w.w_ipad_y - bh2 in
                but_up.f_bbox <- bbox_of_xywh x0 y0 bw2 bh2;
                but_up.f_type <- (
                    match but_up.f_type with
                    | ShadowSunken
                    | ShadowRaised -> if up_active 
                                        then ShadowSunken
                                        else ShadowRaised;
                    | ReliefSunken
                    | ReliefRaised -> if up_active 
                                        then ReliefSunken
                                        else ReliefRaised;
                    | _ -> but_up.f_type;
                    );
    
                let bframe,bframe_off =
                        frame_size but_up,
                        frame_offset but_up in
                sym_up.sym_bbox <-   bbox_of_xywh (x0+bframe_off) 
                                                  (y0+bframe_off) 
                                                  (bw2-bframe) 
                                                  (bh2-bframe);
                drawFrame s.s_display w gcs but_up true;
                drawSymbol s.s_display w gcs sym_up;

            | 3 ->
                let _,_,bw1,bh1 =  bbox_to_xywh but_down.f_bbox in
                let _,_,bw2,bh2 =  bbox_to_xywh but_up.f_bbox in


                if clear then
                begin
                    (*
                    ** Clear entire area. Needed after parameter
                    ** update.
                    *)
                    let gc' = GCCache.get_fg_bg gcs bg bg in
                    let b1 = but_down.f_bbox in
                    let b2 = but_up.f_bbox in
                    let rect = [
                            b1.x1,
                            b1.y2,
                            bw1,
                            (b2.y1-b1.y2-1)
                        ] in
                    X.polyFillRectangle dpy win gc' rect;
                    
                end;


                (*
                ** Size of  drag button depends on fraction page/object len
                *)
                let slide_len = i2f (offered - bh1 - bh2 - w.w_ipad_y) in
                let drag_height = fmax 6.0
                                    ((view_size / object_size) * slide_len) in

                let pad = w.w_ipad_y/2 in
                let _,_,bw,_ =  bbox_to_xywh but_drag.f_bbox in
                let x0,y0 = w.w_ipad_x,
                            but_down.f_bbox.y2 + 1 + pad +
                            (f2i (pos * (slide_len - drag_height))) 
                            in
                but_drag.f_bbox <- bbox_of_xywh x0 y0 bw (f2i drag_height);
                but_drag.f_type <- (
                    match but_drag.f_type with
                    | ShadowSunken
                    | ShadowRaised -> if drag_active 
                                        then ShadowSunken
                                        else ShadowRaised;
                    | ReliefSunken
                    | ReliefRaised -> if drag_active 
                                        then ReliefSunken
                                        else ReliefRaised;
                    | _ -> but_drag.f_type;
                    );

                (*
                ** Clear first area around drag button
                *)
                let gc' = GCCache.get_fg_bg gcs bg bg in
                let bx,by,bw,bh = bbox_to_xywh but_drag.f_bbox in
                let pad = w.w_ipad_y/2 in
                let rect = [
                        bx,by-pad,
                        bw,bh+2*pad
                    ] in
                X.polyFillRectangle dpy win gc' rect;

                (*
                ** Now draw drag button...
                *)
                drawFrame s.s_display w gcs but_drag true;

                (*
                ** Draw cable
                *)

                (*
                ** down side
                *)
                let lw = 3 in
                let y1,y2,x =
                    but_down.f_bbox.y2 + pad,
                    but_drag.f_bbox.y1 - pad,                    
                    width/2 in

                let gc = GCCache.get_fg_bg_lw gcs fg bg lw in
                if y2 > (y1+2) then
                    X.polyLine dpy win gc Origin [x,y1;x,y2];

                (*
                ** Up side
                *)
                let lw = 3 in
                let y1,y2,x =
                    but_drag.f_bbox.y2 + 1 + pad,
                    but_up.f_bbox.y1 - pad,                    
                    width/2 in

                let gc = GCCache.get_fg_bg_lw gcs fg bg lw in
                if y2 > (y1+2) then
                    X.polyLine dpy win gc Origin [x,y1;x,y2];
            | _ -> progerr "wrong button number";
        end;

    (*
    ** Draw the widget content
    *)
    method refresh =
        if s.s_timestamp > w.w_refresh_timestamp && 
           not (w.w_window == noWindow) then
        begin
Db.Pr.s 10 "VX_slider#refresh";
            super#refresh;
            let sz = szhints in

            let fg = w.w_foreground.c_pixel in
            let bg =  w.w_background.c_pixel in
            let fc = w.w_foreground in
            let bc =  w.w_background in
            let font = text_font.font_id in
            let gcs = s.s_gcs in
            let gc = GCCache.get_fg_bg_font gcs fg bg font in

            let g = w.w_geometry in
            let width = g.width in
            let height = g.height in

            let frame = frame_size w.w_frame in
            let frame_off = frame_offset w.w_frame in

            if but_down.f_fillground = noColor then
                but_down.f_fillground <- bc;
            if but_up.f_fillground = noColor then
                but_up.f_fillground <- bc;
            if but_drag.f_fillground = noColor then
                but_drag.f_fillground <- bc;


            (*
            ** Update buttons
            *)
            for i = 1 to 3
            do
                self#update_but i false;
            done;
        end
    (*
    ** Set slider position in range 0.0 - 1.0 and update drag button.
    *)
    method set_pos newpos =
            let newpos = if newpos < 0.0 then 0.0
                         else if newpos > 1.0 then 1.0
                         else newpos in  
            (*
            ** Clear old drag button area
            *)
            let fg = w.w_foreground.c_pixel in
            let bg =  w.w_background.c_pixel in
            let gcs = s.s_gcs in
            let dpy = s.s_display in
            let win = w.w_window in

Db.Pr.sdd 10 "VX_slider#set_pos new/last" (f2i (1000.0*newpos))
                                         (f2i (1000.0*lastpos));

            if not (win == noWindow) &&
               newpos <> lastpos then
            begin
Db.Pr.sd 10 "VX_slider#set_pos new" (f2i (1000.0*newpos));
                (*
                ** Clear old dragon button
                *)
                match kind with
                | Horizontal ->
                    let gc = GCCache.get_fg_bg gcs bg bg in
                    let bx,by,bw,bh = bbox_to_xywh but_drag.f_bbox in
                    let pad = w.w_ipad_x/2 in
                    let rect = [
                            bx-pad,by,bw+2*pad,bh
                        ] in
                    X.polyFillRectangle dpy win gc rect;
                | Vertical ->
                    let gc = GCCache.get_fg_bg gcs bg bg in
                    let bx,by,bw,bh = bbox_to_xywh but_drag.f_bbox in
                    let pad = w.w_ipad_y/2 in
                    let rect = [
                            bx,by-pad,
                            bw,bh+2*pad
                        ] in
                    X.polyFillRectangle dpy win gc rect; 
            end;
            pos <- newpos;
            if newpos <> lastpos then self#update_but 3 false; 
            lastpos <- newpos;

    method get_pos = pos

    (*
    ** Some, but with respect to object_size (in integer pixel units)
    *)
    method set_pagepos newpos =
        let newpos = i2f newpos in
        let view_size = i2f view_size in
        let object_size = i2f object_size in
        let page_size = i2f page_size in
        let newpos' = newpos / (object_size - view_size) in
        self#set_pos newpos';

    (*
    ** Increment and decrement slider position in unit (page) sizes.
    *)
    method page_up =
        let view_size = i2f view_size in
        let object_size = i2f object_size in
        let page_size = i2f page_size in

        let dp = page_size/(object_size-view_size) in
        let pos = if pos < 1.0 
                    then (floor ((pos+1E-6)/dp))*dp 
                    else pos in
        let newpos = pos + dp in
        self#set_pos newpos;

    method page_down =
        let view_size = i2f view_size in
        let object_size = i2f object_size in
        let page_size = i2f page_size in

        let dp = page_size/(object_size-view_size) in
        let pos = if pos < 1.0 
                    then (floor ((pos+1E-3)/dp))*dp 
                    else pos    in
        let newpos = pos - dp in
        self#set_pos newpos;

    method update =
Db.Pr.s 10 "VX_slider#update";
        super#update; 
        self#wait_resize;
        self#wait_refresh true 0 0 0 0;


    (*
    ** Postscript printing
    *)

    (*
    ** Print only one button. 1:down, 2:up, 3:drag
    *)
    method print_but ps wx0 wy0 butn =
        let view_size = i2f view_size in
        let object_size = i2f object_size in
        let page_size = i2f page_size in

        let gcs = s.s_gcs in
        let dpy = s.s_display in

        let fc = w.w_foreground in
        let bc =  w.w_background in
        let g = w.w_geometry in
        let width = g.width in
        let height = g.height in

        match kind with
        | Horizontal ->
        begin
            let offered = width - 2 * w.w_ipad_x in
            match butn with
            | 1 ->  
                printFrame ps w wx0 wy0 but_down true;
                printSymbol ps dpy w wx0 wy0  sym_down;
            | 2 ->
                printFrame ps w wx0 wy0 but_up true;
                printSymbol ps dpy w wx0 wy0  sym_up;
            | 3 ->
                let _,_,bw1,bh1 =  bbox_to_xywh but_down.f_bbox in
                let _,_,bw2,bh2 =  bbox_to_xywh but_up.f_bbox in
                let pad = (i2f w.w_ipad_x)/2.0 in
                (*
                ** Now draw drag button...
                *)
                printFrame ps w wx0 wy0 but_drag true;

                (*
                ** Draw cable
                *)

                (*
                ** left side
                *)
                let lw = 3.0 in
                let x1,x2,y =
                    (i2f but_down.f_bbox.x2) + pad,
                    (i2f but_drag.f_bbox.x1) - pad,                    
                    (i2f height)/2.0 in

                if x2 > (x1+2.0) then
                    VX_ps.polyLine ps wx0 wy0 lw fc 
                            [x1,y;x2,y];
                (*
                ** Right side
                *)
                let x1,x2,y =
                    (i2f but_drag.f_bbox.x2) + 1.0 + pad,
                    (i2f but_up.f_bbox.x1) - pad,                    
                    (i2f height)/2.0 in

                if x2 > (x1+2.0) then
                    VX_ps.polyLine ps wx0 wy0 lw fc [x1,y;x2,y];
            | _ -> progerr "wrong button number";
        end;
        | Vertical ->
        begin
            let offered = height - 2 * w.w_ipad_y in
            match butn with
            | 1 ->  
                printFrame ps w wx0 wy0 but_down true;
                printSymbol ps dpy w wx0 wy0  sym_down;
            | 2 ->
                printFrame ps w wx0 wy0 but_up true;
                printSymbol ps dpy w wx0 wy0  sym_up;
            | 3 ->
                let _,_,bw1,bh1 =  bbox_to_xywh but_down.f_bbox in
                let _,_,bw2,bh2 =  bbox_to_xywh but_up.f_bbox in

                let pad = (i2f w.w_ipad_y)/2.0 in

                (*
                ** Now draw drag button...
                *)
                printFrame ps w wx0 wy0 but_drag true;

                (*
                ** Draw cable
                *)

                (*
                ** down side
                *)
                let lw = 3.0 in
                let y1,y2,x =
                    (i2f but_down.f_bbox.y2) + pad,
                    (i2f but_drag.f_bbox.y1) - pad,                    
                    (i2f width)/2.0 in

                if y2 > (y1+2.0) then
                    VX_ps.polyLine  ps wx0 wy0 lw fc [x,y1;x,y2];

                (*
                ** Up side
                *)
                let lw = 3.0 in
                let y1,y2,x =
                    (i2f but_drag.f_bbox.y2) + 1.0 + pad,
                    (i2f but_up.f_bbox.y1) - pad,                    
                    (i2f width)/2.0 in

                if y2 > (y1+2.0) then
                    VX_ps.polyLine  ps wx0 wy0 lw fc [x,y1;x,y2];
            | _ -> progerr "wrong button number";
        end;


    method print ps wx0 wy0 =
        super#print ps wx0 wy0;
        let sz = szhints in
        let fc = w.w_foreground in
        let bc =  w.w_background in
        let frame = frame_size w.w_frame in
        let frame_off = frame_offset w.w_frame in

        for i = 1 to 3
        do
            self#print_but ps wx0 wy0 i;
        done
end

class t = orig

(*
** Generic vertical aligned box
*)

class v parent attributes  =
    object (self)
    inherit t Vertical parent attributes
  
    initializer 
        self#set_name "slider.v"
end

(*
** Generic horizontal aligned box
*)
class h parent attributes =
    object (self)
    inherit t Horizontal parent attributes
  
    initializer  
        self#set_name "slider.h"
end

(*
** Viewport vertical slider
*)
class view_v parent objectsize
                    viewsize
                    pagesize
                    attributes =
    object (self)
    inherit t Vertical parent attributes
    initializer 
        self#set_name "slider.viewport_v";
        object_size <- objectsize;
        view_size <- viewsize;
        page_size <- pagesize;

    (*
    ** Set slider parameters at runtime
    *)
    method set_params objectsize 
                      viewsize
                      pagesize =
Db.Pr.sddd 10 "VX_slider.view_v#set_params" objectsize
                      viewsize
                      pagesize;
        if (objectsize <> object_size ||
            pagesize <> page_size ||
            viewsize <> view_size) then
        begin
            let oldpos = pos in
            let newpos = oldpos * (i2f object_size) / (i2f objectsize) in
            object_size <- objectsize;
            view_size <- viewsize;
            page_size <- pagesize;
            self#set_pos newpos;
            self#update_but 3 true;
        end;
end

(*
** Viewport horizontal slider
*)
class view_h parent objectsize
                    viewsize
                    pagesize
                    attributes =
    object (self)
    inherit t Horizontal parent attributes
    initializer 
        self#set_name "slider.viewport_h";
        object_size <- objectsize;
        view_size <- viewsize;
        page_size <- pagesize;
        
    (*
    ** Set slider parameters at runtime
    *)
    method set_params objectsize 
                      viewsize
                      pagesize =
        if (objectsize <> object_size ||
            pagesize <> page_size ||
            viewsize <> view_size) then
        begin
            let oldpos = pos in
            let newpos = oldpos * (i2f object_size) / (i2f objectsize) in
            object_size <- objectsize;
            view_size <- viewsize;
            page_size <- pagesize;
            self#set_pos newpos;
            self#update_but 3 true;
        end;
end


(*
** Value vertical slider. Differs from viewport slider in parameter
** settings and handling.
**
*)
class val_v parent      valmin
                        valmax
                        valsmall_step
                        vallarge_step (* must be a multiple of small_step *)
                        attributes =
    object (self)
    inherit t Vertical parent attributes as super
    
    initializer
        self#set_name "slider.value_v";

    val mutable val_min = i2f valmin
    val mutable val_max = i2f valmax
    val mutable val_small_step = i2f valsmall_step
    val mutable val_large_step = i2f vallarge_step


    method action =
        let pos' = (val_min + (val_max-val_min) * pos) in
        action_f pos';
        action_i (f2i pos');
        let scroll_done =   (not up_active) &&
                            (not down_active) &&
                            (not drag_active) in
        if scroll_done then
            action_done ()

    (*
    ** Increment and decrement slider position in large_step units.
    *)
    method page_up =
        let dp = val_large_step/(val_max-val_min) in
        let pos = if pos < 1.0 
                    then (floor ((pos+1E-6)/dp))*dp 
                    else pos in
        let newpos = pos + dp in
        self#set_pos newpos;

    method page_down =
        let dp = val_large_step/(val_max-val_min) in
        let pos = if pos < 1.0 
                    then (floor ((pos+1E-6)/dp))*dp 
                    else pos in
        let newpos = pos - dp in
        self#set_pos newpos;
    
    method set_pos newpos =
        (*
        ** Round up to nearest small_step value
        *)
        let dp = val_small_step/(val_max-val_min) in
        let newpos' = (floor ((newpos+1E-6)/dp))*dp in
        super#set_pos newpos'

end




(*
** Value horizontal slider. Differs from viewport slider in parameter
** settings and handling.
*)
class val_h parent      valmin
                        valmax
                        valsmall_step   
                        vallarge_step   (* must be a multiple of small_step *) 
                        attributes =
    object (self)
    inherit t Horizontal parent attributes as super

    initializer
        self#set_name "slider.value_h";

    val mutable val_min = i2f valmin
    val mutable val_max = i2f valmax
    val mutable val_small_step = i2f valsmall_step
    val mutable val_large_step = i2f vallarge_step

    method action =
        let pos' = (val_min + (val_max-val_min) * pos) in
        action_f pos';
        action_i (f2i (pos'+1E-6));
        let scroll_done =   (not up_active) &&
                            (not down_active) &&
                            (not drag_active) in
        if scroll_done then
            action_done ()

    (*
    ** Increment and decrement slider position in large_step units.
    *)
    method page_up =
        let dp = val_large_step/(val_max-val_min) in
        let pos = if pos < 1.0 
                    then (floor ((pos+1E-6)/dp))*dp 
                    else pos in
        let newpos = pos + dp in
        self#set_pos newpos;

    method page_down =
        let dp = val_large_step/(val_max-val_min) in
        let pos = if pos < 1.0 
                    then (floor ((pos+1E-6)/dp))*dp 
                    else pos in
        let newpos = pos - dp in
        self#set_pos newpos;
    
    method set_pos newpos =
        (*
        ** Round up to nearest small_step value
        *)
        let dp = val_small_step/(val_max-val_min) in
        let newpos' = (floor ((newpos+1E-6)/dp))*dp in
        super#set_pos newpos'

end

(*
** Value horizontal slider. Differs from viewport slider in parameter
** settings and handling. Float type version.
*)
class val_hf parent     valmin
                        valmax
                        valsmall_step   
                        vallarge_step   (* must be a multiple of small_step *) 
                        attributes =
    object (self)
    inherit t Horizontal parent attributes as super

    initializer
        self#set_name "slider.value_h";

    val mutable val_min = valmin
    val mutable val_max = valmax
    val mutable val_small_step = valsmall_step
    val mutable val_large_step = vallarge_step

    method action =
        let pos' = (val_min + (val_max-val_min) * pos) in
        action_f pos';
        action_i (f2i (pos'+1E-6));
        let scroll_done =   (not up_active) &&
                            (not down_active) &&
                            (not drag_active) in
        if scroll_done then
            action_done ()

    (*
    ** Reconfigure slider during runtime
    *)
    method val_config valmin
                      valmax
                      valsmall_step
                      vallarge_step =
        val_min <- valmin;
        val_max <- valmax;
        val_small_step <- valsmall_step;
        val_large_step <- vallarge_step;
        self#update;

    (*
    ** Increment and decrement slider position in large_step units.
    *)
    method page_up =
        let dp = val_large_step/(val_max-val_min) in
        let pos = if pos < 1.0 
                    then (floor ((pos+1E-6)/dp))*dp 
                    else pos in
        let newpos = pos + dp in
        self#set_pos newpos;

    method page_down =
        let dp = val_large_step/(val_max-val_min) in
        let pos = if pos < 1.0 
                    then (floor ((pos+1E-6)/dp))*dp 
                    else pos in
        let newpos = pos - dp in
        self#set_pos newpos;
    
    method set_pos newpos =
        (*
        ** Round up to nearest small_step value
        *)
        let dp = val_small_step/(val_max-val_min) in
        let newpos' = (floor ((newpos+1E-6)/dp))*dp in
        super#set_pos newpos'

end


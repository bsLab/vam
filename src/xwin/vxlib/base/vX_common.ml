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
**    $CREATED:     1.6.2005
**    $VERSION:     1.28
**
**    $INFO:
**
**  Common functions and utils
**
**    $ENDOFINFO
**
*)


open Xtypes
open Xlib
open VX_types
open Math

let i2f = float_of_int
let f2i = int_of_float
let fmin a b = if a < b then a else b
let fmax a b = if a > b then a else b

let vx_warn = ref false


(*
** Attribute warning
*)

let warn_attr str attr myname =
    let name = 
        match attr with
        | Width _ -> "Width";
        | MinWidth _ -> "MinWidth";
        | MaxWidth _ -> "MaxWidth";
        | Height _ -> "Height";
        | MinHeight _ -> "MinHeight";
        | MaxHeight _ -> "MaxHeight";
        | ExpandX _ -> "ExpandX";
        | ExpandY _ -> "ExpandY";
        | Mutable _ -> "Mutable";
        | Mutual _ -> "Mutual";
        | AdjustX _ -> "AdjustX";
        | AdjustY _ -> "AdjustY";
        | OpadX _ -> "OpadX";
        | OpadY _ -> "OpadY";
        | IpadX _ -> "IpadX";
        | IpadY _ -> "IpadY";
        | PosX _ -> "PosX";
        | PosY _ -> "PosY";
        | Background _ -> "Background";
        | Foreground _ -> "Foreground";
        | Border _ -> "Border";
        | Frame _ -> "Frame";
        | Coln _ -> "Coln";
        | Rown _ -> "Rown";
        | ForAll _ -> "ForAll";
        | Cols _ -> "Cols";
        | Rows _ -> "Rows";
        | Group _ -> "Group";
        | Text_baseline _ -> "Text_baseline";
        | Text_size _ -> "Text_size";
        | Text_font _ -> "Text_font";
        | Text_style _ -> "Text_style";
        | Align _ -> "Align";
        | EventMask  _ -> "EventMask";
        | Bindings _ -> "Bindings";
        | Sym _ -> "Sym";
        | Shape _ -> "Shape";
        | Active _ -> "Active";
        | Size _ -> "Size";
        | Line _ -> "Line";
        | But _ -> "But";
        | ActionUU _ -> "ActionUU";
        | ActionSU _ -> "ActionSU";
        | ActionIU _ -> "ActionIU";
        | ActionFU _ -> "ActionFU";
        | ActionSSS _ -> "ActionSSS";
        | ActionUP _ -> "ActionUP";
        | ActionDOWN _ -> "ActionDOWN";
        | Sides _ -> "Sides";
        | Cursor _ -> "Cursor";
        | Color _  -> "Color";
        | Position _ -> "Position";
        | RetractY _ -> "RetractY";
        | RetractX _ -> "RetractX";
        | IncY _ -> "IncY";
        | IncX _ -> "IncX";
        | Widget _ -> "Widget";
        | Label _ -> "Label";
        in
    print_string ("Warning: ["^myname^"]: "^str^" [attribute ignored: "^name^"]");
    print_newline ()

let vx_error str =
    print_string ("Fatal Error: "^str);
    print_newline ();
    exit 1;
    assert false




(*
** Grahical Context Cache shared by several widgets (box, text, ...): 
** Get a context with the following parameters:
**
**  foreground color    -> pixel_to_id col  -> Fg 
**  background color    -> pixel_to_id col  -> Fg_bg
**  font                -> font_to_id font_id   -> Fg_bg_font
**
**  linewidth           -> int -> Fg_bg_ls
**  linestyle           -> LineSolid,LineOnOffDash,LineDoubleDash
**  dashwidth           -> int (ignored when = 0)
*)

module GCCache = struct
    (*
    ** Size of cache
    *)
    let size = 32

    let create dpy scr = 
        let fg = scr.scr_black_pixel in
        let bg = scr.scr_white_pixel in
        let font = X.openFont dpy !default_font in
        let t = { 
            gcs = Array.init size (fun _ -> 
                        X.createGC dpy scr.scr_root
                                   [GCforeground fg; 
                                    GCbackground bg; 
                                    GCfont font;
                                    GCline_width 1]);
            ids = Array.create size Empty;
            next = 0;
            dpy = dpy;
            root = scr.scr_root;
            } in
        for i = 0 to size - 1 
        do 
            t.ids.(i) <- Fg_bg_font (
                             (pixel_to_id fg),
                             (pixel_to_id bg),
                             (font_to_id font));
        done;
        t

    let get_gc t needed =
        let ids = t.ids in
        let rec iter i =
            if ids.(i) = needed then
                t.gcs.(i)
            else if i>0 then
                iter (i-1)
            else
            begin
                let next = t.next in
                let gc = t.gcs.(next) in
                t.next <- (next+1) mod size;
                match needed with
                | Fg fg ->
                    ids.(next) <- needed;
                    X.freeGC t.dpy gc;
                    t.gcs.(next) <- X.createGC t.dpy t.root
                                        [GCforeground fg];
                    t.gcs.(next);
                | Fg_bg (fg,bg) ->
                    ids.(next) <- needed;
                    X.freeGC t.dpy gc;
                    t.gcs.(next) <- X.createGC t.dpy t.root
                                        [GCforeground fg;
                                         GCbackground bg];
                    t.gcs.(next);
                | Fg_bg_font (fg,bg,font) ->
                    ids.(next) <- needed;
                    X.freeGC t.dpy gc;
                    t.gcs.(next) <- X.createGC t.dpy t.root
                                        [GCforeground fg;
                                         GCbackground bg;
                                         GCfont font];
                    t.gcs.(next);
                | Fg_bg_ls (fg,bg,width,style,dashes) ->
                    ids.(next) <- needed;
                    X.freeGC t.dpy gc;
                    t.gcs.(next) <- X.createGC t.dpy t.root 
                                       ([GCforeground fg;
                                         GCbackground bg;
                                         GCline_width width;
                                         GCline_style style;
                                         ] @ (
                                if dashes > 0 then
                                    [GCdashes dashes]
                                else
                                    [])
                            ); 
                    t.gcs.(next);
                | Empty -> failwith "programming error";
            end;
        in
        iter (size-1)
             
    let get_fg t fg =
        get_gc t (Fg fg)

    let get_fg_bg t fg bg =
        get_gc t (Fg_bg (fg,bg))

    let get_fg_bg_font t fg bg font =
        get_gc t (Fg_bg_font (fg,bg,font))

    let get_fg_bg_lw t fg bg lw =
        get_gc t (Fg_bg_ls (fg,bg,lw,LineSolid,0))

  end

(*
** Return a font descriptor of specified font
*)
let font_desc font = 
    {
        text_font = font.font_class;
        text_style = font.font_style;
        text_size = font.font_size;
    }

(*
** Return a X11 font name
*)
let font_name name style size =
    match name with
    | Times -> 
    begin
        match style with
        | NoStyle
        | Roman ->
        begin
            match size with
            | a when a>=0 && a<=8 -> Fonts.Times.Roman.s8;
            | a when a>=9 && a<=10 -> Fonts.Times.Roman.s10;
            | 11 -> Fonts.Times.Roman.s11;
            | 12 -> Fonts.Times.Roman.s12;
            | a when a>=13 && a<=14 -> Fonts.Times.Roman.s14;
            | a when a>=15 && a<=18 -> Fonts.Times.Roman.s18;
            | a when a>=19 && a<=20 -> Fonts.Times.Roman.s20;
            | a when a>=21 && a<=24 -> Fonts.Times.Roman.s24;
            | _ -> Fonts.Times.Roman.s24;
        end;
        | Bold ->
        begin
            match size with
            | a when a>=0 && a<=8 -> Fonts.Times.Bold.s8;
            | a when a>=9 && a<=10 -> Fonts.Times.Bold.s10;
            | 11 -> Fonts.Times.Bold.s11;
            | 12 -> Fonts.Times.Bold.s12;
            | a when a>=13 && a<=14 -> Fonts.Times.Bold.s14;
            | a when a>=15 && a<=18 -> Fonts.Times.Bold.s18;
            | a when a>=19 && a<=20 -> Fonts.Times.Bold.s20;
            | a when a>=21 && a<=24 -> Fonts.Times.Bold.s24;
            | _ -> Fonts.Times.Bold.s24;
        end;
        | Italic ->
        begin
            match size with
            | a when a>=0 && a<=8 -> Fonts.Times.Italic.s8;
            | a when a>=9 && a<=10 -> Fonts.Times.Italic.s10;
            | 11 -> Fonts.Times.Italic.s11;
            | 12 -> Fonts.Times.Italic.s12;
            | a when a>=13 && a<=14 -> Fonts.Times.Italic.s14;
            | a when a>=15 && a<=18 -> Fonts.Times.Italic.s18;
            | a when a>=19 && a<=20 -> Fonts.Times.Italic.s20;
            | a when a>=21 && a<=24 -> Fonts.Times.Italic.s24;
            | _ -> Fonts.Times.Italic.s24;
        end;
    end; 
    | Helvetica ->
    begin
        match style with
        | NoStyle
        | Roman ->
        begin
            match size with
            | a when a>=0 && a<=8 -> Fonts.Helvetica.Roman.s8;
            | a when a>=9 && a<=10 -> Fonts.Helvetica.Roman.s10;
            | 11 -> Fonts.Helvetica.Roman.s11;
            | 12 -> Fonts.Helvetica.Roman.s12;
            | a when a>=13 && a<=14 -> Fonts.Helvetica.Roman.s14;
            | a when a>=15 && a<=18 -> Fonts.Helvetica.Roman.s18;
            | a when a>=19 && a<=20 -> Fonts.Helvetica.Roman.s20;
            | a when a>=21 && a<=24 -> Fonts.Helvetica.Roman.s24;
            | _ -> Fonts.Helvetica.Roman.s24;
        end;
        | Bold ->
        begin
            match size with
            | a when a>=0 && a<=8 -> Fonts.Helvetica.Bold.s8;
            | a when a>=9 && a<=10 -> Fonts.Helvetica.Bold.s10;
            | 11 -> Fonts.Helvetica.Bold.s11;
            | 12 -> Fonts.Helvetica.Bold.s12;
            | a when a>=13 && a<=14 -> Fonts.Helvetica.Bold.s14;
            | a when a>=15 && a<=18 -> Fonts.Helvetica.Bold.s18;
            | a when a>=19 && a<=20 -> Fonts.Helvetica.Bold.s20;
            | a when a>=21 && a<=24 -> Fonts.Helvetica.Bold.s24;
            | _ -> Fonts.Helvetica.Bold.s24;
        end;
        | Italic ->
        begin
            match size with
            | a when a>=0 && a<=8 -> Fonts.Helvetica.Italic.s8;
            | a when a>=9 && a<=10 -> Fonts.Helvetica.Italic.s10;
            | 11 -> Fonts.Helvetica.Italic.s11;
            | 12 -> Fonts.Helvetica.Italic.s12;
            | a when a>=13 && a<=14 -> Fonts.Helvetica.Italic.s14;
            | a when a>=15 && a<=18 -> Fonts.Helvetica.Italic.s18;
            | a when a>=19 && a<=20 -> Fonts.Helvetica.Italic.s20;
            | a when a>=21 && a<=24 -> Fonts.Helvetica.Italic.s24;
            | _ -> Fonts.Helvetica.Italic.s24;
        end;
    end;
    | Symbol ->
    begin
        match style with
        | NoStyle
        | Roman ->
        begin
            match size with
            | a when a>=0 && a<=8 -> Fonts.Symbol.Roman.s8;
            | a when a>=9 && a<=10 -> Fonts.Symbol.Roman.s10;
            | 11 -> Fonts.Symbol.Roman.s11;
            | 12 -> Fonts.Symbol.Roman.s12;
            | a when a>=13 && a<=14 -> Fonts.Symbol.Roman.s14;
            | a when a>=15 && a<=18 -> Fonts.Symbol.Roman.s18;
            | a when a>=19 && a<=20 -> Fonts.Symbol.Roman.s20;
            | a when a>=21 && a<=24 -> Fonts.Symbol.Roman.s24;
            | _ -> Fonts.Symbol.Roman.s24;
        end;
        | Bold ->
        begin
            match size with
            | a when a>=0 && a<=8 -> Fonts.Symbol.Bold.s8;
            | a when a>=9 && a<=10 -> Fonts.Symbol.Bold.s10;
            | 11 -> Fonts.Symbol.Bold.s11;
            | 12 -> Fonts.Symbol.Bold.s12;
            | a when a>=13 && a<=14 -> Fonts.Symbol.Bold.s14;
            | a when a>=15 && a<=18 -> Fonts.Symbol.Bold.s18;
            | a when a>=19 && a<=20 -> Fonts.Symbol.Bold.s20;
            | a when a>=21 && a<=24 -> Fonts.Symbol.Bold.s24;
            | _ -> Fonts.Symbol.Bold.s24;
        end;
        | Italic ->
        begin
            match size with
            | a when a>=0 && a<=8 -> Fonts.Symbol.Italic.s8;
            | a when a>=9 && a<=10 -> Fonts.Symbol.Italic.s10;
            | 11 -> Fonts.Symbol.Italic.s11;
            | 12 -> Fonts.Symbol.Italic.s12;
            | a when a>=13 && a<=14 -> Fonts.Symbol.Italic.s14;
            | a when a>=15 && a<=18 -> Fonts.Symbol.Italic.s18;
            | a when a>=19 && a<=20 -> Fonts.Symbol.Italic.s20;
            | a when a>=21 && a<=24 -> Fonts.Symbol.Italic.s24;
            | _ -> Fonts.Symbol.Italic.s24;
        end;
    end;
    | Fixed
    | NoFont
    | Courier -> 
    begin
        match style with
        | NoStyle
        | Roman ->
        begin
            match size with
            | a when a>=0 && a<=8 -> Fonts.Courier.Roman.s8;
            | a when a>=9 && a<=10 -> Fonts.Courier.Roman.s10;
            | 11 -> Fonts.Courier.Roman.s11;
            | 12 -> Fonts.Courier.Roman.s12;
            | a when a>=13 && a<=14 -> Fonts.Courier.Roman.s14;
            | a when a>=15 && a<=18 -> Fonts.Courier.Roman.s18;
            | a when a>=19 && a<=20 -> Fonts.Courier.Roman.s20;
            | a when a>=21 && a<=24 -> Fonts.Courier.Roman.s24;
            | _ -> Fonts.Courier.Roman.s24;
        end;
        | Bold ->
        begin
            match size with
            | a when a>=0 && a<=8 -> Fonts.Courier.Bold.s8;
            | a when a>=9 && a<=10 -> Fonts.Courier.Bold.s10;
            | 11 -> Fonts.Courier.Bold.s11;
            | 12 -> Fonts.Courier.Bold.s12;
            | a when a>=13 && a<=14 -> Fonts.Courier.Bold.s14;
            | a when a>=15 && a<=18 -> Fonts.Courier.Bold.s18;
            | a when a>=19 && a<=20 -> Fonts.Courier.Bold.s20;
            | a when a>=21 && a<=24 -> Fonts.Courier.Bold.s24;
            | _ -> Fonts.Courier.Bold.s24;
        end;
        | Italic ->
        begin
            match size with
            | a when a>=0 && a<=8 -> Fonts.Courier.Italic.s8;
            | a when a>=9 && a<=10 -> Fonts.Courier.Italic.s10;
            | 11 -> Fonts.Courier.Italic.s11;
            | 12 -> Fonts.Courier.Italic.s12;
            | a when a>=13 && a<=14 -> Fonts.Courier.Italic.s14;
            | a when a>=15 && a<=18 -> Fonts.Courier.Italic.s18;
            | a when a>=19 && a<=20 -> Fonts.Courier.Italic.s20;
            | a when a>=21 && a<=24 -> Fonts.Courier.Italic.s24;
            | _ -> Fonts.Courier.Italic.s24;
        end;
    end

(*
** Return nearest avaiable size (must be kept consistent with font_name sizing)
*)
let font_size size =
    match size with
    | a when a>=0 && a<=8 -> 8;
    | a when a>=9 && a<=10 -> 10;
    | 11 -> 11;
    | 12 -> 12;
    | a when a>=13 && a<=14 -> 14;
    | a when a>=15 && a<=18 -> 18;
    | a when a>=19 && a<=20 -> 20;
    | a when a>=21 && a<=24 -> 24;
    | _ -> 24

    
(*
** Create a frame structure from Border attribute list.
*)
let create_frame win colormake attrs =
    let g = win.w_geometry in
    let w,h = g.width, g.height in
    let f = {
        f_bbox = bbox_of_xywh 0 0 w h;
        f_type = Flat;
        f_shape = S_Rect;
        f_size = 1;
        (*
        ** Must be updated later
        *)
        f_foreground = noColor;
        f_background = noColor;
        f_fillground = noColor;
        f_auxiliary = noColor;
        f_black = colormake "black";
        f_line = L_Solid;
        f_depth = 1;
        f_sides = [];
    } in
    List.iter (fun a ->
        match a with
        | Width n -> f.f_size <- n;
        | Size n -> f.f_size <- n;
        | Shape s -> f.f_shape <- s;
        | Frame t -> f.f_type <- t;
        | Sides s -> f.f_sides <- s;    
        | Color c -> f.f_foreground <- colormake c;
        | _ -> warn_attr "" a "create_frame";
        ) attrs;
    f

(*
** Create text baseline structure from attributes.
*)

let create_baseline win colormake attrs =
    let tb = {
            tb_width=1;
            tb_color=noColor;
            tb_linetype=L_Solid;
        } in
    List.iter (fun a ->
        match a with
        | Width n -> tb.tb_width <- n;
        | Size n -> tb.tb_width <- n;
        | Line l -> tb.tb_linetype <- l;
        | Color c -> tb.tb_color <- colormake c;
        | _ -> warn_attr "" a "create_baseline";
        ) attrs;
    tb


(*
** Does bounding box area overlaps with clipping region ?
*)

let clip_visible clip bbox =
    let x1,y1,x2,y2 = clip.x,
                      clip.y,
                      clip.x+clip.width-1,
                      clip.y+clip.height-1 in
    (
        (bbox.x1 >= x1 && bbox.x1 <= x2) ||
        (x1 >= bbox.x1 && x1 <= bbox.x2) ||
        (bbox.x2 >= x1 && bbox.x2 <= x2) ||
        (x2 >= bbox.x1 && x2 <= bbox.x2)
    ) &&
    (
        (bbox.y1 >= y1 && bbox.y1 <= y2) ||
        (y1 >= bbox.y1 && y1 <= bbox.y2) ||
        (bbox.y2 >= y1 && bbox.y2 <= y2) ||
        (y2 >= bbox.y1 && y2 <= bbox.y2)
    )

let delta_move_size = ref 1

      
let set_grabs display win list =
  List.iter (fun (g,_) ->
      try
        match g with
        | GrabbedKey (keysym,modifiers) ->
            let keycode,_ = KeyBind.keysymToKeycode display keysym in
            X.grabKey display win false
              GrabModeAsync GrabModeAsync keycode modifiers;
        | GrabbedButton (button,modifiers) ->
            X.grabButton display win false []
              GrabModeAsync GrabModeAsync noConfineTo Xtypes.noCursor 
              button modifiers;
        | _ -> ()
      with _ -> ()
  ) list
  
let unset_grabs display win list =
  List.iter (fun (g,_) ->
      try
        match g with
        | GrabbedKey (keysym,modifiers) ->
            let keycode,_ = KeyBind.keysymToKeycode display keysym in
            X.ungrabKey display win keycode modifiers;
        | GrabbedButton (button,modifiers) ->
            X.ungrabButton display win button modifiers;
        |  _ -> ()
      with _ -> ()
  ) list

let mouse_x_event = ref 0
let mouse_y_event = ref 0
let button_event = ref 0
let key_string = ref ""
let key_sym = ref 0
let modifiers_event = ref 0
      
type null = < >
let null_handler () = ()

let default_click =     {
    Xbutton.detail = 0;
    Xbutton.time = currentTime;
    Xbutton.root = noWindow;
    Xbutton.event = noWindow;
    Xbutton.child = noWindow;
    Xbutton.x_event = 0;
    Xbutton.y_event = 0;
    Xbutton.x_root = 0;
    Xbutton.y_root = 0;
    Xbutton.state = 0;
    Xbutton.same_screen = false;
  }


(*
** Draw routines (some based on XMU library)
*)


(*
** Convert angle in degree in X11 angle value
*)
let xa a = a * 64 
let drawRoundedRectangle dpy win gc x y w h ew eh =
    let ew2 = ew * 2 in
    let eh2 = eh * 2 in
    let arcs = [
            x,y,ew2,eh2,xa 180,xa (-90);
            x+ew,y,w-ew2,0,xa 180,xa (-180);
            x+w-ew2,y,ew2,eh2,xa 90,xa (-90);
            x+w,y+eh,0,h-eh2,xa 90,xa (-180);
            x+w-ew2,y+h-eh2,ew2,eh2,0,xa (-90);
            x+ew,y+h,w-ew2,0,0,xa (-180);
            x,y+h-eh2,ew2,eh2,xa 270,xa (-90);
            x,y+eh,0,h-eh2,xa 270,xa (-180);
        ] in
    X.polyArc dpy win gc arcs

let fillRoundedRectangle dpy win gc x y w h ew eh =
    let ew2 = ew * 2 in
    let eh2 = eh * 2 in
    let arcs = [
            x,y,ew2,eh2,xa 180,xa (-90);
            x+w-ew2-1,y,ew2,eh2,xa 90, xa (-90);
            x+w-ew2-1,y+h-eh2-1,ew2,eh2,0,xa (-90);
            x,y+h-eh2-1,ew2,eh2,xa 270, xa (-90);
        ] in
    X.polyFillArc dpy win gc arcs;
    let rects = [
            x+ew,y,w-ew2,h;
            x,y+eh,ew,h-eh2;
            x+w-ew,y+eh,ew,h-eh2;
        ] in
    X.polyFillRectangle dpy win gc rects

(*
** Postscript printing
*)
let printRoundedRectangle ps x0 y0 color lw x y w h ew eh =
    let ew2 = ew * 2.0 in
    let eh2 = eh * 2.0 in
    let arcs = [
            x,y,ew2,eh2,180.0,(-90.0);
            x+w-ew2,y,ew2,eh2,90.0,(-90.0);
            x+w-ew2,y+h-eh2,ew2,eh2,0.0,(-90.0);
            x,y+h-eh2,ew2,eh2,270.0,(-90.0);
        ] in
    VX_ps.polyArc ps x0 y0 lw color arcs;
    let lw2 = lw/2.0 in
    let l1 = [x+ew,y+lw2; x+w-ew,y+lw2;] in
    let l2 = [x+ew,y+h-lw2; x+w-ew,y+h-lw2;] in
    let l3 = [x+lw2,y+eh;x+lw2,y+h-eh] in
    let l4 = [x+w-lw2,y+eh;x+w-lw2,y+h-eh] in
    VX_ps.polyLine ps x0 y0 lw color l1;
    VX_ps.polyLine ps x0 y0 lw color l2;
    VX_ps.polyLine ps x0 y0 lw color l3;
    VX_ps.polyLine ps x0 y0 lw color l4


let printFilledRoundedRectangle ps x0 y0 color x y w h ew eh =
    let ew2 = ew * 2.0 in
    let eh2 = eh * 2.0 in
    let one = 1.0 in
    let arcs = [
            x,y,ew2,eh2,180.0,(-90.0);
            x+w-ew2-one,y,ew2,eh2,90.0,(-90.0);
            x+w-ew2-one,y+h-eh2-one,ew2,eh2,0.0,(-90.0);
            x,y+h-eh2-one,ew2,eh2,270.0,(-90.0);
        ] in
    VX_ps.polyFillArc ps x0 y0 color arcs; 
    let rects = [
            x+ew,y,w-ew2+one,h;
            x,y+eh,ew+one,h-eh2+one;
            x+w-ew-one,y+eh,ew+one,h-eh2+one;
        ] in
    VX_ps.polyFillRectangle ps x0 y0 color rects


(*
** Frame drawing : Border (flat line), Shadow, Relief.
** Note: the background must be filled externally!!
*)
    
let drawBorder2 dpy win cache fg bg frame =
    let f = frame in
    let b = f.f_bbox in
    let x,y,w,h = bbox_to_xywh b in
    let x1,y1,x2,y2 = b.x1,b.y1,b.x2,b.y2 in
    let lw = f.f_size in
    let fg = if f.f_foreground <> noColor then
                    f.f_foreground.c_pixel 
                 else
                    fg in
    let bg = if f.f_background <> noColor then
                    f.f_background.c_pixel 
                 else
                    bg in
    let ig = f.f_fillground.c_pixel in

    let gc = GCCache.get_fg_bg_lw cache fg bg lw in
    let gc' = GCCache.get_fg_bg_lw cache bg bg lw in


    let lw2 = lw/2 in
    let one = if lw2 = 0 then 1 else 0 in

    let dx,dy = w,h in
    let x,y,dx,dy = ref x,
                    ref y,
                    ref dx,
                    ref dy in


    match f.f_shape with
    | S_Rect ->
        for i = 1 to f.f_depth
        do
            if f.f_sides = [] then      (* draw all border sides *)
                X.polyLine dpy win gc Origin [
                    !x+lw2,            !y+lw2;
                    !x+ !dx-lw2-one,   !y+lw2;
                    !x+ !dx-lw2-one,   !y+ !dy-lw2-one;
                    !x+lw2,            !y+ !dy-lw2-one;
                    !x+lw2,            !y+lw2;
                ]
            else
            begin
                (*
                ** Draw only selected border sides
                *)
                List.iter (fun s ->
                    match s with
                    | B_top ->
                        X.polyLine dpy win gc Origin [
                                !x,          !y+lw2;
                                !x+ !dx,     !y+lw2;
                            ];
                    | B_bottom ->
                        X.polyLine dpy win gc Origin [
                                !x,          !y+ !dy-lw2-one;
                                !x+ !dx,     !y+ !dy-lw2-one;
                            ];
                    | B_left ->
                        X.polyLine dpy win gc Origin [
                                !x+lw2,      !y;
                                !x+lw2,      !y+ !dy;
                            ];
                    | B_right ->
                        X.polyLine dpy win gc Origin [
                                !x+ !dx-lw2-one, !y;
                                !x+ !dx-lw2-one, !y+ !dy;
                            ];
                    ) f.f_sides;
            end;
            x := !x + (2*f.f_size);
            y := !y + (2*f.f_size);
            dx := !dx - (4*f.f_size);
            dy := !dy - (4*f.f_size);
        done

    | S_Oval ->
        let bw = lw in
        (*
        ** border line width adjustment
        *)

        (*
        ** Draw first fillground, then foreground.
        *)
        let d = 0 in

        let dx' = max 0 ((bw-1) / 2) in 
        let dx'' = max 0 (bw / 2) in 
        let dy' = max 0 ((bw-1) / 2) in
        let dy'' = max 0 (bw / 2) in

        let x1' = x1 + d in
        let y1' = y1 + d in
        let x2' = x2 + d - dx' in
        let y2' = y2 + d - dy' in
        let w = x2' - x1' in
        let h = y2' - y1' in
    
        let cr = min (h/2) 10 in
        (*
        ** Erase first unused areas to background
        *)
        let gc = GCCache.get_fg_bg_lw cache bg bg 1 in

        X.polyFillRectangle dpy win gc [
                x1,y1,cr+1,cr+1;
                x2-cr,y2-cr,cr+1,cr+1;
                x1,y2-cr,cr+1,cr+1;
                x2-cr,y1,cr+1,cr+1;
                x1,y1,(x2-x1+1),1;
            ];

        (*
        ** Erase to fillground
        *)
        let gc = GCCache.get_fg_bg_lw cache fg bg bw in
        let gc' = GCCache.get_fg_bg_lw cache ig ig bw in

        fillRoundedRectangle dpy win gc' x1' y1' (w+1) (h+1) cr cr;
        drawRoundedRectangle dpy win gc x1' y1' w h cr cr;

    | S_Circ ->
        ()

let px xy = let x,y = xy in x
let py xy = let x,y = xy in y


(*
** Shadow boxes. Background is filled in this routine!
*)
let shadow_buffer = ref None

let drawShadow2 dpy win cache fg bg frame =
  let f = frame in
  let fg = if f.f_foreground <> noColor then
                    f.f_foreground.c_pixel 
                 else
                    fg in
  let bg = if f.f_background <> noColor then
                    f.f_background.c_pixel 
                 else
                    bg in
  let ig = f.f_fillground.c_pixel in


  let b = f.f_bbox in
  let x1,y1,x2,y2 = b.x1,b.y1,b.x2,b.y2 in
  let x,y,w,h = bbox_to_xywh b in
  let sw = f.f_size in
  let bw = max 1 (sw / 2) in

  match f.f_type with
  | ShadowRaised ->
  begin    
    match f.f_shape with
    | S_Rect ->
        (*
        ** shadow effect
        *)

        (*
        ** border line width adjustment
        *)

        let dx' = max 0 ((bw-1) / 2) in               
        let dx'' = max 0 (bw / 2) in               
        let dy' = max 0 ((bw-1) / 2) in
        let dy'' = max 0 (bw / 2) in

        let gc = GCCache.get_fg_bg_lw cache fg ig bw in
        let gc' = GCCache.get_fg_bg_lw cache ig ig bw in
        let cc = [x1+dx'',          y1+dy'';
                  x2-sw-dx',        y1+dy'';
                  x2-sw-dx',        y2-sw-dy';
                  x1+dx'',          y2-sw-dy';
                  x1+dx'',          y1+dy''] in

        (*
        ** Fill with fillground
        *)
        X.fillPoly dpy win gc' Origin Complex cc;
        (*
        ** Draw outline
        *)
        X.polyLine dpy win gc Origin cc;
        

        (*
        ** shadow line width adjustment
        *)
        let dx' = max 0 ((sw-1) / 2) in 
        let dx'' = max 0 (sw / 2) in 
        let dy' = max 0 ((sw-1) / 2) in
        let dy'' = max 0 (sw / 2) in

        (*
        ** Fill some undrawn area with sourrounding background
        *)
        let gc = GCCache.get_fg_bg_lw cache bg bg f.f_size in
        X.polyLine dpy win gc Origin
                           [
                            x1,     y2-dy';
                            x2-dx', y2-dy';
                            x2-dx', y1;
                           ];

        (*
        ** Draw the shadow
        *)
        let gc = GCCache.get_fg_bg_lw cache fg ig f.f_size in
        X.polyLine dpy win gc Origin
                           [x1+sw,  y2-dy';
                            x2-dx', y2-dy';
                            x2-dx', y1+sw;
                            ];
    | S_Oval ->

        (*
        ** Only do buffered hidden drawing with small frames (buttons).
        ** This avoids flickering...
        *)
        let win',buffered,x1,y1,x2,y2 = 
            if w < 100 && h < 100 then
            begin
                match !shadow_buffer with
                | Some b -> b,true,0,0,w-1,h-1;
                | None ->
                    let p = X.createPixmap dpy (defaultRoot dpy)
                                           100 100 (defaultDepth dpy) in
                    shadow_buffer := Some p;
                    p,true,0,0,w-1,h-1
            end
            else
                win,false,x1,y1,x2,y2 in

        let d = sw in
        let x1',y1' = x1+d+1,y1+d+1 in
        let x2',y2' = x2-sw+d-1,y2-sw+d-1 in
        let w',h' = x2'-x1'+1,y2'-y1'+1 in
    
        let cr = min (h'/2) 10 in


        (*
        ** Erase first unused areas to background
        *)
        let gc = GCCache.get_fg_bg_lw cache bg bg 1 in

        if not buffered then
            X.polyFillRectangle dpy win' gc [
                x1,y1,cr+1,cr+1;
                x2-cr,y2-cr,cr+1,cr+1;
                x1,y2-cr,cr+1,cr+1;
                x2-cr,y1,cr+1,cr+1;
                x1,y1,(x2-x1+1),1;
            ]
        else
            X.polyFillRectangle dpy win' gc [
                x1,y1,(x2-x1+1),(y2-y1+1);
            ];

        (*
        ** Draw and fill shadow
        *)
        let gc = GCCache.get_fg_bg_lw cache fg ig 1 in


        fillRoundedRectangle dpy win' gc x1' y1' (w'+1) (h'+1) cr cr; 
        drawRoundedRectangle dpy win' gc x1' y1' w' h' cr cr;
        (*
        ** The content frame
        *)

        let gc  = GCCache.get_fg_bg_lw cache fg ig 1 in
        let gc' = GCCache.get_fg_bg_lw cache ig ig 1 in

        (*
        ** border line width adjustment
        *)
        (*
        ** Draw first filled background, then foreground.
        *)
        let d = 0 in

        let x1',y1' = x1+d+1,y1+d+1 in
        let x2',y2' = x2-sw+d-1,y2-sw+d-1 in
        let w',h' = x2'-x1'+1,y2'-y1'+1 in
    
        let cr = min (h'/2) 10 in

        fillRoundedRectangle dpy win' gc' x1' y1' (w'+1) (h'+1) cr cr;
        drawRoundedRectangle dpy win' gc x1' y1' w' h' cr cr;

        if buffered then
            X.copyArea dpy gc win' 0 0 win x y w h;
    | S_Circ ->
        ()
  end;
  | ShadowSunken ->
  begin
    match f.f_shape with
    | S_Rect ->
        (*
        ** shadow effect
        *)
        (*
        ** border line width adjustment
        *)

        let dx' = max 0 ((bw-1) / 2) in               
        let dx'' = max 0 (bw / 2) in               
        let dy' = max 0 ((bw-1) / 2) in
        let dy'' = max 0 (bw / 2) in

        let gc = GCCache.get_fg_bg_lw cache fg ig bw in
        let gc' = GCCache.get_fg_bg_lw cache ig ig bw in
        let cc = [x1+dx''+sw,       y1+dy''+sw;
                  x2-dx',           y1+dy''+sw;
                  x2-dx',           y2-dy';
                  x1+dx''+sw,       y2-dy';
                  x1+dx''+sw,       y1+dy''+sw] in


        (*
        ** Fill with background
        *)
        X.fillPoly dpy win gc' Origin Complex cc;
        (*
        ** Draw outline
        *)
        X.polyLine dpy win gc Origin cc;

        (*
        ** shadow line width adjustment
        *)
        let dx' = max 0 ((sw-1) / 2) in 
        let dx'' = max 0 (sw / 2) in 
        let dy' = max 0 ((sw-1) / 2) in
        let dy'' = max 0 (sw / 2) in

        (*
        ** Fill some undrawn area with parent background
        *)
        let gc = GCCache.get_fg_bg_lw cache bg bg f.f_size in
        X.polyLine dpy win gc Origin
                           [
                            x1+dx'',        y2;
                            x1+dx'',        y1+dy'';
                            x2,          y1+dy'';
                           ];

        let gc = GCCache.get_fg_bg_lw cache fg ig sw in
        X.polyLine dpy win gc Origin
                           [x1+dx'',        y2-sw;
                            x1+dx'',        y1+dy'';
                            x2-sw,          y1+dy'';
                            ];
    | S_Oval ->

        (*
        ** Only do buffered hidden drawing with small frames (buttons).
        ** This avoids flickering...
        *)
        let win',buffered,x1,y1,x2,y2 = 
            if w < 100 && h < 100 then
            begin
                match !shadow_buffer with
                | Some b -> b,true,0,0,w-1,h-1;
                | None ->
                    let p = X.createPixmap dpy (defaultRoot dpy)
                                           100 100 (defaultDepth dpy) in
                    shadow_buffer := Some p;
                    p,true,0,0,w-1,h-1
            end
            else
                win,false,x1,y1,x2,y2 in

        let d = 0 in
        let x1',y1' = x1+d+1,y1+d+1 in
        let x2',y2' = x2-sw+d-1,y2-sw+d-1 in
        let w',h' = x2'-x1'+1,y2'-y1'+1 in
    
        let cr = min (h'/2) 10 in


        (*
        ** Erase first unused areas to background
        *)
        let gc = GCCache.get_fg_bg_lw cache bg bg 1 in

        if not buffered then
            X.polyFillRectangle dpy win' gc [
                x1,y1,cr+1,cr+1;
                x2-cr,y2-cr,cr+1,cr+1;
                x1,y2-cr,cr+1,cr+1;
                x2-cr,y1,cr+1,cr+1;
                x1,y1,(x2-x1+1),1;
            ]
        else
            X.polyFillRectangle dpy win' gc [
                x,y,w,h;
            ];


        (*
        ** Draw and fill shadow
        *)
        let gc = GCCache.get_fg_bg_lw cache fg ig 1 in

        fillRoundedRectangle dpy win' gc x1' y1' (w'+1) (h'+1) cr cr;
        drawRoundedRectangle dpy win' gc x1' y1' w' h' cr cr;

        (*
        ** The content frame
        *)

        let gc  = GCCache.get_fg_bg_lw cache fg ig 1 in
        let gc' = GCCache.get_fg_bg_lw cache ig ig 1 in

        (*
        ** border line width adjustment
        *)
        (*
        ** Draw first filled background, then foreground.
        *)
        let d = sw in

        let x1',y1' = x1+d+1,y1+d+1 in
        let x2',y2' = x2-sw+d-1,y2-sw+d-1 in
        let w',h' = x2'-x1'+1,y2'-y1'+1 in
    
        let cr = min (h'/2) 10 in

        fillRoundedRectangle dpy win' gc' x1' y1' (w'+1) (h'+1) cr cr;
        drawRoundedRectangle dpy win' gc x1' y1' w' h' cr cr;

        if buffered then
            X.copyArea dpy gc win' 0 0 win x y w h;

    | S_Circ ->
        ()
  end;
  | _ -> ()   (* Nothing to do *)

(*
** Relief boxes. Background must be filled externally! Optionally a black
** border is drawn.
*)
let drawRelief2 dpy win cache fg bg frame =
  let f = frame in
  
  let hilite = f.f_auxiliary.c_pixel in
  let shadow = f.f_foreground.c_pixel in 
  let fill = f.f_fillground.c_pixel in

  let x,y,w,h = bbox_to_xywh f.f_bbox in

  let black = f.f_black.c_pixel in
  let black_border = 1 in

  match f.f_type with
  | ReliefRaised ->
  begin
      let gc = GCCache.get_fg cache hilite in
      let gc' = GCCache.get_fg cache shadow in
      let gc'' = GCCache.get_fg cache black in
      for i = 0 to f.f_size-1 do
        let x = x + i + black_border in
        let y = y + i + black_border in
        let w = w -i-i - 2*black_border in
        let h = h -i-i - 2*black_border in
        X.polyLine dpy win gc Origin [(w+x-1),y; x,y; x, (h+y-1)];
      done;
      for i = 0 to f.f_size-1 do
        let x = x + i + black_border in
        let y = y + i + black_border in
        let w = w -i-i - 2*black_border in
        let h = h -i-i - 2*black_border in
        X.polyLine dpy win gc' Origin [(w+x-1),y; (w+x-1),(h+y-1); x,(h+y-1)]
      done;
      if black_border > 0 then
      X.polyLine dpy win gc'' Origin [(w+x-1),y; (w+x-1),(h+y-1);x,(h+y-1);
                                      x, (h+y-1); x,y; (w+x-1),y;]
      
  end;
  | ReliefSunken ->
  begin
      let gc = GCCache.get_fg cache shadow in
      let gc' = GCCache.get_fg cache hilite in
      let gc'' = GCCache.get_fg cache black in
      for i = 0 to f.f_size-1 do
        let x = x + i + black_border in
        let y = y + i + black_border in
        let w = w -i-i - 2*black_border in
        let h = h -i-i - 2*black_border in
        X.polyLine dpy win gc Origin [(w+x-1),y; x,y; x, (h+y-1)];
      done;
      for i = 0 to f.f_size-1 do
        let x = x + i + black_border in
        let y = y + i + black_border in
        let w = w -i-i - 2*black_border in
        let h = h -i-i - 2*black_border in
        X.polyLine dpy win gc' Origin [(w+x-1),y; (w+x-1),(h+y-1); x,(h+y-1)]
      done;
      if black_border > 0 then
      X.polyLine dpy win gc'' Origin [(w+x-1),y; (w+x-1),(h+y-1);x,(h+y-1);
                                      x, (h+y-1); x,y; (w+x-1),y;]
  end;
  | _ -> ()


(*
** Main entry for frame drawing.
**
**  Flat (solid line) border: no background is filled
**  Shadow border: background is filled
**  Relief: no background is filled 
**
**  fill: if true, in all three cases the frame background is filled
**        with f_fillground color.
*)



let drawFrame dpy win cache frame fill =
    let g = win.w_geometry in
    let fg = win.w_foreground.c_pixel in
    let bg = win.w_background.c_pixel in
    let x,y,w,h = bbox_to_xywh frame.f_bbox in
    
    match frame.f_type with
    | ShadowRaised
    | ShadowSunken -> 
        drawShadow2 dpy win.w_window cache fg bg frame;
    | ReliefRaised
    | ReliefSunken ->
        if fill then
        begin
            let gc = GCCache.get_fg cache frame.f_fillground.c_pixel in
            X.polyFillRectangle dpy win.w_window gc [x,y,w,h];
        end;        
        drawRelief2 dpy win.w_window cache fg bg frame;
    | Flat ->
        if fill && frame.f_shape = S_Rect then
        begin
            let x,y,w,h = bbox_to_xywh frame.f_bbox in
            let gc = GCCache.get_fg cache frame.f_fillground.c_pixel in
            X.polyFillRectangle dpy win.w_window gc [x,y,w,h];
        end;        
        drawBorder2 dpy win.w_window cache fg bg frame;
    | Plain -> 
        if fill && frame.f_shape = S_Rect then
        begin
            let x,y,w,h = bbox_to_xywh frame.f_bbox in
            let gc = GCCache.get_fg cache frame.f_fillground.c_pixel in
            X.polyFillRectangle dpy win.w_window gc [x,y,w,h];
        end        

(*
** Return total frame size (sum for both sides x/y)
*)
let frame_size frame =
    let f = frame in
    match f.f_type with
    | ShadowRaised
    | ShadowSunken -> 
        let sw = f.f_size in
        let bw = max 1 (sw/2) in
        2*bw+sw
    | ReliefRaised
    | ReliefSunken ->
        2*(f.f_size+1)
    | Flat ->
        let l = f.f_size + (f.f_depth-1)* f.f_size +
                           (2*(f.f_depth-1)) * f.f_size in
        l*2
    | _ -> 0

(*
** Return offset (x & y) of usable area inside depending on current 
** frame type
*)
let frame_offset frame =
    let f = frame in
    match f.f_type with
    | ShadowRaised ->
        let sw = f.f_size in
        let bw = max 1 (sw/2) in
        bw        
    | ShadowSunken -> 
        let sw = f.f_size in
        let bw = max 1 (sw/2) in
        bw+sw
    | ReliefRaised
    | ReliefSunken ->
        f.f_size+1
    | Flat ->
        let l = f.f_size + (f.f_depth-1)* f.f_size +
                           (2*(f.f_depth-1)) * f.f_size in
        l
    | _ -> 0


(*
** Postscript printing
*)

(*
** Print (PS) border (x0,y0: absolute screen origin of this window)
*)
let printBorder2 ps x0 y0 fc bc frame =
    let f = frame in
    let b = f.f_bbox in
    let lw = i2f f.f_size in
    let lw2 = lw/2.0 in

    let fc = if f.f_foreground <> noColor then
                    f.f_foreground
                 else
                    fc in
    let bc = if f.f_background <> noColor then
                    f.f_background
                 else
                    bc in
    let ic = f.f_fillground in

    let x,y,w,h = bbox_to_xywh b in
    let x1,y1,x2,y2 = i2f b.x1, i2f b.y1, i2f b.x2, i2f b.y2 in
    let x,y,w,h = i2f x, i2f y, i2f w, i2f h in

    let x,y,dx,dy = ref x,
                    ref y,
                    ref w,
                    ref h in

    match f.f_shape with
    | S_Rect ->
        for i = 1 to f.f_depth 
        do
            if f.f_sides = [] then      (* draw all border sides *)
                VX_ps.polyLine ps x0 y0 lw fc [
                    !x+lw2,             !y+lw2;
                    !x+ !dx-lw2,        !y+lw2;
                    !x+ !dx-lw2,        !y+ !dy-lw2;
                    !x+lw2,             !y+ !dy-lw2;
                    !x+lw2,             !y+lw2;
                ]
            else
            begin
                (*
                ** Draw only selected border sides
                *)
                List.iter (fun s ->
                    match s with
                    | B_top ->
                        VX_ps.polyLine ps x0 y0 lw fc [
                                !x,          !y+lw2;
                                !x+ !dx,     !y+lw2;
                            ];
                    | B_bottom ->
                        VX_ps.polyLine ps x0 y0 lw fc [
                                !x,          !y+ !dy-lw2;
                                !x+ !dx,     !y+ !dy-lw2;
                            ];
                    | B_left ->
                        VX_ps.polyLine ps x0 y0 lw fc [
                                !x+lw2,      !y;
                                !x+lw2,      !y+ !dy;
                            ];
                    | B_right ->
                        VX_ps.polyLine ps x0 y0 lw fc [
                                !x+ !dx-lw2, !y;
                                !x+ !dx-lw2, !y+ !dy;
                            ];
                    ) f.f_sides;
            end;
            x := !x + (2.0 * lw);
            y := !y + (2.0 * lw);
            dx := !dx - (4.0 * lw);
            dy := !dy - (4.0 * lw);
        done;

    | S_Oval ->
        let bw = lw in
        let d = 0.0 in
        let dx' = bw / 2.0 in 
        let dy' = dx' in 

        let x1' = x1 + d in
        let y1' = y1 + d in
        let x2' = x2 + d - dx' in
        let y2' = y2 + d - dy' in
        let w = x2' - x1' in
        let h = y2' - y1' in
    
        let cr = fmin (h/2.0) 10.0 in

        VX_ps.polyFillRectangle ps x0 y0 bc [
                x1,y1,cr+1.,cr+1.;
                x2-cr,y2-cr,cr+1.,cr+1.;
                x1,y2-cr,cr+1.,cr+1.;
                x2-cr,y1,cr+1.,cr+1.;
                x1,y1,(x2-x1+1.),1.;
            ];
        printFilledRoundedRectangle ps x0 y0 ic x1'
                                         y1'
                                         w
                                         h 
                                         cr cr;

        printRoundedRectangle ps x0 y0 fc bw x1'
                                             y1'
                                             w
                                             h 
                                             cr cr;

    | S_Circ ->
        ()



(*
** Print a shadow
*)

let printShadow2 ps x0 y0 fc bc frame =
  let f = frame in
  let b = f.f_bbox in
  let x1,y1,x2,y2 = i2f b.x1, i2f b.y1, 
                    i2f b.x2, i2f b.y2 in
  let x,y,w,h = bbox_to_xywh b in
  let x,y,w,h = i2f x, i2f y, i2f w, i2f h in

  let sw = i2f f.f_size in
  let bw = sw / 2.0 in

  let fc = if f.f_foreground <> noColor then
                    f.f_foreground
                 else
                    fc in
  let bc = if f.f_background <> noColor then
                    f.f_background
                 else
                    bc in
  let ic = if f.f_fillground <> noColor then
                    f.f_fillground
                 else
                    bc in

  match f.f_type with
  | ShadowRaised ->
  begin    
    match f.f_shape with
    | S_Rect ->
        (*
        ** shadow effect
        *)
        (*
        ** border line width adjustment
        *)
        

        let dx' = bw /. 2.0 in
        let dy' = dx' in
        let dx'' = dx' in
        let dy'' = dy' in


        let cc = [x1 +. dx'',           y1 +. dy'';
                  x2 -. sw -. dx',      y1 +. dy'';
                  x2 -. sw -. dx',      y2 -. sw -. dy';
                  x1 +. dx'',           y2 -. sw -. dy';
                  x1 +. dx'',           y1 +. dy''] in

        (*
        ** Draw border and fill with fill background.
        *)
        VX_ps.fillPoly ps x0 y0 ic cc;
        VX_ps.polyLine ps x0 y0 bw fc cc;


        (*
        ** shadow line width adjustment
        *)

        let dx' = sw /. 2.0 in 
        let dx'' = dx' in 
        let dy' = sw /. 2.0 in
        let dy'' = dy' in

        (*
        ** Fill with background
        *)
        VX_ps.polyLine ps x0 y0 sw bc
                           [x1,        y2 -. dy';
                            x2 -. dx', y2 -. dy';
                            x2 -. dx', y1;
                            ];
        (*
        ** print shadow
        *)
        VX_ps.polyLine ps x0 y0 sw fc
                           [x1 +. sw,  y2 -. dy';
                            x2 -. dx', y2 -. dy';
                            x2 -. dx', y1 +. sw;
                            ];
    | S_Oval ->

        let d = sw in

        let x1',y1' = x1+d+1.,y1+d+1. in
        let x2',y2' = x2-sw+d-1.,y2-sw+d-1. in
        let w',h' = x2'-x1'+1.,y2'-y1'+1. in
    
        let cr = fmin (h'/2.0) 10.0 in


        (*
        ** Erase first unused areas to background
        *)
(*
        VX_ps.polyFillRectangle ps x0 y0 bc [
                x1,y1,cr+1.0,cr+1.0;
                x2-cr,y2-cr,cr+1.0,cr+1.0;
                x1,y2-cr,cr+1.0,cr+1.0;
                x2-cr,y1,cr+1.0,cr+1.0;
            ];
*)

        printFilledRoundedRectangle ps x0 y0 fc x1'
                                             y1'
                                             (w'+1.0)
                                             (h'+1.0) 
                                             cr cr;
        printRoundedRectangle ps x0 y0 fc 1.0 x1'
                                             y1'
                                             w'
                                             h' 
                                             cr cr;


        let d = 0.0 in

        let x1',y1' = x1+d+1.,y1+d+1. in
        let x2',y2' = x2-sw+d-1.,y2-sw+d-1. in
        let w',h' = x2'-x1'+1.,y2'-y1'+1. in
    
        let cr = fmin (h'/2.0) 10.0 in


        printFilledRoundedRectangle ps x0 y0 ic x1'
                                             y1'
                                             (w'+0.5)
                                             (h'+0.5) 
                                             cr cr;

        printRoundedRectangle ps x0 y0 fc bw x1'
                                             y1'
                                             w'
                                             h' 
                                             cr cr;
    | S_Circ ->
        ()
  end;
  | ShadowSunken ->
  begin
    match f.f_shape with
    | S_Rect ->
        (*
        ** shadow effect
        *)
        (*
        ** border line width adjustment
        *)

        let dx' = bw / 2.0 in
        let dy' = dx' in
        let dx'' = dx' in
        let dy'' = dy' in



        let cc = [x1+dx''+sw,       y1+dy''+sw;
                  x2-dx',           y1+dy''+sw;
                  x2-dx',           y2-dy';
                  x1+dx''+sw,       y2-dy';
                  x1+dx''+sw,       y1+dy''+sw] in

        (*
        ** Draw border and fill with fillground.
        *)
        VX_ps.fillPoly ps x0 y0 ic cc;
        VX_ps.polyLine ps x0 y0 bw fc cc;

        (*
        ** shadow line width adjustment
        *)
        let dx' = sw / 2.0 in
        let dy' = dx' in
        let dx'' = dx' in
        let dy'' = dy' in

        (*
        ** Fill with background
        *)
        VX_ps.polyLine ps x0 y0 sw bc
                           [x1+dx'',        y2;
                            x1+dx'',        y1+dy'';
                            x2,             y1+dy'';
                            ];

        (*
        ** Draw the shadow
        *)
        VX_ps.polyLine ps x0 y0 sw fc
                           [x1+dx'',        y2-sw;
                            x1+dx'',        y1+dy'';
                            x2-sw,          y1+dy'';
                            ];
    | S_Oval ->

        let d = 0.0 in

        let x1',y1' = x1+d+1.,y1+d+1. in
        let x2',y2' = x2-sw+d-1.,y2-sw+d-1. in
        let w',h' = x2'-x1'+1.,y2'-y1'+1. in
    
        let cr = fmin (h'/2.0) 10.0 in


        (*
        ** Erase first unused areas to background
        *)
(*
        VX_ps.polyFillRectangle ps x0 y0 bc [
                x1,y1,cr+1.0,cr+1.0;
                x2-cr,y2-cr,cr+1.0,cr+1.0;
                x1,y2-cr,cr+1.0,cr+1.0;
                x2-cr,y1,cr+1.0,cr+1.0;
            ];
*)

        printFilledRoundedRectangle ps x0 y0 fc x1'
                                             y1'
                                             (w'+1.0)
                                             (h'+1.0) 
                                             cr cr;
        printRoundedRectangle ps x0 y0 fc 1.0 x1'
                                             y1'
                                             w'
                                             h' 
                                             cr cr;


        let d = sw in

        let x1',y1' = x1+d+1.,y1+d+1. in
        let x2',y2' = x2-sw+d-1.,y2-sw+d-1. in
        let w',h' = x2'-x1'+1.,y2'-y1'+1. in
    
        let cr = fmin (h'/2.0) 10.0 in


        printFilledRoundedRectangle ps x0 y0 ic x1'
                                             y1'
                                             (w'+0.5)
                                             (h'+0.5) 
                                             cr cr;

        printRoundedRectangle ps x0 y0 fc bw x1'
                                             y1'
                                             w'
                                             h' 
                                             cr cr;
    | S_Circ ->
        ()
  end;
  | _ -> ()   (* Nothing to do *)

(*
** Print a relief frame
*)
let printRelief2 ps x0 y0 fc bc frame =
  let f = frame in
  
  let hilite = f.f_auxiliary in
  let shadow = f.f_foreground in 
  let fill = f.f_fillground in
  let black = f.f_black in

  let x,y,w,h = bbox_to_xywh f.f_bbox in
  let x,y,w,h = i2f x, i2f y, i2f w, i2f h in

  let black_border = 1.0 in

  match f.f_type with
  | ReliefRaised ->
  begin
      for i = 0 to f.f_size-1 do
        let i = i2f i in
        let x = x + i + black_border in
        let y = y + i + black_border in
        let w = w -i-i - 2.0*black_border in
        let h = h -i-i - 2.0*black_border in
        VX_ps.polyLine ps x0 y0 1.0 hilite [(w+x-1.0),y; 
                                            x,y; 
                                            x, (h+y-1.0)];
      done;
      for i = 0 to f.f_size-1 do
        let i = i2f i in
        let x = x + i + black_border in
        let y = y + i + black_border in
        let w = w -i-i - 2.0*black_border in
        let h = h -i-i - 2.0*black_border in
        VX_ps.polyLine ps x0 y0 1.0 shadow [(w+x-1.0),y; 
                                            (w+x-1.0),(h+y-1.0); 
                                            x,(h+y-1.0)];
      done;
      if black_border > 0.0 then
      VX_ps.polyLine ps x0 y0 1.0 black [(w+x-1.0),y; 
                                      (w+x-1.0),(h+y-1.0);
                                      x,(h+y-1.0);
                                      x, (h+y-1.0); 
                                      x,y; (w+x-1.0),y;]

  end;
  | ReliefSunken ->
  begin
      for i = 0 to f.f_size-1 do
        let i = i2f i in
        let x = x + i + black_border in
        let y = y + i + black_border in
        let w = w -i-i - 2.0*black_border in
        let h = h -i-i - 2.0*black_border in
        VX_ps.polyLine ps x0 y0 1.0 shadow [(w+x-1.0),y; 
                                            x,y; 
                                            x, (h+y-1.0)];
      done;
      for i = 0 to f.f_size-1 do
        let i = i2f i in
        let x = x + i + black_border in
        let y = y + i + black_border in
        let w = w -i-i - 2.0*black_border in
        let h = h -i-i - 2.0*black_border in
        VX_ps.polyLine ps x0 y0 1.0 hilite [(w+x-1.0),y; 
                                            (w+x-1.0),(h+y-1.0); 
                                            x,(h+y-1.0)]
      done;
      if black_border > 0.0 then
      VX_ps.polyLine ps x0 y0 1.0 black [(w+x-1.0),y; 
                                      (w+x-1.0),(h+y-1.0);
                                      x,(h+y-1.0);
                                      x, (h+y-1.0); 
                                      x,y; (w+x-1.0),y;]

  end;
  | _ -> ()



(*
** Main entry for frame printing
**
**  Flat (solid line) border: no background is filled
**  Shadow border: background is filled
**  Relief: no background is filled 
**
**  fill: if true, in all three cases the frame background is filled
**        with f_fillground color.
*)
let printFrame ps win x0 y0 frame fill =
    let g = win.w_geometry in
    let fc = win.w_foreground in
    let bc =  win.w_background in

    match frame.f_type with
    | ShadowRaised
    | ShadowSunken -> 
        printShadow2 ps x0 y0 fc bc frame;
    | ReliefRaised
    | ReliefSunken ->
        if fill then
        begin
            let x,y,w,h = bbox_to_xywh frame.f_bbox in
            VX_ps.polyFillRectangle ps x0 y0 frame.f_fillground 
                                             [i2f x,
                                              i2f y,
                                              i2f w,
                                              i2f h];
        end;
        printRelief2 ps x0 y0 fc bc frame;
    | Flat ->
        if fill && frame.f_shape = S_Rect then
        begin
            let x,y,w,h = bbox_to_xywh frame.f_bbox in
            VX_ps.polyFillRectangle ps x0 y0 frame.f_fillground
                                             [i2f x,
                                              i2f y,
                                              i2f w,
                                              i2f h];
        end;
        printBorder2 ps x0 y0 fc bc frame;
    | _ -> 
        if fill && frame.f_shape = S_Rect then
        begin
            let x,y,w,h = bbox_to_xywh frame.f_bbox in
            VX_ps.polyFillRectangle ps x0 y0 frame.f_fillground
                                             [i2f x,
                                              i2f y,
                                              i2f w,
                                              i2f h];
        end


(*
** Generic symbols. Simple symbols like +,-,! are  generated by
** vector commands, other more complicated like ? with text fonts.
*)

(*
** Symbol font management
*)
let sym_fonts = Hashtbl.create 10 
let sym_font_make display text_font text_style text_size =
    let font_name = font_name text_font text_style text_size in
    try
        Hashtbl.find sym_fonts font_name
    with
    | Not_found ->
        let font = X.openFont display font_name in
        let qf = X.queryFont display font in
        let maxb = qf.qf_info.font_max_bounds in
        let font = { 
                (*
                ** Currently unknown!
                *)
                font_class = text_font;
                font_style = text_style;
                font_size = text_size;
                font_name = font_name;
                font_id = font; font_info = qf;
                font_ascent = maxb.char_ascent;
                font_height = maxb.char_ascent + maxb.char_descent;
                font_width = maxb.char_width;
                } in

        Hashtbl.add sym_fonts font_name font;
        font
                

let drawSymbol2 dpy win cache fg bg symbol =
    let bbox = symbol.sym_bbox in
    let x1,y1,x2,y2 = bbox.x1,bbox.y1,bbox.x2,bbox.y2 in
    let w = x2-x1+1 in
    let h = y2-y1+1 in
    let xc,yc = x1 + w/2,
                y1 + h/2 in
    
        
    (*
    ** Scaling
    *)
    let f2i = int_of_float in
    let i2f = float_of_int in
    let wf = i2f w in
    let hf = i2f h in
    let kx = 100. /. wf in
    let ky = 100. /.  hf in

    (*
    ** Up and down rounded versions of scaling functions
    *)
    let round y =
        let dy = abs_float (y -. (floor y)) in
        if dy < 0.5 then 
            f2i (floor y)
        else
            f2i (ceil y)
        in
    let wk w = round ((i2f w) /. ky) in
    let px x = x1 + (round ((i2f x) /. kx))  in
    let py y = y1 + (round ((i2f y) /. ky))  in 


    match symbol.sym_type with
    | S_OK ->
    begin
        let fg = if symbol.sym_col <> noColor then
                    symbol.sym_col.c_pixel 
                 else
                    fg in
        let bw = symbol.sym_width in
        let gc  = GCCache.get_fg_bg_lw cache fg bg bw in        

        let cc = [px 25,py 45;
                                      px 20,py 60;
                                      px 40,py 90;
                                      px 90,py 0;
                                      px 40,py 65;
                                      px 25,py 45] in

        X.fillPoly dpy win gc Origin Complex cc;
        X.polyLine dpy win gc Origin cc;
                                        
    end
    | S_ENTER ->
    begin
        let fg = if symbol.sym_col <> noColor then
                    symbol.sym_col.c_pixel 
                 else
                    fg in
        let bw = symbol.sym_width in
        let gc  = GCCache.get_fg_bg_lw cache fg bg bw in        

        X.polyLine dpy win gc Origin [px 80,py 20;
                                      px 80,py 50;
                                      px 25,py 50];

        let cc = [px 40,py 20;
                                      px 40,py 80;
                                      px 20,py 50;
                                      px 40,py 20] in
        X.fillPoly dpy win gc Origin Complex cc;
        X.polyLine dpy win gc Origin cc;
                                        
        
    end
    | S_ERR ->
    begin
        let fg = if symbol.sym_col <> noColor then
                    symbol.sym_col.c_pixel 
                 else
                    fg in
        let bw = symbol.sym_width in
        let gc  = GCCache.get_fg_bg_lw cache fg bg bw in        


        let cc = [px 30,py 10;
                  px 70,py 10;
                  px 50,py 60;
                  px 30,py 10] in
        X.fillPoly dpy win gc Origin Complex cc;
        X.polyLine dpy win gc Origin cc;
                                        
        let r = 10 in
        let r' = wk r in

        let ac = [(px (50-r)),(py (70-r)),(2*r'),(2*r'),
                                  0,(360*64)] in
        X.polyFillArc dpy win gc ac;
        X.polyArc dpy win gc ac;

    end
    | S_WARN ->
    begin
        let fg = if symbol.sym_col <> noColor then
                    symbol.sym_col.c_pixel 
                 else
                    fg in
        let bw = symbol.sym_width in
        let gc  = GCCache.get_fg_bg_lw cache fg bg bw in        


        X.polyLine dpy win gc Origin 
                                     [px 30,py 10;
                                      px 70,py 10;
                                      px 50,py 50;
                                      px 30,py 10];
                                        
        let r = 10 in
        let r' = wk r in
        X.polyArc dpy win gc [(px (50-r)),(py (80-r)),(2*r'),(2*r'),
                                  0,(360*64)];
    end
    | S_QUEST ->
    begin
        let fg = if symbol.sym_col <> noColor then
                    symbol.sym_col.c_pixel 
                 else
                    fg in
        let bw = symbol.sym_width in
        let font = 
                        match (w-3) with
                        | s when s <= 8 -> 
                            sym_font_make dpy Helvetica Bold 8;
                        | s when s > 8 && s <= 10 ->
                            sym_font_make dpy Helvetica Bold 10;
                        | s when s > 10 && s <= 12 ->
                            sym_font_make dpy Helvetica Bold 12;
                        | s when s > 12 && s <= 14 ->
                            sym_font_make dpy Helvetica Bold 14;
                        | s when s > 14 && s <= 18 ->
                            sym_font_make dpy Helvetica Bold 18;
                        | s when s > 18 && s <= 20 ->
                            sym_font_make dpy Helvetica Bold 20;
                        | _ -> 
                            sym_font_make dpy Helvetica Bold 24;
                    in
        let gc  = GCCache.get_fg_bg_font cache fg bg font.font_id in        

        let sym = "?" in
        let sw = Xtext.width font.font_info sym in
        let sh = font.font_height in
        let x0,y0 =
            x1 + (w-sw)/2,
            y1 + font.font_ascent + (h-sh)/2 in

        Xlib.drawSubString dpy win gc  
                           x0 y0 sym 0 1;
    end
    | S_CROSS ->
    begin
        let fg = if symbol.sym_col <> noColor then
                    symbol.sym_col.c_pixel 
                 else
                    fg in
        let bw = symbol.sym_width in
        let font = 
                        match (w-3) with
                        | s when s <= 8 -> 
                            sym_font_make dpy Helvetica Bold 8;
                        | s when s > 8 && s <= 10 ->
                            sym_font_make dpy Helvetica Bold 10;
                        | s when s > 10 && s <= 12 ->
                            sym_font_make dpy Helvetica Bold 12;
                        | s when s > 12 && s <= 14 ->
                            sym_font_make dpy Helvetica Bold 14;
                        | s when s > 14 && s <= 18 ->
                            sym_font_make dpy Helvetica Bold 18;
                        | s when s > 18 && s <= 20 ->
                            sym_font_make dpy Helvetica Bold 20;
                        | _ -> 
                            sym_font_make dpy Helvetica Bold 24;
                    in
        let gc  = GCCache.get_fg_bg_font cache fg bg font.font_id in        

        let sym = "X" in
        let sw = Xtext.width font.font_info sym in
        let sh = font.font_height in
        let x0,y0 =
            x1 + (w-sw)/2,
            y1 + font.font_ascent + (h-sh)/2 in

        Xlib.drawSubString dpy win gc  
                           x0 y0 sym 0 1;
                                        
    end;
    | S_0 ->
    begin
        let fg = if symbol.sym_col <> noColor then
                    symbol.sym_col.c_pixel 
                 else
                    fg in
        let bw = symbol.sym_width in
        let gc  = GCCache.get_fg_bg_lw cache fg bg bw in        

        
        let r = w/2 - w/5 in
        X.polyArc dpy win gc [xc-r,yc-r,(2*r),(2*r),
                               0,(360*64)];
    end;
    | S_1 ->
    begin
        let fg = if symbol.sym_col <> noColor then
                    symbol.sym_col.c_pixel 
                 else
                    fg in
        let bw = symbol.sym_width in
        let gc  = GCCache.get_fg_bg_lw cache fg bg bw in        

        let d = w/2 - w/5 in

        X.polyLine dpy win gc Origin [xc,(yc-d);
                                      xc,(yc+d)];
    end;
    | S_MINUS ->
    begin
        let fg = if symbol.sym_col <> noColor then
                    symbol.sym_col.c_pixel 
                 else
                    fg in
        let bw = symbol.sym_width in
        let gc  = GCCache.get_fg_bg_lw cache fg bg bw in        

        let d = w/2 - w/5 in

        X.polyLine dpy win gc Origin [xc+d,yc;
                                      xc-d+1,yc];
    end;
    | S_PLUS ->
    begin
        let fg = if symbol.sym_col <> noColor then
                    symbol.sym_col.c_pixel 
                 else
                    fg in
        let bw = symbol.sym_width in
        let gc  = GCCache.get_fg_bg_lw cache fg bg bw in        

        let d = w/2 - w/5 in

        X.polyLine dpy win gc Origin [xc+d,yc;
                                      xc-d+1,yc];
        X.polyLine dpy win gc Origin [xc,yc+d;
                                      xc,yc-d+1];

    end;
    | S_R d -> 
    begin
        (*
        ** Draw a filled rectangular area with w=bbox.w-d, h=bbox.h-d
        *)
        let fg = if symbol.sym_col <> noColor then
                    symbol.sym_col.c_pixel 
                 else
                    fg in
        let bw = 1 in
        let gc  = GCCache.get_fg_bg_lw cache fg bg bw in        
        let x1',y1',x2',y2'=x1+d,
                            y1+d,
                            x2-d,
                            y2-d in
        let cc = [x1',y1';
                  x2',y1';
                  x2',y2';
                  x1',y2';
                  x1',y1'] in

        X.fillPoly dpy win gc Origin Complex cc;
        X.polyLine dpy win gc Origin cc;
         
    end;
    | S_C d -> ()
    | S_LOCK -> ()

    | S_UP -> 
    begin
        let d = 2 in
        (*
        ** Draw a filled triangle area with w=bbox.w-d, h=bbox.h-d
        *)
        let fg = if symbol.sym_col <> noColor then
                    symbol.sym_col.c_pixel 
                 else
                    fg in
        let bw = 1 in
        let gc  = GCCache.get_fg_bg_lw cache fg bg bw in        
        let px1,py1,px2,py2,px3,py3 =
                    x1+d,y2-d,
                    x2-d,y2-d,
                    xc,y1+d in
                
        let cc = [
                    px1,py1;
                    px2,py2;
                    px3,py3;
                    px1,py1] in

        X.fillPoly dpy win gc Origin Complex cc;
        X.polyLine dpy win gc Origin cc;
    end;
    | S_DOWN -> 
    begin
        let d = 2 in
        (*
        ** Draw a filled triangle area with w=bbox.w-d, h=bbox.h-d
        *)
        let fg = if symbol.sym_col <> noColor then
                    symbol.sym_col.c_pixel 
                 else
                    fg in
        let bw = 1 in
        let gc  = GCCache.get_fg_bg_lw cache fg bg bw in        
        let px1,py1,px2,py2,px3,py3 =
                    x1+d,y1+d,
                    x2-d,y1+d,
                    xc,y2-d in
                
        let cc = [
                    px1,py1;
                    px2,py2;
                    px3,py3;
                    px1,py1] in

        X.fillPoly dpy win gc Origin Complex cc;
        X.polyLine dpy win gc Origin cc;
    end;     
    | S_LEFT -> 
    begin
        let d = 2 in
        (*
        ** Draw a filled triangle area with w=bbox.w-d, h=bbox.h-d
        *)
        let fg = if symbol.sym_col <> noColor then
                    symbol.sym_col.c_pixel 
                 else
                    fg in
        let bw = 1 in
        let gc  = GCCache.get_fg_bg_lw cache fg bg bw in        
        let px1,py1,px2,py2,px3,py3 =
                    x1+d+1,yc,
                    x2-d,y1+d,
                    x2-d,y2-d in
                
        let cc = [
                    px1,py1;
                    px2,py2;
                    px3,py3;
                    px1,py1] in

        X.fillPoly dpy win gc Origin Complex cc; 
        X.polyLine dpy win gc Origin cc; 
    end;
    | S_RIGHT -> 
    begin
        let d = 2 in
        (*
        ** Draw a filled triangle area with w=bbox.w-d, h=bbox.h-d
        *)
        let fg = if symbol.sym_col <> noColor then
                    symbol.sym_col.c_pixel 
                 else
                    fg in
        let bw = 1 in
        let gc  = GCCache.get_fg_bg_lw cache fg bg bw in        
        let px1,py1,px2,py2,px3,py3 =
                    x1+d,y1+d,
                    x2-d-1,yc,
                    x1+d,y2-d in
                
        let cc = [
                    px1,py1;
                    px2,py2;
                    px3,py3;
                    px1,py1] in

        X.fillPoly dpy win gc Origin Complex cc;
        X.polyLine dpy win gc Origin cc;
    end;
    | S_BUSY -> ()
    | S_BLANK -> ()
    
let drawSymbol dpy win cache symbol =
    let fg = win.w_foreground.c_pixel in
    let bg = win.w_background.c_pixel in
    drawSymbol2 dpy win.w_window cache fg bg symbol


(*
** Postscript version
*)
let printSymbol2 ps dpy wx0 wy0 fc bc symbol =
    let bbox = symbol.sym_bbox in
    let x1,y1,x2,y2 = i2f bbox.x1,i2f bbox.y1,i2f bbox.x2,i2f bbox.y2 in
    let w = x2-x1+1.0 in
    let h = y2-y1+1.0 in
    let xc,yc = x1 + w/2.0,
                y1 + h/2.0 in
    
        
    (*
    ** Scaling
    *)
    let wf = w in
    let hf = h in
    let kx = 100.0 / wf in
    let ky = 100.0 /  hf in

    (*
    ** Up and down rounded versions of scaling functions
    *)
    let round y =
        let dy = abs_float (y - (floor y)) in
        if dy < 0.5 then 
            (floor y)
        else
            (ceil y)
        in
    let wk w = (w /. ky) in
    let px x = x1 + (((x) / kx))  in
    let py y = y1 + (((y) / ky))  in 


    match symbol.sym_type with
    | S_OK ->
    begin
        let fc = if symbol.sym_col <> noColor then
                    symbol.sym_col 
                 else
                    fc in

        VX_ps.fillPoly ps wx0 wy0 fc 
                                     [px 25.0,py 45.0;
                                      px 20.0,py 60.0;
                                      px 40.0,py 90.0;
                                      px 90.0,py 0.0;
                                      px 40.0,py 65.0;
                                      px 25.0,py 45.0];
                                        
    end
    | S_ENTER ->
    begin
        let fc = if symbol.sym_col <> noColor then
                    symbol.sym_col 
                 else
                    fc in

        VX_ps.fillPoly ps wx0 wy0 fc [px 80.0,py 20.0;
                                      px 80.0,py 50.0;
                                      px 25.0,py 50.0];

        VX_ps.fillPoly ps wx0 wy0 fc
                                     [px 40.,py 20.;
                                      px 40.,py 80.;
                                      px 20.,py 50.;
                                      px 40.,py 20.];
                                        
        
    end
    | S_ERR ->
    begin
        let fc = if symbol.sym_col <> noColor then
                    symbol.sym_col
                 else
                    fc in


        VX_ps.fillPoly ps wx0 wy0 fc 
                                     [px 30.,py 10.;
                                      px 70.,py 10.;
                                      px 50.,py 60.;
                                      px 30.,py 10.];
                                        
        let r = 10. in
        let r' = wk r in

        VX_ps.polyFillArc ps wx0 wy0 fc
                [(px (50. - r)),(py (80. - r)),(2. * r'),(2. * r'),
                 0.,180.];
        VX_ps.polyFillArc ps wx0 wy0 fc
                [(px (50. - r)),(py (80. - r)),(2. * r'),(2. * r'),
                 180.,180.];

    end
    | S_WARN ->
    begin
        let fc = if symbol.sym_col <> noColor then
                    symbol.sym_col
                 else
                    fc in
        let bw = i2f symbol.sym_width in


        VX_ps.polyLine ps wx0 wy0 bw fc
                                     [px 30.,py 10.;
                                      px 70.,py 10.;
                                      px 50.,py 50.;
                                      px 30.,py 10.];
                                        
        let r = 10. in
        let r' = wk r in
        VX_ps.polyArc ps wx0 wy0 bw fc 
                [(px (50. - r)),(py (80. - r)),(2. * r'),(2. * r'),
                 0. ,360.];
    end
    | S_QUEST ->
    begin
        let fc = if symbol.sym_col <> noColor then
                    symbol.sym_col 
                 else
                    fc in
        let bw = i2f symbol.sym_width in
        let font = 
                        match ((f2i w)-3) with
                        | s when s <= 8 -> 
                            sym_font_make dpy Helvetica Bold 8;
                        | s when s > 8 && s <= 10 ->
                            sym_font_make dpy Helvetica Bold 10;
                        | s when s > 10 && s <= 12 ->
                            sym_font_make dpy Helvetica Bold 12;
                        | s when s > 12 && s <= 14 ->
                            sym_font_make dpy Helvetica Bold 14;
                        | s when s > 14 && s <= 18 ->
                            sym_font_make dpy Helvetica Bold 18;
                        | s when s > 18 && s <= 20 ->
                            sym_font_make dpy Helvetica Bold 20;
                        | _ -> 
                            sym_font_make dpy Helvetica Bold 24;
                    in

        let sym = "?" in
        let sw = i2f (Xtext.width font.font_info sym) in
        let sh = i2f (font.font_height) in
        let x0,y0 =
            x1 + (w-sw)/2.0,
            y1 + (i2f font.font_ascent) + (h-sh)/2.0 in

        VX_ps.print_text ps wx0 wy0
                      x0 y0
                      fc bc
                      font
                      sym;
    end
    | S_CROSS ->
    begin
        let fc = if symbol.sym_col <> noColor then
                    symbol.sym_col 
                 else
                    fc in
        let bw = i2f symbol.sym_width in
        let font = 
                        match ((f2i w)-3) with
                        | s when s <= 8 -> 
                            sym_font_make dpy Helvetica Bold 8;
                        | s when s > 8 && s <= 10 ->
                            sym_font_make dpy Helvetica Bold 10;
                        | s when s > 10 && s <= 12 ->
                            sym_font_make dpy Helvetica Bold 12;
                        | s when s > 12 && s <= 14 ->
                            sym_font_make dpy Helvetica Bold 14;
                        | s when s > 14 && s <= 18 ->
                            sym_font_make dpy Helvetica Bold 18;
                        | s when s > 18 && s <= 20 ->
                            sym_font_make dpy Helvetica Bold 20;
                        | _ -> 
                            sym_font_make dpy Helvetica Bold 24;
                    in

        let sym = "X" in
        let sw = i2f (Xtext.width font.font_info sym) in
        let sh = i2f font.font_height in
        let x0,y0 =
            x1 + (w-sw)/2.0,
            y1 + (i2f font.font_ascent) + (h-sh)/2.0 in

        VX_ps.print_text ps wx0 wy0
                      x0 y0
                      fc bc
                      font
                      sym;
                                        
    end;
    | S_0 ->
    begin
        let fc = if symbol.sym_col <> noColor then
                    symbol.sym_col 
                 else
                    fc in
        let bw = i2f symbol.sym_width in

        
        let r = w/2.0 - w/5.0 in
        VX_ps.polyArc ps wx0 wy0 bw fc
                      [xc-r,yc-r,(2.0 * r),(2.0 * r),
                       0.0,360.0];
    end;
    | S_1 ->
    begin
        let fc = if symbol.sym_col <> noColor then
                    symbol.sym_col 
                 else
                    fc in
        let bw = i2f symbol.sym_width in

        let d = w/2.0 - w/5.0 in

        VX_ps.polyLine ps wx0 wy0 bw fc [xc,(yc-d);
                                         xc,(yc+d)];
    end;
    | S_MINUS ->
    begin
        let fc = if symbol.sym_col <> noColor then
                    symbol.sym_col 
                 else
                    fc in
        let bw = i2f symbol.sym_width in

        let d = w/2.0 - w/5.0 in

        VX_ps.polyLine ps wx0 wy0 bw fc [xc+d,yc;
                                         xc-d+1.0,yc];
    end;
    | S_PLUS ->
    begin
        let fc = if symbol.sym_col <> noColor then
                    symbol.sym_col 
                 else
                    fc in
        let bw = i2f symbol.sym_width in

        let d = w/2.0 - w/5.0 in

        VX_ps.polyLine ps wx0 wy0 bw fc [xc+d,yc;
                                         xc-d+1.0,yc];
        VX_ps.polyLine ps wx0 wy0 bw fc [xc,yc+d;
                                         xc,yc-d+1.0];

    end;
    | S_R d -> 
    begin
        (*
        ** Draw a filled rectangular area with w=bbox.w-d, h=bbox.h-d
        *)
        let fc = if symbol.sym_col <> noColor then
                    symbol.sym_col 
                 else
                    fc in
        let bw = 1.0 in
        let d = i2f d in
        let x1',y1',x2',y2'=x1+d,
                            y1+d,
                            x2-d,
                            y2-d in
        let cc = [x1',y1';
                  x2',y1';
                  x2',y2';
                  x1',y2';
                  x1',y1'] in

        VX_ps.fillPoly ps wx0 wy0 fc cc;
        VX_ps.polyLine ps wx0 wy0 bw fc cc;
         
    end;
    | S_C d -> ()
    | S_LOCK -> ()

    | S_UP -> 
    begin
        let d = 2.0 in
        (*
        ** Draw a filled triangle area with w=bbox.w-d, h=bbox.h-d
        *)
        let fc = if symbol.sym_col <> noColor then
                    symbol.sym_col 
                 else
                    fc in
        let bw = 1.0 in
        let px1,py1,px2,py2,px3,py3 =
                    x1+d,y2-d,
                    x2-d,y2-d,
                    xc,y1+d in
                
        let cc = [
                    px1,py1;
                    px2,py2;
                    px3,py3;
                    px1,py1] in

        VX_ps.fillPoly ps wx0 wy0 fc cc;
        VX_ps.polyLine ps wx0 wy0 bw fc cc;
    end;
    | S_DOWN -> 
    begin
        let d = 2.0 in
        (*
        ** Draw a filled triangle area with w=bbox.w-d, h=bbox.h-d
        *)
        let fc = if symbol.sym_col <> noColor then
                    symbol.sym_col 
                 else
                    fc in
        let bw = 1.0 in

        let px1,py1,px2,py2,px3,py3 =
                    x1+d,y1+d,
                    x2-d,y1+d,
                    xc,y2-d in
                
        let cc = [
                    px1,py1;
                    px2,py2;
                    px3,py3;
                    px1,py1] in

        VX_ps.fillPoly ps wx0 wy0 fc cc;
        VX_ps.polyLine ps wx0 wy0 bw fc cc;
    end;     
    | S_LEFT -> 
    begin
        let d = 2.0 in
        let bw = 1.0 in
        (*
        ** Draw a filled triangle area with w=bbox.w-d, h=bbox.h-d
        *)
        let fc = if symbol.sym_col <> noColor then
                    symbol.sym_col
                 else
                    fc in
        let px1,py1,px2,py2,px3,py3 =
                    x1+d,yc,
                    x2-d,y1+d,
                    x2-d,y2-d in
                
        let cc = [
                    px1,py1;
                    px2,py2;
                    px3,py3;
                    px1,py1] in

        VX_ps.fillPoly ps wx0 wy0 fc cc;
        VX_ps.polyLine ps wx0 wy0 bw fc cc;
    end;
    | S_RIGHT -> 
    begin
        let d = 2.0 in
        let bw  = 1.0 in
        (*
        ** Draw a filled triangle area with w=bbox.w-d, h=bbox.h-d
        *)
        let fc = if symbol.sym_col <> noColor then
                    symbol.sym_col
                 else
                    fc in
        let px1,py1,px2,py2,px3,py3 =
                    x1+d,y1+d,
                    x2-d,yc,
                    x1+d,y2-d in
                
        let cc = [
                    px1,py1;
                    px2,py2;
                    px3,py3;
                    px1,py1] in

        VX_ps.fillPoly ps wx0 wy0 fc cc;
        VX_ps.polyLine ps wx0 wy0 bw fc cc;
    end;
    | S_BUSY -> ()
    | S_BLANK -> ()
    
let printSymbol ps dpy win wx0 wy0 symbol =
    let fc = win.w_foreground in
    let bc = win.w_background in
    printSymbol2 ps dpy wx0 wy0 fc bc symbol


(*
** Text string utils
*)

(*
** Translate embedded tex symbol to corresponding character string
** from symbol font.
*)

let tex_translate str =
    let tmp = " " in
    let i2s i = tmp.[0] <- char_of_int i; tmp; in
    match str with
    | "\\alpha" -> i2s 0x61;
    | "\\beta" -> i2s 0x62;
    | "\\gamma" -> i2s 0x67;
    | "\\delta" -> i2s 0x64; 
    | "\\epsilon" -> i2s 0x65;
    | "\\varepsilon" -> i2s 0x65;
    | "\\zeta" -> i2s 0x7a;
    | "\\eta" -> i2s 0x68;
    | "\\theta" -> i2s 0x71;
    | "\\vartheta" -> i2s 0x75;
    | "\\iota" -> i2s 0x69;
    | "\\kappa" -> i2s 0x6b;
    | "\\lambda" -> i2s 0x6c;
    | "\\mu" -> i2s 0x6d;
    | "\\nu" -> i2s 0x6e;
    | "\\xi" -> i2s 0x78;
    | "\\pi" -> i2s 0x70;
    | "\\varpi" -> i2s 0x76;
    | "\\rho" -> i2s 0x72;
    | "\\sigma" -> i2s 0x73;
    | "\\varsigma" -> i2s 0x56;
    | "\\tau" -> i2s 0x74;
    | "\\phi" -> i2s 0x66;
    | "\\varphi" -> i2s 0x6a;
    | "\\chi" -> i2s 0x63;
    | "\\psi" -> i2s 0x79;
    | "\\omega" -> i2s 0x77;
    | "\\Gamma" -> i2s 0x47;
    | "\\Delta" -> i2s 0x44;
    | "\\Theta" -> i2s 0x51;
    | "\\Lambda" -> i2s 0x4c;
    | "\\Xi" -> i2s 0x58;
    | "\\Pi" -> i2s 0x50;
    | "\\Sigma" -> i2s 0x53;
    | "\\Upsilon" -> i2s 0xa1;
    | "\\Phi" -> i2s 0x46;
    | "\\Psi" -> i2s 0x59;
    | "\\Omega" -> i2s 0x57;
    | "\\pm" -> i2s 0xb1;
    | "\\times" -> i2s 0xb4;
    | "\\div" -> i2s 0xb8;
    | "\\ast" -> i2s 0x2a;
    | "\\circ" -> i2s 0xb0;
    | "\\bullet" -> i2s 0xb7;
    | "\\cdot" -> i2s 0xd7;
    | "\\cap" -> i2s 0xc7;
    | "\\cup" -> i2s 0xc8;
    | "\\vee" -> i2s 0xda;
    | "\\wedge" -> i2s 0xd9;
    | "\\diamond" -> i2s 0xe0;
    | "\\oplus" -> i2s 0xc5;
    | "\\otimes" -> i2s 0xc4;
    | "\\leq" -> i2s 0xa3;
    | "\\subset" -> i2s 0xcc;
    | "\\subseteq" -> i2s 0xcd;
    | "\\in" -> i2s 0xce;
    | "\\geq" -> i2s 0xb3;
    | "\\supset" -> i2s 0xc9;
    | "\\supseteq" -> i2s 0xca;
    | "\\notin" -> i2s 0xcf;
    | "\\equiv" -> i2s 0xba;
    | "\\sim" -> i2s 0x7e;
    | "\\approx" -> i2s 0xbb;
    | "\\cong" -> i2s 0x40;
    | "\\neq" -> i2s 0xb9;
    | "\\perp" -> i2s 0x5e;
    | "\\mid" -> i2s 0xbd;
    | "\\propto" -> i2s 0xb5;
    | "\\leftarrow" -> i2s 0xac;
    | "\\Leftarrow" -> i2s 0xdc;
    | "\\rightarrow" -> i2s 0xae;
    | "\\Rightarrow" -> i2s 0xde;
    | "\\leftrightarrow" -> i2s 0xab;
    | "\\Leftrightarrow" -> i2s 0xdb;
    | "\\hookleftarrow" -> i2s 0xbf;
    | "\\uparrow" -> i2s 0xad;
    | "\\Uparrow" -> i2s 0xdd;
    | "\\downarrow" -> i2s 0xaf;
    | "\\Downarrow" -> i2s 0xdf;
    | "\\aleph" -> i2s 0xc0;
    | "\\wp" -> i2s 0xc3;
    | "\\Re" -> i2s 0xc2;
    | "\\Im" -> i2s 0xc1;
    | "\\emptyset" -> i2s 0xc6;
    | "\\nabla" -> i2s 0xd1;
    | "\\surd" -> i2s 0xd6;
    | "\\bot" -> i2s 0x5e;
    | "\\angle" -> i2s 0xd0;
    | "\\neg" -> i2s 0xd8;
    | "\\sharp" -> i2s 0x23;
    | "\\partial" -> i2s 0xb6;
    | "\\infty" -> i2s 0xa5;
    | "\\Diamond" -> i2s 0xe0;
    | "\\clubsuit" -> i2s 0xa7;
    | "\\diamondsuit" -> i2s 0xa8;
    | "\\hartsuit" -> i2s 0xa9;
    | "\\spadesuit" -> i2s 0xaa;
    | "\\sum" -> i2s 0xe5;
    | "\\prod" -> i2s 0xd5;
    | _ -> str

(*
** Return the length of a string in pixel units. Depends on current
** font setting.
*)
let string_width font str =
    if (font.font_name = "fixed") then
        ((String.length str)*font.font_width)
    else
        Xtext.width font.font_info str

(*
** Same as above, but with optional embedded symbols in tex like style
**
**  "The distance in \\mu m."
*)

let string_width_S font font_symbol str =
    let contains_sym = String.contains str '\\' in
    if not contains_sym then
        string_width font str 
    else
    begin    
        let parts = ref [] in
        let cur = ref "" in
    
        let in_symbol = ref false in
        let tmp = " " in
        String.iter (fun c -> 
            tmp.[0] <- c;        
            match c with
            | '\\' -> in_symbol := true;
                      parts := !parts @ [false, !cur];
                      cur := "" ^ tmp;
            | ' ' -> 
                     if !in_symbol then
                     begin
                        parts := !parts @ [true, !cur];
                        cur := "";
                        in_symbol := false;
                     end
                     else
                        cur := !cur ^ tmp;
            | _ -> cur := !cur ^ tmp; 
            ) str;          
        if !cur <> "" then 
            parts := !parts @ [!in_symbol, !cur];
        let width = ref 0 in
        List.iter (fun (is_symbol,str) ->
            if not is_symbol then
                width := !width + 
                    (
                        if (font.font_name = "fixed") then
                            ((String.length str)*font.font_width)
                        else    
                            Xtext.width font.font_info str
                    )
            else
            begin
                let str = tex_translate str in
                width := !width + (Xtext.width font_symbol.font_info str);
            end;
            ) !parts;
        !width
    end


(*
** Maximal width for text line list.
*)
let string_width_max font lines =
    let width = ref 0 in
    List.iter (fun line ->
        width := max !width (string_width font line);
        ) lines;
    !width

(*
** Embedded symbol support version.
*)
let string_width_max_S font font_symbol lines =
    let width = ref 0 in
    List.iter (fun line ->
        width := max !width (string_width_S font font_symbol line);
        ) lines;
    !width


(*
** Mean width of a line with n times 'o' chars.
*)
let string_width_Xn font n =
    if (font.font_name = "fixed") then
        (n*font.font_width)
    else
        (n*(Xtext.width font.font_info "o"))


(*
** Emit a string at given start position x0,y0. Embedded symbols
** are resolved and displayed with symbol font.
*)

let draw_string_S dpy win cache 
                x0 y0 fg bg 
                font font_symbol str =
    let gc1 = GCCache.get_fg_bg_font cache fg bg  
                                     font.font_id in

    let len = String.length str in
    let contains_sym = String.contains str '\\' in
    if not contains_sym then
        Xlib.drawSubString dpy win gc1
                           x0 y0 str 0 len
    else
    begin
        let gc2 = GCCache.get_fg_bg_font cache fg bg  
                                         font_symbol.font_id in
        let x = ref x0 in

        let parts = ref [] in
        let cur = ref "" in
        let in_symbol = ref false in
        let tmp = " " in
        String.iter (fun c -> 
            tmp.[0] <- c;        
            match c with
            | '\\' -> in_symbol := true;
                      parts := !parts @ [false, !cur];
                      cur := "" ^ tmp;
            | ' ' -> 
                     if !in_symbol then
                     begin
                        parts := !parts @ [true, !cur];
                        cur := "";
                        in_symbol := false;
                     end
                     else
                        cur := !cur ^ tmp;
            | _ -> cur := !cur ^ tmp; 
            ) str;          
        if !cur <> "" then 
            parts := !parts @ [!in_symbol, !cur];
        List.iter (fun (is_symbol,str) ->
            if not is_symbol then
            begin
                let len = String.length str in
                Xlib.drawSubString dpy win gc1  
                                   !x y0 str 0 len; 
                x := !x + (string_width font str);
            end
            else
            begin
                let str = tex_translate str in
                let len = String.length str in
                Xlib.drawSubString dpy win gc2  
                                   !x y0 str 0 len; 
                x := !x + (string_width font_symbol str);
            end;
            ) !parts;
    end

(*
** Print string. Extracts embedded symbols. 
*)
let print_string_S ps wx0 wy0 
                 x0 y0 fc bc 
                 font 
                 font_symbol 
                 str =

    let len = String.length str in
    let contains_sym = String.contains str '\\' in
    if not contains_sym then
        VX_ps.print_text ps wx0 wy0
                         x0 y0
                         fc bc 
                         font
                         str 
    else
    begin
        let x = ref x0 in

        let parts = ref [] in
        let cur = ref "" in
        let in_symbol = ref false in
        let tmp = " " in
        String.iter (fun c -> 
            tmp.[0] <- c;        
            match c with
            | '\\' -> in_symbol := true;
                      parts := !parts @ [false, !cur];
                      cur := "" ^ tmp;
            | ' ' -> 
                     if !in_symbol then
                     begin
                        parts := !parts @ [true, !cur];
                        cur := "";
                        in_symbol := false;
                     end
                     else
                        cur := !cur ^ tmp;
            | _ -> cur := !cur ^ tmp; 
            ) str;          
        if !cur <> "" then 
            parts := !parts @ [!in_symbol, !cur];
        List.iter (fun (is_symbol,str) ->
            if not is_symbol then
            begin
                VX_ps.print_text ps wx0 wy0
                                 !x y0
                                 fc bc
                                 font
                                 str;
                x := !x + (i2f (string_width font str));
            end
            else
            begin
                let str = tex_translate str in
                VX_ps.print_text ps wx0 wy0
                                 !x y0 
                                 fc bc
                                 font_symbol
                                 str;
                x := !x + (i2f (string_width font_symbol str));
            end;
            ) !parts;
    end


(*
** Remove newlines from a string.
*)
let no_nl str =
    let i = ref 0 in
    String.iter (fun c -> 
                    if c = '\n' then str.[!i] <- ' ';
                    incr i;
        ) str;
    str

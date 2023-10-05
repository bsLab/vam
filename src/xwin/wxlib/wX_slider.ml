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
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2003 BSSLAB
**    $CREATED:     
**    $VERSION:     1.04
**
**    $INFO:
**
**  Slider class, similar to WX_valbar class.
**
** This widget displays a value controlled slider. The default range and
** unit is 0 .. 100 %. Either use the decr/incr_value methods or
** the set_value method to change the value.
** The action handler must eb set with the set_action method. There are
** two reasons for calling the action handler: the slider was immediately
** changed, and the slider was not changed within a timeout period.
** The first case is needed for example to synchronosize a label
** widget displaying the value, and the second case to perform the
** real action related with the slider (== the final choice).
** The current value can be extracted with the get_value method.
**
** Example:
**
**  let hbar = new WX_bar.h vbar#container [ IpadX 5; IpadY 5;]
**  let sb = new WX_slider.h hbar#container [MinWidth 200]
**  let sl = new WX_label.t hbar#container " f=0.1 Hz" []
**
**  hbar#container_add_s [sb#contained; sl#contained];
**
**  sb#set_range (0.1,2.0);
**  sb#set_delta 0.1;
**  sb#set_value 0.1;
**  sb#set_action (fun () ->
**            sl#set_string (Printf.sprintf "H=%3.0f %%"
**                                    sb#get_value);
**            if (sb#activated = false) then
**            begin
**              >>> ACTION <<<
**            end;
**        );
**
**    $ENDOFINFO
**
*)


open Xtypes
open WX_types


(*
** some utils
*)
let string_width font str =
    (*
    ** Devide string in sub string seperated by newlines
    ** and examine each string; use the maximum of string width
    *)
    
    let strlist = Str.split (Str.regexp "[\n]") str in
    let maxwidth = ref 0 in 
    List.iter (fun str ->
                let linewidth =
                    if (font.font_name = "fixed") then
                        ((String.length str)*font.font_width)
                    else
                        Xtext.width font.font_info str
                in
                if (linewidth > !maxwidth) then
                    maxwidth := linewidth;
              ) strlist;
    !maxwidth



class h parent attributes =

  let (nilformat:(float -> string, unit, string) format) = "%fNOVALUE" in
  let dpy' = parent#display in

  let defcmap = Xlib.defaultColormap dpy' in

  (* some default colors *)

  let { anc_pixel = red } = X.allocNamedColor dpy' defcmap "red" in
  let { anc_pixel = blue } = X.allocNamedColor dpy' defcmap "blue" in
  let { anc_pixel = green } = X.allocNamedColor dpy' defcmap "green" in
  let { anc_pixel = white } = X.allocNamedColor dpy' defcmap "white" in
  let { anc_pixel = yellow } = X.allocNamedColor dpy' defcmap "yellow" in
  let { anc_pixel = black } = X.allocNamedColor dpy' defcmap "black" in
  let { anc_pixel = grey } = X.allocNamedColor dpy' defcmap "grey" in
  let { anc_pixel = grey60 } = X.allocNamedColor dpy' defcmap "grey60" in

  let color_of_name name =
    (
            match name with
            | "blue"    -> blue;
            | "red"     -> red;
            | "green"   -> green;
            | _   -> 
                         let { anc_pixel = col } = X.allocNamedColor dpy'
                                                     defcmap name in
                         col;
     )
  in
  
  object (self)

  val mutable font = parent#font_make !default_font true

  inherit WX_object.t parent 
            ([
                MinWidth 100; MinHeight 20;
                Relief ReliefSunken;
             ] @ attributes)
    as super

  val mutable activated = false
  val mutable value = 0.0      
  val mutable last_value = 0.0
  val mutable action = (fun () -> ());

  (*
  ** Default setting: Range = 0..100%
  *)

  val mutable range_min = 0.0
  val mutable range_max = 100.0

  val mutable delta = 1.0

  val mutable formatter = 
    let (f:(float -> string, unit, string) format) = "%4.0f %%" in
    f


  (*
  ** Color of the value bar
  *)

  val mutable color = grey
  val mutable backcolor = black

  val mutable timer_active = false
  method timer t f =
    timer_active <- true;
    ignore(Thread.thread_create (fun () -> 
            Thread.delay t; f (); timer_active <- false) ())

  initializer
    self#configure [Bindings [
        Button (1,0), (fun _ ->
            begin
              activated <- true;
              let sz = szhints in
              let width = max sz.min_width sz.requested_width in
              let pos, total = !mouse_x_event , width
                in
              self#set_value 
                    ((float_of_int pos)/.(float_of_int width)
                        *. (range_max -. range_min) +. range_min);
              if (timer_active = false) then
                  self#timer 1 (fun () -> activated <- false; action ());
            end
            );
        ButtonMotion, (fun _ -> 
            begin
              activated <- true;
              let sz = szhints in
              let width = max sz.min_width sz.requested_width in
              let pos, total = !mouse_x_event , width
                in
              self#set_value 
                    ((float_of_int pos)/.(float_of_int width)
                        *.(range_max -. range_min) +. range_min);
              if (timer_active = false) then
                  self#timer 1 (fun () -> activated <- false; action ());
            end
            );
    ]];

  (* 
  ** Return the current widget size
  *)

  method size_request =
    let sz = szhints in
    if not w.w_size_modified || sz.comp_timestamp = s.s_timestamp then 
        sz
    else
      begin
        sz.comp_timestamp <- s.s_timestamp;
        sz.requested_height <- max sz.min_height sz.requested_height;
        sz.requested_width <- max sz.min_width sz.requested_width;
        sz
      end

  (*
  ** Full refresh of the widget
  *)

  method refresh =
    if s.s_timestamp > w.w_refresh_timestamp && not (w.w_window == noWindow)
    then
      begin
        super#refresh;
        let sz = szhints in
        let width = max sz.min_width sz.requested_width in
        let height = max sz.min_height sz.requested_height in

        let fg = w.w_foreground.c_pixel in
        let bg = w.w_background.c_pixel in

        let gc_frame = GCCache.get3 s.s_gcs white bg font.font_id in
        let gc_bar = GCCache.get3 s.s_gcs grey60 bg font.font_id in
        let gc_notbar = GCCache.get3 s.s_gcs backcolor bg font.font_id in
        let gc_text = GCCache.get3 s.s_gcs white backcolor font.font_id in
        let gc_barc = GCCache.get3 s.s_gcs color bg font.font_id in


        let g = w.w_geometry in
        let dpy = s.s_display in
        let win = w.w_window in

        let xc = max
                (int_of_float 
                 (
                  (  ((float_of_int width) -. 8.0 -. 5.0)  *. 
                    (value -. range_min)
                  ) /. (range_max -. range_min) +. 5.0
                 ))
                (4+5)                 
        in

        let x0 = 4 + 10 
            in
        let x1 = max
                 (xc-5)
                 4                 
            in
        let x2 = min
                 (xc+5)
                 (width-4)                
            in

        Xlib.fillRectangle dpy win gc_notbar 4 4 (x1-x0+10) (height-8);
        Xlib.fillRectangle dpy win gc_notbar x2 4 (width-x2-4) (height-8);
        Xlib.fillRectangle dpy win gc_bar x1 4 (x2-x1) (height-8) ;
        Xlib.drawRectangle dpy win gc_frame 2 2 (width-4) (height-5);
        Xlib.fillRectangle dpy win gc_barc (x1+2) 6 (x2-x1-4) (height-12) ;

(*
        let string = if (formatter = nilformat) then
                        ""
                     else
                        (Printf.sprintf formatter value)
        in

        let len = String.length string in
        
        let xt = width/2 - (string_width font string)/2 in
        let yt = height/2 + (font.font_ascent / 2) in

        X.changeGC dpy gc_text [GCfonction GXxor];
        Xlib.drawSubString dpy win gc_text
                           xt yt string 0 len;
*)
        last_value <- value;
      end

  (*
  ** Partial update (only the difference to the last_value
  ** is handled). 
  *)

  method update =
    if not (w.w_window == noWindow)
    then
      begin
        super#refresh;
        let sz = szhints in
        let width = max sz.min_width sz.requested_width in
        let height = max sz.min_height sz.requested_height in

        let g = w.w_geometry in
        let dpy = s.s_display in
        let win = w.w_window in

        let string_new = if (formatter = nilformat) then
                        ""
                     else
                        (Printf.sprintf formatter value)
        in
        

        let len_new = String.length string_new in
        let xt_new = width/2  - (string_width font string_new)/2 in
        let yt_new = height/2 + (font.font_ascent / 2) in

        let string_old = if (formatter = nilformat) then
                        ""
                     else
                        (Printf.sprintf formatter last_value)
        in


        let len_old = String.length string_old in
        let xt_old = width/2 - (string_width font string_old)/2 in
        let yt_old = height/2 + (font.font_ascent / 2)  in

        let fg =  w.w_foreground.c_pixel in
        let bg =  w.w_background.c_pixel in

        let gc_frame = GCCache.get3 s.s_gcs white bg font.font_id in
        let gc_bar = GCCache.get3 s.s_gcs grey60 bg font.font_id in
        let gc_notbar = GCCache.get3 s.s_gcs backcolor bg font.font_id in
        let gc_text = GCCache.get3 s.s_gcs white backcolor font.font_id in
        let gc_barc = GCCache.get3 s.s_gcs color bg font.font_id in

        let xc_old = max
                (int_of_float 
                 (
                  (  ((float_of_int width) -. 8.0 -. 5.0)  *. 
                    (last_value -. range_min)
                  ) /. (range_max -. range_min) +. 5.0
                 ))
                (4+5)                 
        in

        let x0_old = 4 + 10
            in
        let x1_old = max
                 (xc_old-5)
                 4                 
            in
        let x2_old = min
                 (xc_old+5)
                 (width-4)                
            in
        let xc_new = max
                (int_of_float 
                 (
                  (  ((float_of_int width) -. 8.0 -. 5.0)  *. 
                    (value -. range_min)
                  ) /. (range_max -. range_min) +. 5.0
                 ))
                (4+5)                 
        in

        let x0_new = 4 + 10
            in
        let x1_new = max
                 (xc_new-5)
                 4                 
            in
        let x2_new = min
                 (xc_new+5)
                 (width-4)
            in

        Xlib.fillRectangle dpy win gc_notbar x1_old 4 
                                            (x2_old-x1_old) (height-8) ;
        Xlib.fillRectangle dpy win gc_bar x1_new 4 
                                            (x2_new-x1_new) (height-8) ;
        Xlib.fillRectangle dpy win gc_barc (x1_new+2) 6 
                                           (x2_new-x1_new-4) (height-12) ;

        last_value <- value;

      end

  (*
  ** The user specified action handler is always the slider was changed
  ** called. But sometimes, only the last selected values if of
  ** interest. User can ask still activeated slider (button pressed).
  *)
  method activated = activated

  (*
  ** User method to set the new value value [range_im..range_max]. The
  ** widget will be updated to this new value.
  *)

  method set_value v =
        if (v <= range_max) &&
           (v >= range_min) then
        begin
                value <- v;
                self#update;
                action ();
        end

  method get_value = value

  (*
  ** Increment or decremnt the value value [+-delta].
  *)

  method incr_value  =
        let v = value +. delta in
        if (v <= range_max) &&
           (v >= range_min) then
        begin
                value <- v;
                self#update;
                action ();
        end;
        value

  method decr_value  =
        let v = value -. delta in
        if (v <= range_max) &&
           (v >= range_min) then
        begin
                value <- v;
                self#update;
                action ();
        end;
        value

  (*
  ** Set the delta value
  *)

  method set_delta d =
        delta <- d


  (*
  ** Set the value bar range
  *)

  method set_range r =
        let (mn,mx) = r in
        range_min <- mn;
        range_max <- mx

  (*
  ** Set the value bar and background color 
  *)

  method set_color c =
        color <- color_of_name c
  method set_backcolor c =
        backcolor <- color_of_name c



  (*
  ** Set the value bar format string,
  ** for example "%f Volt" - printf like format !
  ** Disable printing the value with the string "%fNOVALUE".
  *)

  method set_format f = 
    formatter <- f

  (*
  ** Set action called after slider changed
  *)
  method set_action f = action <- f

  method name = "valbar"

end


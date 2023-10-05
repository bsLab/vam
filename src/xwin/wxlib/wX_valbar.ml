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
**    $VERSION:     1.06
**
**    $INFO:
**
** Progress bar widget. 
**
** This widget displays a value (progess) bar. The default range and
** unit is 0 .. 100 %. Either use the decr/incr_value methods or
** the set_value method to change the value. The current value
** can be extracted with the get_value method.
** Foregound and background colors can be controlled with the
** set_color and set_backcolor methods. The displayed value format
** can be set with the set_format method (in the printf style).
**
** Example:
**
**  let hbar = new WX_bar.h vbar#container [ IpadX 5; IpadY 5;]
**
**  let pb2 = new WX_valbar.t hbar#container [MinWidth 200]
**  pb#set_format "%2.0f %% Voltage";
**  pb#set_color "red"; [or blue (default),green]
**  hbar#container_add pb#contained;
**
**  pb#set_value 20.0;
**
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



class t parent attributes =

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
                Relief ReliefSunken
             ] @ attributes)
    as super

  val mutable value = 0.0      
  val mutable last_value = 0.0


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

  val mutable color = blue
  val mutable backcolor = white

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
        let gc_bar = GCCache.get3 s.s_gcs color bg font.font_id in
        let gc_notbar = GCCache.get3 s.s_gcs backcolor bg font.font_id in
        let gc_text = GCCache.get3 s.s_gcs white backcolor font.font_id in


        let g = w.w_geometry in
        let dpy = s.s_display in
        let win = w.w_window in
        let x1 = 4 in

        let x2 = max
                (int_of_float 
                 (
                  (  ((float_of_int width) -. 8.0 )  *. 
                    (value -. range_min)
                  ) /. (range_max -. range_min)
                 ))
                4                 
        in
                

        Xlib.fillRectangle dpy win gc_bar x1 4 (x2-x1) (height-8) ;
        Xlib.fillRectangle dpy win gc_notbar x2 4 (width-x2-4) (height-8);
        Xlib.drawRectangle dpy win gc_frame 2 2 (width-4) (height-5);


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
        X.changeGC dpy gc_text [GCfonction GXcopy];
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
        let gc_bar = GCCache.get3 s.s_gcs color bg font.font_id in
        let gc_notbar = GCCache.get3 s.s_gcs backcolor bg font.font_id in
        let gc_text = GCCache.get3 s.s_gcs white backcolor font.font_id in

        if (last_value > value) then
        begin
            X.changeGC dpy gc_text [GCfonction GXxor];
            Xlib.drawSubString dpy win gc_text
                           xt_old yt_old string_old 0 len_old;

            (*
            let x1 = ((width-8) * value) / 100 + 4 in
            let x2 = max (((width-8) * (last_value-value+1)) / 100) 1 in 
            *)

            let x1 = (int_of_float (floor
                      (
                        ( ((float_of_int width) -. 8.0) *.
                          (value-.range_min)
                        ) /. (range_max-.range_min)
                      ))
                     ) + 4
            in
            let x2 = max
                     (int_of_float (ceil    
                      (
                        ( ((float_of_int width) -. 8.0 )  *. 
                          (last_value -. value)
                        ) /. (range_max -. range_min)
                      ))
                     )
                     1
            in

            Xlib.fillRectangle dpy win gc_notbar x1 4 x2 (height-8) ;
            X.changeGC dpy gc_text [GCfonction GXxor];
            Xlib.drawSubString dpy win gc_text
                           xt_new yt_new string_new 0 len_new;
            X.changeGC dpy gc_text [GCfonction GXcopy];
        end
        else
        begin


            X.changeGC dpy gc_text [GCfonction GXxor];
            Xlib.drawSubString dpy win gc_text
                           xt_old yt_old string_old 0 len_old;


            (*
            let x1 = ((width-8) * last_value) / 100 + 4 in
            let x2 = max (((width-8) * (value-last_value+1)) / 100) 1 in 
            *)

            let x1 = (int_of_float (floor  
                      (
                        ( ((float_of_int width) -. 8.0) *.
                          (last_value -. range_min)
                        ) /. (range_max -. range_min)
                      ))
                     ) + 4
            in
            let x2 = max
                     (int_of_float (ceil
                      (
                        (  
                            ((float_of_int width) -. 8.0 )  *. 
                            (value -. last_value)
                        ) /. (range_max -. range_min)
                      )) 
                     )
                     1
            in

            Xlib.fillRectangle dpy win gc_bar x1 4 x2 (height-8) ;

            X.changeGC dpy gc_text [GCfonction GXxor];
            Xlib.drawSubString dpy win gc_text
                           xt_new yt_new string_new 0 len_new;
            X.changeGC dpy gc_text [GCfonction GXcopy];
        end;

        last_value <- value;

      end

  (*
  ** User method to set the new value value [range_im..range_max]. The
  ** widget will be updated to this new value.
  *)

  method set_value v =
        if (v <= range_max) &&
           (v >= range_min) then
        begin
                value <- v;
                self#update

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
                self#update
        end;
        value

  method decr_value  =
        let v = value -. delta in
        if (v <= range_max) &&
           (v >= range_min) then
        begin
                value <- v;
                self#update
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

  method name = "valbar"

end


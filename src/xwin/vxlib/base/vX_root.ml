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
**    $CREATED:     10.5.2005
**    $VERSION:     1.12
**
**    $INFO:
**
**  VXlib root widget.
**
**    $ENDOFINFO
**
*)



open Xtypes
open VX_types
open VX_common

class t display i =
    let eloop = display#eloop in
    let display = display#display in
    let scr = display.dpy_roots.(i) in
    let fid = X.openFont display !default_font in
    let qf = X.queryFont display fid in
    let maxb = qf.qf_info.font_max_bounds in
    let font = {
      (*
      ** Currently unknown.
      *)
      font_class = NoFont;
      font_style = NoStyle;
      font_size = 0;
      font_name = !default_font;
      font_id = fid;
      font_info = qf;
      font_ascent = maxb.char_ascent;
      font_height = maxb.char_ascent + maxb.char_descent;
      font_width = maxb.char_width;      
    } in
    let s =  {
      s_display = display;
      s_screen = scr;
      s_colors = Hashtbl.create 23;
      s_fonts = Hashtbl.create 23;
      s_pixmaps = Hashtbl.create 23;
      s_images = Hashtbl.create 23;
      s_cursors = Hashtbl.create 23;
      s_eloop = eloop;
      s_default_color = { c_name = ""; c_pixel = scr.scr_black_pixel;
        c_red = 0; c_green = 0; c_blue = 0; };
      s_default_font = font;
      s_last_click = default_click;
      s_timestamp = 5;
      s_wait_resize = [];
      s_wait_refresh = [];
      s_gcs = GCCache.create display scr;
    } in
    let root = display.dpy_roots.(i) in
    let window = { 
      w_clear = false;
      w_clipped = false;
      w_window = root.scr_root;
      w_shown = true;
      w_geometry = { x=0; y=0; border=0;
        width = root.scr_width; height = root.scr_height } ;
      w_clipping = { x=0; y=0; border=0; width=0; height =0};
      w_override = true;
      w_background = noColor;
      w_foreground = noColor;
      w_frame  = empty_frame;
      w_cursor = noCursor;
      w_mask = [];
      w_size_timestamp = 0;
      w_refresh_timestamp = 0;
      w_inverse = false;
      w_enter_window = null_handler;
      w_leave_window = null_handler;
      w_button_released = null_handler;
      w_button_press = null_handler;
      w_key_press = null_handler;      
      w_key_release = null_handler;      
      w_button_motion = null_handler;
      w_pointer_motion = null_handler;
      w_focus_in = null_handler;
      w_focus_out = null_handler;
      w_actions = [];
      w_ipad_x = 0;
      w_ipad_y = 0;
      w_adjust_x = false;
      w_adjust_y = false;
      w_size_modified = true;
    } in
    object (self)
  
    inherit VX_base.t window s

    initializer 
        w.w_button_press <- (fun _ -> self#handle_button ());
        w.w_key_press <- (fun _ -> self#handle_key ());
        w.w_key_release <- (fun _ -> ());
        Eloop.add_after_events_hook (fun () -> self#update);

    val mutable default_font = font
    method update_top_size = ()
    method wait_refresh clear x y dx dy = ()
    method default_font = default_font
    method wait_resize = ()
    method container_add (o : contained) = ()
    method geometry = w.w_geometry
end


class from_display dpyname num =
  let display = new VX_display.t dpyname in
  object (self)
  inherit t display num
end
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
**    $INITIAL:     (C) 1999 INRIA
**    $VERSION:     1.02
**
**    $INFO:
**
**    $ENDOFINFO
**
*)





let default_label_font = ref Fonts.Times.Bold.s12

(*
** Note: some widgets should not have associated windows. A label can be
** one of them. 
*)

open Xtypes
open WX_types


(*
** some utilities
*)

let string_width font str =
    if (font.font_name = "fixed") then
        ((String.length str)*font.font_width)
    else
        Xtext.width font.font_info str


type options =
  Justification of justified
| String of string
| Font of string


class orig parent str attributes =

  object (self)

  val mutable font = parent#font_make !default_label_font true


    inherit WX_object.t parent attributes as super
  
  val mutable string = str
  val mutable justified = Left
  
  val cursor = String.create 1
  val mutable cursor_x = -1
  val mutable display_cursor = false
  
  method set_options attrs =
    let draw_changed = ref false in
    let size_changed = ref false in
    List.iter (fun attr ->
        match attr with
          Justification j ->
            justified <- j;
            draw_changed := true
        | String s ->
            if s <> string then
              begin
                draw_changed := true;
                if String.length s <> String.length string then
                  (size_changed := true;
                    if cursor_x > String.length s then
                      cursor_x <- String.length s);
                string <- s;                 
              end
        | Font f ->
            if f <> font.font_name then
              begin
                font <- self#font_make f true;
                draw_changed := true;
                size_changed := true;                
              end) attrs;
    if not (w.w_window == noWindow) then
      begin
        if !size_changed then self#wait_resize;
        if !draw_changed then self#wait_refresh true 0 0 0 0;
      end
  
  method set_string s = 
    self#set_options [String s]

  method set_font s = self#set_options [Font s]
  method set_justification s = self#set_options [Justification s]
  method string = string
  
  method size_request =
    let sz = szhints in
    if not w.w_size_modified || sz.comp_timestamp = s.s_timestamp then sz else
      begin
        sz.comp_timestamp <- s.s_timestamp;
        (* let char_info = Xtext.extents font.font_info string in *)
        sz.requested_width <- 
          max ((string_width font string) + 4 + 2 * w.w_ipad_x) sz.min_width;
        sz.requested_height <- max
          (font.font_height + 4 + 2 * w.w_ipad_y) sz.min_height;
        sz
      end
      
  method refresh =
    if s.s_timestamp > w.w_refresh_timestamp && not (w.w_window == noWindow) 
    then    
      begin
        super#refresh;
        let sz = szhints in
        let qf = font.font_info in
        let fg = w.w_foreground.c_pixel in
        let bg =  w.w_background.c_pixel in
        let gc = GCCache.get3 s.s_gcs fg bg font.font_id in
        let height =  font.font_height in
        let offset = max 0 (w.w_geometry.height - height - 2 * w.w_ipad_y)/2 in
        let y = font.font_ascent + offset in
        let len = String.length string in
        let width = string_width font string in
        let g = w.w_geometry in

        let x = match justified with
            Left -> w.w_ipad_x + 2
          | Center -> (g.width - width)/2
          | Right -> g.width - width - 2 - w.w_ipad_x
        in

        X.clearArea s.s_display w.w_window 0 0 0 0 false;
        Xlib.drawSubString s.s_display w.w_window gc  
                           x (w.w_ipad_y + y) string 0 len;

        if display_cursor then 
          let gc = GCCache.get2 s.s_gcs bg fg in
          cursor.[0] <- if cursor_x > String.length string - 1 then ' ' else
            string.[cursor_x];

          Xlib.imageSubString s.s_display w.w_window gc  
            (x+(string_width font (String.sub string 0 cursor_x)))
            (w.w_ipad_y + y) cursor 0 1
      end  
  method name = "label"
end

class t = orig

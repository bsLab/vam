(***********************************************************************)
(*                                                                     *)
(*                             ____                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(*
** Generic image widget. Derived from the WX_pixmap class.
*)

open Xtypes
open WX_types
open Ximage

class orig parent descr attributes =
  let image = parent#image_make descr in
  
  object (self)
  inherit WX_object.t parent attributes as super
  
  val mutable dy = image.iheight
  val mutable dx = image.iwidth
  val mutable pixmap = image.xpixmap

  method set_imagebuf desc =
    image.imagebuf  <- desc;
    image.imodified <- true
  
  method modified =
    image.imodified <- true
  
  method size_request =
    let sz = szhints in
    if not w.w_size_modified || sz.comp_timestamp = s.s_timestamp then sz else
      begin
        sz.comp_timestamp <- s.s_timestamp;
        sz.requested_width <- max (dx+4+2* w.w_ipad_x) sz.min_width;
        sz.requested_height <- max (dy+4+ 2*w.w_ipad_y) sz.min_height;
        sz
      end


      
  method refresh =
    if s.s_timestamp > w.w_refresh_timestamp && not (w.w_window == noWindow) 
      then    
      begin
        super#refresh;
        let gc = X.createGC s.s_display s.s_screen.scr_root
            [GCforeground w.w_foreground.c_pixel;
            GCbackground w.w_background.c_pixel]
        in
        let g = w.w_geometry in
        let sz = szhints in
        let x = max ((g.width - dx)/2) w.w_ipad_x in
        let y = max ((g.height - dy)/2) w.w_ipad_y in

	    let mask = 0 in

	    Ximage.putImage s.s_display 
                        pixmap
                        gc
                        0 0
                        image;			


        X.changeGC s.s_display gc 
                [GCclip_mask mask; GCclip_y_origin x; GCclip_x_origin y];


        X.copyArea s.s_display gc pixmap 0 0 
              w.w_window x y dx dy;

        X.freeGC s.s_display gc;


      end  
  method name = "imagemap"
end

class t = orig

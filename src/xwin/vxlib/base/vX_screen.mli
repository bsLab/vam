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
**    $VERSION:     1.03
**
**    $INFO:
**
**  VXlib screen management
**
**    $ENDOFINFO
**
*)

val screen_resize : VX_types.screen_struct -> unit
val iter_refresh : < refresh : 'a; .. > list -> unit
val screen_update : VX_types.screen_struct -> unit
class virtual t :
  VX_types.screen_struct ->
  object
    val s : VX_types.screen_struct
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method cursor_make : VX_types.cursor_desc -> bool -> VX_types.cursor
    method display : Xtypes.display
    method font_make : VX_types.text_font -> VX_types.text_style -> 
                       VX_types.text_size ->
                       bool -> VX_types.font

    method getHilite : VX_types.color -> VX_types.color
    method getShadow : VX_types.color -> VX_types.color

    method global_color_make : VX_types.color_desc -> bool -> VX_types.color

    method pixmap_make : string * VX_types.pixmap_desc -> Xpm.pixmap
    method pixmap_get : string -> Xpm.pixmap
    method image_make : string * VX_types.image_desc -> Ximage.t
    method image_get : string -> Ximage.t

    method screen : VX_types.screen_struct
    method update : unit
  end

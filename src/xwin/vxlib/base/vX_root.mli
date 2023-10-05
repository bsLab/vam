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
**    $VERSION:     1.06
**
**    $INFO:
**
**  VXlib root widget.
**
**    $ENDOFINFO
**
*)


class t :
  < display : Xtypes.display; eloop : Eloop.display; .. > ->
  int ->
  object
    val mutable default_font : VX_types.font
    val s : VX_types.screen_struct
    val w : VX_types.window

    (*
    ** Inherited from VX_screen
    *)
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

    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method container : VX_types.container
    method container_add : VX_types.contained -> unit
    method default_font : VX_types.font
    method foreground : VX_types.color
    method geometry : Xtypes.geometry
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method name : string
    method set_name : string -> unit
    method root_coordinates : int * int
    method update_top_size : unit
    method wait_refresh :
      bool ->
      Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
    method wait_resize : unit
    method width : int
    method window : Xtypes.window
    method win : VX_types.window
    method xevents : Xtypes.xevent -> unit
  end

class from_display :
  string ->
  int ->
  object
    val mutable default_font : VX_types.font
    val s : VX_types.screen_struct
    val w : VX_types.window

    (*
    ** Inherited from VX_screen
    *)
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

    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method container : VX_types.container
    method container_add : VX_types.contained -> unit
    method default_font : VX_types.font
    method foreground : VX_types.color
    method geometry : Xtypes.geometry
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method name : string
    method set_name : string -> unit
    method root_coordinates : int * int
    method update_top_size : unit
    method wait_refresh :
      bool ->
      Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
    method wait_resize : unit
    method width : int
    method window : Xtypes.window
    method win : VX_types.window
    method xevents : Xtypes.xevent -> unit
  end

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
**    $CREATED:     11.5.2005
**    $VERSION:     1.11
**
**    $INFO:
**
**  VXlib object management
**
**    $ENDOFINFO
**
*)


val next_id : int ref
val new_szhints : 'a -> VX_types.szhints
class t :
  VX_types.container ->
  VX_types.widget_attributes list ->
  object
    val id : int
    val mutable parent : VX_types.container
    val s : VX_types.screen_struct
    val mutable szhints : VX_types.szhints
    val w : VX_types.window

    (*
    ** Inherited from VX_screen
    *)
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method cursor_make : VX_types.cursor_desc -> bool -> VX_types.cursor
    method display : Xtypes.display
    method font_make : VX_types.text_font -> VX_types.text_style ->
                       VX_types.text_size -> bool -> VX_types.font
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
    method configure : VX_types.widget_attributes list ->
                       VX_types.widget_attributes list   
    method contained : VX_types.contained
    method container : VX_types.container
    method default_font : VX_types.font
    method destroy : unit
    method draw_frame : unit
    method focus : unit
    method foreground : VX_types.color
    method geometry : Xtypes.geometry
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method hide : unit
    method id : string
    method inverse : unit
    method iter : (VX_types.contained -> unit) -> unit
    method iter_visible : (VX_types.contained -> unit) -> unit
    method name : string
    method set_name : string -> unit
    method normal : unit
    method parent : VX_types.container
    method print : VX_types.ps -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method set_parent : VX_types.container -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    method size_request : VX_types.szhints
    method win_request : VX_types.window
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update_size : unit
    method update_top_size : unit
    method update_clipping : unit
    method wait_refresh :
      bool ->
      Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
    method wait_resize : unit
    method width : int
    method window : Xtypes.window
    method win : VX_types.window
    method xevents : Xtypes.xevent -> unit
  end

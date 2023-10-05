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
**    $VERSION:     1.12
**
**    $INFO:
**
**  Check box selector with buttons.
**
**  Horizontal:     B1 Label1 B2 Label2 ...
**  Vertical:       B1 Label1
**                  B2 Label2 
**                  ...
**             
**  Each time the button is pressed, the selected checkbox entry 
**  changes his state. 
**  Mutual checkboxes (Mutual attribute) allow only 1 entry activated with 
**  state S_1. All other entries have state S_0.
**
**  The space between checkbox entries is specified with IpadX (H) and 
**  IpadY (V) attributes of the widget, or automatically calculated if
**  AdjustX (H) or AdjustY (V) is specified. 
**
**  All buttons and labels can be configured individually using the
**  Rows attribute: Each row specifies one button+label pair. 
**  The space between the button and the label can be specified there
**  with the IpadX (H/V) attribute.
**
**  If AdjustX & ExpandX is specified, the total width of
**  all button+label pairs are expanded to the offered space by the
**  parent widget, in contrast to the behaviour of boxes! Fixed
**  padding IpadX and IpadY is required, too, and is applied only
**  (IpadX) on the left of the most left, and on the right of the most
**  right entry field.
** 
**    $ENDOFINFO
**
*)
type kind = Horizontal | Vertical
and box_desc = {
  mutable cb_label : string;
  mutable cb_active : bool;
  mutable cb_cur_state : bool;
  mutable cb_but : VX_types.symbol_type list;
  mutable cb_frame : VX_types.frame;
  mutable cb_label_width : int;
  mutable cb_but_size : int;
} 

(*
** Button and labels are configured either with global attributes
** or locally with the Cols array: Each row containes two column entries:
** The button and the label, nevertheless the orintation is!
*)

class orig :
  kind ->
  VX_types.container ->
  string array ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    (*
    ** Attributes: [ActionIU,Text_font,Text_style,Text_size,Mutual,Rows]
    ** Rows: [Size,Active,Sym,IpadX,Frame]
    *)
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method contained : VX_types.contained
    method container : VX_types.container
    method cursor_make : VX_types.cursor_desc -> bool -> VX_types.cursor
    method default_font : VX_types.font
    method destroy : unit
    method display : Xtypes.display
    method draw_frame : unit
    method focus : unit
    method font_make :
      VX_types.text_font ->
      VX_types.text_style -> VX_types.text_size -> bool -> VX_types.font
    method foreground : VX_types.color
    method geometry : Xtypes.geometry
    method getHilite : VX_types.color -> VX_types.color
    method getShadow : VX_types.color -> VX_types.color
    method global_color_make : VX_types.color_desc -> bool -> VX_types.color
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method hide : unit
    method id : string
    method image_get : string -> Ximage.t
    method image_make : string * VX_types.image_desc -> Ximage.t
    method init : unit
    method inverse : unit
    method iter : (VX_types.contained -> unit) -> unit
    method iter_visible : (VX_types.contained -> unit) -> unit
    method name : string
    method normal : unit
    method parent : VX_types.container
    method pixmap_get : string -> Xpm.pixmap
    method pixmap_make : string * VX_types.pixmap_desc -> Xpm.pixmap
    method print : VX_types.ps -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    (*
    ** Is a checkbox entry selected ?
    *)              
    method selected : int -> bool
    (*
    ** Set action handler (called when a selection was done). The action
    ** handler is called with the selected checkbox index.
    *)
    method set_action : (int -> unit) -> unit
    method set_name : string -> unit
    method set_parent : VX_types.container -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    (*
    ** Calculate required width and height of this widget.
    *)
    method size_request : VX_types.szhints
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
    (*
    ** Update the one specified button
    *)
    method update_but : int -> unit
    method update_clipping : unit
    method update_size : unit
    method update_top_size : unit
    method wait_refresh :
      bool ->
      Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
    method wait_resize : unit
    method width : int
    method win : VX_types.window
    method win_request : VX_types.window
    method window : Xtypes.window
    method xevents : Xtypes.xevent -> unit

    (*
    ** Internal checkbox info array with informations about the
    ** state of each checkbox entry.
    *)
    val mutable mutual : bool
    val mutable check_fun : int -> unit
    val mutable check_info : box_desc array

    val id : int
    val mutable inited : bool
    (*
    ** Padding space between button and label text. Padding between
    ** checkbox entries is specified with IpadX and IpadY.
    *)
    val mutable pad_x : int
    val mutable parent : VX_types.container
    val s : VX_types.screen_struct
    val mutable szhints : VX_types.szhints
    val mutable text_font : VX_types.font
    val mutable text_font_symbol : VX_types.font
    val w : VX_types.window
  end

(*
** Horizontal aligned checkbox
*)
class h :
  VX_types.container ->
  string array ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method contained : VX_types.contained
    method container : VX_types.container
    method cursor_make : VX_types.cursor_desc -> bool -> VX_types.cursor
    method default_font : VX_types.font
    method destroy : unit
    method display : Xtypes.display
    method draw_frame : unit
    method focus : unit
    method font_make :
      VX_types.text_font ->
      VX_types.text_style -> VX_types.text_size -> bool -> VX_types.font
    method foreground : VX_types.color
    method geometry : Xtypes.geometry
    method getHilite : VX_types.color -> VX_types.color
    method getShadow : VX_types.color -> VX_types.color
    method global_color_make : VX_types.color_desc -> bool -> VX_types.color
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method hide : unit
    method id : string
    method image_get : string -> Ximage.t
    method image_make : string * VX_types.image_desc -> Ximage.t
    method init : unit
    method inverse : unit
    method iter : (VX_types.contained -> unit) -> unit
    method iter_visible : (VX_types.contained -> unit) -> unit
    method name : string
    method normal : unit
    method parent : VX_types.container
    method pixmap_get : string -> Xpm.pixmap
    method pixmap_make : string * VX_types.pixmap_desc -> Xpm.pixmap
    method print : VX_types.ps -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    method selected : int -> bool
    method set_action : (int -> unit) -> unit
    method set_name : string -> unit
    method set_parent : VX_types.container -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    method size_request : VX_types.szhints
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
    method update_but : int -> unit
    method update_clipping : unit
    method update_size : unit
    method update_top_size : unit
    method wait_refresh :
      bool ->
      Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
    method wait_resize : unit
    method width : int
    method win : VX_types.window
    method win_request : VX_types.window
    method window : Xtypes.window
    method xevents : Xtypes.xevent -> unit
    val mutable check_fun : int -> unit
    val mutable check_info : box_desc array
    val id : int
    val mutable inited : bool
    val mutable mutual : bool
    val mutable pad_x : int
    val mutable parent : VX_types.container
    val s : VX_types.screen_struct
    val mutable szhints : VX_types.szhints
    val mutable text_font : VX_types.font
    val mutable text_font_symbol : VX_types.font
    val w : VX_types.window
  end

(*
** Vertical aligned checkbox
*)
class v :
  VX_types.container ->
  string array ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method contained : VX_types.contained
    method container : VX_types.container
    method cursor_make : VX_types.cursor_desc -> bool -> VX_types.cursor
    method default_font : VX_types.font
    method destroy : unit
    method display : Xtypes.display
    method draw_frame : unit
    method focus : unit
    method font_make :
      VX_types.text_font ->
      VX_types.text_style -> VX_types.text_size -> bool -> VX_types.font
    method foreground : VX_types.color
    method geometry : Xtypes.geometry
    method getHilite : VX_types.color -> VX_types.color
    method getShadow : VX_types.color -> VX_types.color
    method global_color_make : VX_types.color_desc -> bool -> VX_types.color
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method hide : unit
    method id : string
    method image_get : string -> Ximage.t
    method image_make : string * VX_types.image_desc -> Ximage.t
    method init : unit
    method inverse : unit
    method iter : (VX_types.contained -> unit) -> unit
    method iter_visible : (VX_types.contained -> unit) -> unit
    method name : string
    method normal : unit
    method parent : VX_types.container
    method pixmap_get : string -> Xpm.pixmap
    method pixmap_make : string * VX_types.pixmap_desc -> Xpm.pixmap
    method print : VX_types.ps -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    method selected : int -> bool
    method set_action : (int -> unit) -> unit
    method set_name : string -> unit
    method set_parent : VX_types.container -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    method size_request : VX_types.szhints
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
    method update_but : int -> unit
    method update_clipping : unit
    method update_size : unit
    method update_top_size : unit
    method wait_refresh :
      bool ->
      Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
    method wait_resize : unit
    method width : int
    method win : VX_types.window
    method win_request : VX_types.window
    method window : Xtypes.window
    method xevents : Xtypes.xevent -> unit
    val mutable check_fun : int -> unit
    val mutable check_info : box_desc array
    val id : int
    val mutable inited : bool
    val mutable mutual : bool
    val mutable pad_x : int
    val mutable parent : VX_types.container
    val s : VX_types.screen_struct
    val mutable szhints : VX_types.szhints
    val mutable text_font : VX_types.font
    val mutable text_font_symbol : VX_types.font
    val w : VX_types.window
  end

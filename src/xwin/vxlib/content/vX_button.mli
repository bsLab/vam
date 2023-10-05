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
**    $CREATED:     31.5.2005
**    $VERSION:     1.22
**
**    $INFO:
**
**  VXlib button widgets.
**
**  class t: one button with text label
**
**  class table : buttons with label organized in rows and columns
**  
**  Main attributes: IpadX,IpadY are applied both inside the button
**  (space between text label and button border), and between buttons
**  and rows!
** 
**  Either the width of a button (border to border width) is specified
**  with the Width attribute inside the Cols attribute, or the
**  size of each button is either calculated from the label width and
**  inside padding, or the buttons will be expanded to the extents
**  of the window (with specified padding). This requires the ExpandX
**  attribute.     
**
**  Together with the ExpandX attribute and fixed button width autopadding
**  can be enabled with teh AdjustX attribute. The button will be aligned
**  automatically inside the window widget. 
**
**  Single buttons can be grouped using the Group attribute inside the
**  Cols attribute. A unique group number in each button entry specifies
**  all buttons belonging to the same group.
**
**
**    $ENDOFINFO
**
*)

(*
** Single text button
*)

class orig :
  VX_types.container ->
  string ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    (*
    ** Set text attributes
    ** [Text_font,Text_style,Text_size,ActionUU,Shape,Frame,Border]
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
    method get_state : bool
    method global_color_make : VX_types.color_desc -> bool -> VX_types.color
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method hide : unit
    method id : string
    method image_get : string -> Ximage.t
    method image_make : string * VX_types.image_desc -> Ximage.t
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
    method set_action : (unit -> unit) -> unit
    method set_name : string -> unit
    method set_parent : VX_types.container -> unit
    method set_state : bool -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    (*
    ** Get the size of this button widget. Text string area +
    ** shadow and padding. Widget border is not very usefull here!
    *)
    method size_request : VX_types.szhints
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
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
    val mutable action : unit -> unit
    val mutable active : bool
    val id : int
    val mutable inited : bool
    val mutable parent : VX_types.container
    val s : VX_types.screen_struct
    val mutable szhints : VX_types.szhints

    (*
    ** Default text font: generic text and symbol of same size
    *)
    val mutable text_font : VX_types.font
    val mutable text_font_symbol : VX_types.font
    val mutable text_str : string
    val w : VX_types.window
  end
class t :
  VX_types.container -> string -> VX_types.widget_attributes list -> orig

(*
** Button tables
*)

(*
** One column entry of a row
*)

type col_desc = {
    (*
    ** Content
    *)
    mutable col_str : string;
    mutable col_font : VX_types.font;         (* normal text font *)
    mutable col_symbol_font : VX_types.font;  (* embedded symbols font *)
    mutable col_fg : VX_types.color;
    mutable col_bg : VX_types.color;

    (*
    ** Button frame
    *)
    mutable col_shadow : VX_types.frame;

    (*
    ** Requested width in pixel including
    ** shape and inside label padding. Can be zero. Either the column
    ** entry width is fixed or expandable.
    *)
    mutable col_width : int;
    mutable col_expand : bool;
    mutable col_fixed : bool;

    (*
    ** Button action
    *)
    mutable col_action : (unit -> unit) option;

    (*
    ** Is button sunken or raised ?
    *)
    mutable col_active : bool;

    (*
    ** Grouping of buttons (row,col). Only one button
    ** of the list can be activated (sunken).
    *)
    mutable col_group : int option;
} 
and row_desc = {
    (*
    ** each row contains 1..n columns.
    *)
    mutable row_cols : col_desc array;

    (*
    ** Requested height in pixel including
    ** border and column padding   
    *)
    mutable row_height : int;
    (*
    ** Pad space after this row
    *)
    mutable row_pad : int;
} 
and table_desc = {
    mutable tb_rows : row_desc array;
    mutable tb_colpad_x : int;
    mutable tb_colpad_y : int;
    mutable tb_regular : bool;
    (*
    ** Dynamic auto adjustment (space padding between buttons).
    *)
    mutable tb_autopad_x : bool;
} 

class orig2 :
  VX_types.container ->
  table_desc ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method click_type : VX_types.click
    method col_iter : (int -> int -> row_desc -> col_desc -> unit) -> unit
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
    method get_col : int -> int -> col_desc
    method get_state : int -> int -> bool
    method global_color_make : VX_types.color_desc -> bool -> VX_types.color
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method hide : unit
    method id : string
    method image_get : string -> Ximage.t
    method image_make : string * VX_types.image_desc -> Ximage.t
    method inverse : unit
    method iter : (VX_types.contained -> unit) -> unit
    method iter_visible : (VX_types.contained -> unit) -> unit
    method name : string
    method normal : unit
    method parent : VX_types.container
    method pixmap_get : string -> Xpm.pixmap
    method pixmap_make : string * VX_types.pixmap_desc -> Xpm.pixmap
    method print : VX_types.ps -> int -> int -> unit
    method print_but : VX_types.ps -> int -> int -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method realize : unit
    (*
    ** Redraw full button table widget. Recalculate bounding boxes.
    *)
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    method set_action : int -> int -> (unit -> unit) option -> unit
    method set_name : string -> unit
    method set_parent : VX_types.container -> unit
    method set_state : int -> int -> bool -> unit
    method show : unit
    (*
    **
    ** (Re)Size this object and (re)size the X window associated
    ** with this object widget.
    **
    **  Args:
    **      x y dx dy -> Maximal available area!
    **
    *)
    method size_allocate : int -> int -> int -> int -> bool
    (*
    ** Calculate the size of this text widget. Expandable and unsized
    ** columns entries are recalculated. If the height of a row is not
    ** specified, calculate it from the entries of the row, too.
    *)
    method size_request : VX_types.szhints
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
    (*
    ** Update one button
    *)
    method update_but : int -> int -> unit
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
    val id : int
    val mutable parent : VX_types.container
    val s : VX_types.screen_struct
    val mutable szhints : VX_types.szhints
    val table : table_desc
    val w : VX_types.window
  end

(*
** Generates a table descriptor. Not specified parameters are filled
** with default values.
**
** Attributes:
** [IpadX,IpadY,Rows,Cols,AdjustX,Text_font,Text_style,Text_size,
**  But [Shape,Frame,Background,Color,Width], Frame]
**
*)
val table_gen :
  < color_make : string -> bool -> VX_types.color;
    font_make : VX_types.text_font ->
                VX_types.text_style ->
                VX_types.text_size -> bool -> VX_types.font;
    .. > ->
  string array array -> VX_types.widget_attributes list -> table_desc

class table :
  VX_types.container ->
  string array array ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method click_type : VX_types.click
    method col_iter : (int -> int -> row_desc -> col_desc -> unit) -> unit
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
    method get_col : int -> int -> col_desc
    method get_state : int -> int -> bool
    method global_color_make : VX_types.color_desc -> bool -> VX_types.color
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method hide : unit
    method id : string
    method image_get : string -> Ximage.t
    method image_make : string * VX_types.image_desc -> Ximage.t
    method inverse : unit
    method iter : (VX_types.contained -> unit) -> unit
    method iter_visible : (VX_types.contained -> unit) -> unit
    method name : string
    method normal : unit
    method parent : VX_types.container
    method pixmap_get : string -> Xpm.pixmap
    method pixmap_make : string * VX_types.pixmap_desc -> Xpm.pixmap
    method print : VX_types.ps -> int -> int -> unit
    method print_but : VX_types.ps -> int -> int -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    method set_action : int -> int -> (unit -> unit) option -> unit
    method set_name : string -> unit
    method set_parent : VX_types.container -> unit
    method set_state : int -> int -> bool -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    method size_request : VX_types.szhints
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
    method update_but : int -> int -> unit
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
    val id : int
    val mutable parent : VX_types.container
    val s : VX_types.screen_struct
    val mutable szhints : VX_types.szhints
    val table : table_desc
    val w : VX_types.window
  end

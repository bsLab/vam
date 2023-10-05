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
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2005 BSSLAB
**    $CREATED:     24.5.2005
**    $VERSION:     1.34
**
**    $INFO:
**
**  Text table widget. This widget supports text cells either editable
**  or not. The font, the border lines and the width of each cell can
**  be specified. Furthermore, a user supplied callback function can 
**  be specified. Each cell can include a button. The button behaviour
**  depends on the attribute settings:
**
**  1. A status button. The current status of the (editable) text entry
**     is displayed (Modified, Error, Ok...). If the text was modified,
**     the status is updated (Modified). If the user presses teh button,
**     an installed callback function (ActionSSS) is called with the current
**     status and string. This function returns a new status, which is
**     displayed.
**
**
**  2. The button can also be used (for non editable entries) to popup other
**     widgets (another texttable, for example), specified with the
**     Widget attribute and optional ActionUP and ActionDOWN callback
**     functions.  
**
**  3. Generic button with label and action handler (ActionUU).
**
**  4. Generic action handler without a button (ActionSU) called each time
**     the cell content was modified.
**
**  The height of a row can be specified. If this specification is missing,
**  the height is determined automatically from the padding, border and
**  font specification. The maximal value from all column entries of one
**  row is used.
**
**
**  Regular tables: all rows have same number of columns, and
**                  all entries of one column have the same width.
**
**
**  The width of each column entry can be specified with the Width
**  attribute inside the Cols attribute. Either all entries
**  from one column have a specified width (that means the width is fixed),
**  or all entries have width 0 (not specified). All entries from one
**  column must have the same width (regular table), except all 
**  entries have a specified width (irregular table)!
**
**  Column entries can be expanded, specified with the ExpandX attribute
**  inside the Cols attribute. 
**  Editable columns entries have either a specified fixed width or will be
**  expanded according the window size (they consume all available space).
**
**  Both padding of the table inside widget container AND padding of
**  cell content of each entry is specified with IpadX and IpadY attributes
**  in main attribute list (not inside Cols/Rows). IpadY inside Rows
**  attributes specify additional space between rows!!!  
**
**    $ENDOFINFO
**
*)
val warn : string -> unit
(*
** Status button of an editable column entry. Displayed in the right
** side of the text field. Each status (Submitted, Modified,...)
** corresponds to a symbol. The symbols displayed must be specifed.
** Empty symbol list = blank button field.
*)
type status_button = {
    (*
    ** Callback function called if button was pressed.
    *)
    mutable sb_fun : string -> VX_types.status -> VX_types.status;

    (*
    ** The status symbols displayed. If a status symbol is not
    ** contained in the list, the button keeps blank.
    *)
    mutable sb_syms : VX_types.symbol_type list;
    (*
    ** Frame box of button
    *)
    mutable sb_frame : VX_types.frame;
    mutable sb_color : VX_types.color;
    mutable sb_active : bool;
} 
(*
** Generic labeled button with action handler.
*)

and label_button = {
    (*
    ** Callback function called if button was pressed.
    *)
    mutable lb_fun : unit -> unit;
    (*
    ** Label string and fonts
    *)
    mutable lb_label : string;
    mutable lb_font : VX_types.font;
    mutable lb_font_symbol : VX_types.font;
    (*  
    ** Label padding. Default: tb_col_pad_X
    *)
    mutable lb_pad_x : int;
    mutable lb_pad_y : int;

    (*
    ** Bounding box of button
    *)
    mutable lb_frame : VX_types.frame;
    mutable lb_color : VX_types.color;
    mutable lb_active : bool;
}
(*
** Popup widget button 
*)
 
and popup_button = {
    (*
    ** Callback functions called if widget window is opened and closed.
    *)
    mutable pb_fun_up : unit -> unit;
    mutable pb_fun_down : unit -> unit;

    (*
    ** Bounding box of button
    *)
    mutable pb_frame : VX_types.frame;
    mutable pb_color : VX_types.color;
    mutable pb_active : bool;
    (*
    ** Widget window opened (popuped?)
    *)
    mutable pb_opened : bool;
    (*
    ** The widget contained  
    *)
    mutable pb_widget : VX_types.contained option;
    mutable pb_realization : VX_top.t option;
} 
and button =
    Status_but of status_button
  | Label_but of label_button
  | Popup_but of popup_button

(*
** One column entry of a row
*)
and col_desc = {
    (*
    ** Content
    *)    
    (*
    ** The content string can contain embedded symbols if not editable:
    **  "The distance in \\mu meters"
    ** The symbols are displayed with the symbol_font instead.
    ** The symbols are specified in TeX notation.
    *)
    mutable col_str : string;
    mutable col_lines : string list;
    mutable col_multi_line : bool;
    (*
    ** Single or multiline texts:
    **  Number of text rows  = 0 -> auto height calculated multiline text
    **                       = 1 -> single line
    **                       > 1 -> fixed multiline height
    *)
    mutable col_rows : int;
    mutable col_font : VX_types.font;
    mutable col_symbol_font : VX_types.font;
    mutable col_align_x : VX_types.align;
    mutable col_align_y : VX_types.align;
    mutable col_fg : VX_types.color;
    mutable col_bg : VX_types.color;
    (*
    ** Border descriptor
    *)
    mutable col_frame : VX_types.frame;
    (*
    ** Requested width in pixel including 
    ** border and column padding. Can be zero. Either the column
    ** entry width is fixed or expandable.
    *)
    mutable col_width : int;
    mutable col_expand : bool;
    mutable col_fixed : bool;
    mutable col_edit : bool;
    mutable col_baseline : VX_types.text_baseline option;
    (*
    ** An optional button
    *)
    mutable col_but : button option;
    (*
    ** Informations at runtime
    *)
    mutable col_cursor : int;
    mutable col_cursor_display : bool;
    mutable col_lastpad : int;
    (*
    ** Current status of entry (modified ?)
    *)
    mutable col_status : VX_types.status;
    (*
    ** generic action handler called each time the cell content
    ** was modified
    *)
    mutable col_action : string -> unit;
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
    mutable row_width : int;
    mutable row_pad : int;
} 
and table_desc = {
    mutable tb_rows : row_desc array;
    mutable tb_colpad_x : int;
    mutable tb_colpad_y : int;
    (*
    ** Only in case of multiline text with non fixed height the
    ** table widget must be resized if the height of a column entry
    ** changed.
    *)
    mutable tb_height_expand : bool;
} 

val def_but_size : int

(*
** The table widget.
*)
class orig :
  VX_types.container ->
  VX_root.t option ->
  table_desc ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method but_bbox : int -> int -> VX_types.bounding_box
    method but_frame : int -> int -> VX_types.frame
    method but_height : int -> int -> int
    (*
    ** Optional button size (width) including extra
    ** padding space.
    *)
    method but_width : int -> int -> int
    method click_type : VX_types.click
    method col_iter : (int -> int -> row_desc -> col_desc -> unit) -> unit
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list

    (*
    ** Configure one table cell. Only a few attributes can be set at
    ** runtime.
    ** [Border,Foreground,Background,ActionSU,But]
    ** But [Sym,Label,ActionUU,ActionSSS]
    *)
    method configure_col :
      int -> int -> VX_types.widget_attributes list -> unit
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
    (*
    ** Get a column entry
    *)
    method get_col : int -> int -> col_desc
    (*
    ** Get the actual content of the specified column entry.
    *)
    method get_text : int -> int -> string
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
    (*
    ** Popup button support
    *)

    method popdown : int -> int -> unit
    method popup : int -> int -> int -> int -> unit

    (*
    ** Postscript printing
    *)
    method print : VX_types.ps -> int -> int -> unit
    (*
    ** Print button
    *)
    method print_but : VX_types.ps -> int -> int -> int -> int -> unit
    (*
    ** Print one column entry
    *)
    method print_col : VX_types.ps -> int -> int -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method realize : unit
    (*
    ** Draw the graphics content of the full table widget:
    **  text, border, cursor...
    *)
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    method set_name : string -> unit
    method set_parent : VX_types.container -> unit
    (*
    ** Set the text content of one specific column entry.
    *)
    method set_text : int -> int -> string -> unit
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
    (*
    ** The pixel width actually available for the text content of a
    ** column entry.
    *)
    method text_width : int -> int -> int
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
    (*
    ** A column entry can have an optional action button.
    ** Set the state of the button (and redraw content).
    *)
    method update_but : int -> int -> unit
    method update_clipping : unit
    (*
    ** Update and redraw one specific column entry.
    *)
    method update_col : int -> int -> unit
    (*
    **  The cursor got a new position or the display state
    **  changed. In the first case, restore old position, draw new position.
    ** The third argument specifies an optional cursor position change.
    *)
    method update_cursor : int -> int -> int -> unit
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
    ** Cursor and user interaction utilities. Only one table entry
    ** can get the input focus (cursor set).
    *)
    val cursor : string
    val mutable display_cursor : bool
    val id : int
    (*
    ** Last active (row,col) editable entry if any.
    *)
    val mutable last_active : (int * int) option
    (*
    ** For multiline text fields
    *)
    val mutable line_spacing : int
    val mutable parent : VX_types.container
    (*
    ** Popup button implementation needs the root widget!
    *)
    val root : VX_root.t option
    val s : VX_types.screen_struct
    val mutable szhints : VX_types.szhints
    val table : table_desc
    val w : VX_types.window
  end

(*
** Generates a table descriptor. Not specified parameters are filled
** with default values. Sizes are computed in size_request if necessary.
**
** Attributes: [IpadX,IpadY,Rows,Cols,Text_font,Text_style,Text_size]
** Cols [Width,Rown,Border,Text_baseline,Foreground,Background,Mutable,
**       Align,ActionSU,But,Text_font,Text_style,Text_size,ExpandX]
** But [Sym,Widget,Label,ActionSSS,ActionUU,IpadX,IpadY,Frame,Size,
**      Color]
**
*)
val table_gen :
  < color_make : string -> bool -> VX_types.color;
    font_make : VX_types.text_font ->
                VX_types.text_style ->
                VX_types.text_size -> bool -> VX_types.font;
    win : VX_types.window; .. > ->
  string array array -> VX_types.widget_attributes list -> table_desc

(*
** Texttable main class.
*)
class t :
  VX_types.container ->
  string array array ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method but_bbox : int -> int -> VX_types.bounding_box
    method but_frame : int -> int -> VX_types.frame
    method but_height : int -> int -> int
    method but_width : int -> int -> int
    method click_type : VX_types.click
    method col_iter : (int -> int -> row_desc -> col_desc -> unit) -> unit
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method configure_col :
      int -> int -> VX_types.widget_attributes list -> unit
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
    method get_text : int -> int -> string
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
    method popdown : int -> int -> unit
    method popup : int -> int -> int -> int -> unit
    method print : VX_types.ps -> int -> int -> unit
    method print_but : VX_types.ps -> int -> int -> int -> int -> unit
    method print_col : VX_types.ps -> int -> int -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    method set_name : string -> unit
    method set_parent : VX_types.container -> unit
    method set_text : int -> int -> string -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    method size_request : VX_types.szhints
    method text_width : int -> int -> int
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
    method update_but : int -> int -> unit
    method update_clipping : unit
    method update_col : int -> int -> unit
    method update_cursor : int -> int -> int -> unit
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
    val cursor : string
    val mutable display_cursor : bool
    val id : int
    val mutable last_active : (int * int) option
    val mutable line_spacing : int
    val mutable parent : VX_types.container
    val root : VX_root.t option
    val s : VX_types.screen_struct
    val mutable szhints : VX_types.szhints
    val table : table_desc
    val w : VX_types.window
  end

(*
** With additional root window needed for popup button implementation.
*)
class with_root :
  VX_types.container ->
  VX_root.t ->
  string array array ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method but_bbox : int -> int -> VX_types.bounding_box
    method but_frame : int -> int -> VX_types.frame
    method but_height : int -> int -> int
    method but_width : int -> int -> int
    method click_type : VX_types.click
    method col_iter : (int -> int -> row_desc -> col_desc -> unit) -> unit
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method configure_col :
      int -> int -> VX_types.widget_attributes list -> unit
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
    method get_text : int -> int -> string
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
    method popdown : int -> int -> unit
    method popup : int -> int -> int -> int -> unit
    method print : VX_types.ps -> int -> int -> unit
    method print_but : VX_types.ps -> int -> int -> int -> int -> unit
    method print_col : VX_types.ps -> int -> int -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    method set_name : string -> unit
    method set_parent : VX_types.container -> unit
    method set_text : int -> int -> string -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    method size_request : VX_types.szhints
    method text_width : int -> int -> int
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
    method update_but : int -> int -> unit
    method update_clipping : unit
    method update_col : int -> int -> unit
    method update_cursor : int -> int -> int -> unit
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
    val cursor : string
    val mutable display_cursor : bool
    val id : int
    val mutable last_active : (int * int) option
    val mutable line_spacing : int
    val mutable parent : VX_types.container
    val root : VX_root.t option
    val s : VX_types.screen_struct
    val mutable szhints : VX_types.szhints
    val table : table_desc
    val w : VX_types.window
  end

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
**    $CREATED:     16.5.2005
**    $VERSION:     1.27
**
**    $INFO:
**
**  VXlib text widget.
**
**  1. Simple text label (single line)
**  2. Multiline text 
**
**  3. Editable (mutable) text (single line)
**  4. Multliline editable (mutable) text
**
**
**  Simple text: just a string (auto formatted and line broken).
**  The width of the text widget can be calculated from the
**  current text string or explicitly by attributes (MinWidth,
**  Width). 
**  The number of text lines is either determined automatically
**  or fixed with Rown attribute. If the Rown value is 0, the
**  row number is determined automatically, either if > 1 it's a fixed
**  value.
**
**  class t,text : non editable text.
**                  Rown 0      -> multiline text, auto heigth scale
**                  Rown > 1    -> multiline text, fixed number of rows
**                  
**
**  class edit : same as above, but editable
**          
**  class label: simple, single line text widget. A subset of class text.
**  
**
**  Non editable text strings can contain embedded symbols in TeX style
**  format:
**      "This is greek \\alpha and \\mu."
**
**  The appropiate symbol font is derived (in size and style) from the
**  specified generic text font.
**
**
**    $ENDOFINFO
**
*)
val warn : string -> unit

(*
** Break up string into space separated atoms.
*)

val atoms_of_str : string -> string list

(*
** Return list of formatted text lines from atom text list.
** The maximal line_width in pixel units may not be exceeded.
** In the case a text atom string is wider than line_width, this atom
** must be broken!
** Also, a new text string concatenated from the atoms and the remaining
** pixel space in the last line is returned.
*)

val format_lines :
  VX_types.font -> string list -> int -> string list * string * int

(*
** Basic text class shared by text and exit class. Single and multiline text
** display is provided. Embedded symbols with TeX syntax are
** supported in non editable text widgets. A slightly complex dragon...
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
    ** [Rown,Text_font,Text_style,Text_size,Align,Text_baseline,Mutable]
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
    method get_text : string
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
    ** Postscript printing
    *)
    method print : VX_types.ps -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method print_info : int -> unit
    method realize : unit

    (*
    ** Draw the graphics content: text, basline, cursor...
    *)
    method refresh : unit

    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    method set_name : string -> unit
    method set_parent : VX_types.container -> unit

    (*
    ** Set (replace) and get text string.
    *) 
    method set_text : string -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool

    (*
    ** Calculate the size of this text widget:
    **
    **      1. from current string width and text font height ( mult.
    **         with  lines)
    **      2. from min width, min height
    **      3. from max width, max height
    **
    **
    *)

    method size_request : VX_types.szhints

    (*
    ** Pixel width of text content available. Derived from our widget size.
    *)
    method text_width : int

    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
    method update_clipping : unit

    (*
    **  The cursor got a new position. Restore old position,
    **  draw new position.
    *)
    method update_cursor : unit

    (*
    ** Reformat multiline text
    *)
    method update_multiline : unit

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
    val mutable base_line : VX_types.text_baseline option

    (*
    ** Cursor and focus management if any
    *)

    val cursor : string
    val mutable cursor_x : int
    val mutable display_cursor : bool
    val mutable last_cursor : (int * int * char) option

    (*
    ** Fixed text size?
    *)

    val mutable fixed : bool
    val id : int
    val mutable last_text_width : int

    (*
    ** Space between text lines in pixel
    *)

    val mutable line_spacing : int
    val mutable multi_line : bool
    val mutable parent : VX_types.container
    val s : VX_types.screen_struct
    val mutable szhints : VX_types.szhints

    (*
    ** Text alignment 
    *)

    val mutable text_align : VX_types.align

    (*
    ** Content editable ? Set by derived edit widget class.
    *)
    val mutable text_edit : bool

    (*
    ** Default text font: generic text and symbol of same size
    *)

    val mutable text_font : VX_types.font
    val mutable text_font_symbol : VX_types.font

    (*
    ** Remaining pixel space of (last) text line.
    *)
    val mutable text_lastpad : int

    (*
    ** Single or multiline texts:
    **  Number of text rows  = 0 -> auto height calculated multiline text
    **                       = 1 -> single line
    **                       > 1 -> fixed multiline height
    *)

    val mutable text_rows : int
    (*
    ** Text content: 
    **  text_str: one text string (single and multiline)
    **  text_lines: splitted into lines for multiline texts
    *)
    val mutable text_str : string
    val mutable text_lines : string list

    val w : VX_types.window
  end

(*
** None editable simple text. Both single and multiline texts are supported.
*)
class t :
  VX_types.container -> string -> VX_types.widget_attributes list -> orig
class text :
  VX_types.container -> string -> VX_types.widget_attributes list -> orig


(*
** Editable text widget. Both single and multiline texts are supported.
*) 
class edit :
  VX_types.container ->
  string ->
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
    method get_text : string
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
    method print_info : int -> unit
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    (*
    ** Set action handler
    *)
    method set_action : (string -> unit) -> unit
    (*
    ** Place cursor to new position
    *)
    method set_cursor : int -> unit
    method set_name : string -> unit
    method set_parent : VX_types.container -> unit
    method set_text : string -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    method size_request : VX_types.szhints
    method text_width : int
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
    method update_clipping : unit
    method update_cursor : unit
    method update_multiline : unit
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
    ** Optional action handler called any time the string was modified
    *)
    val mutable action : string -> unit
    val mutable base_line : VX_types.text_baseline option
    val cursor : string
    val mutable cursor_x : int
    val mutable display_cursor : bool
    val mutable fixed : bool
    val id : int
    val mutable input_focus : bool
    val mutable last_cursor : (int * int * char) option
    val mutable last_text_width : int
    val mutable last_width : int
    val mutable line_spacing : int
    val mutable multi_line : bool
    val mutable parent : VX_types.container
    val s : VX_types.screen_struct
    val mutable szhints : VX_types.szhints
    val mutable text_align : VX_types.align
    val mutable text_edit : bool
    val mutable text_font : VX_types.font
    val mutable text_font_symbol : VX_types.font
    val mutable text_lastpad : int
    val mutable text_lines : string list
    val mutable text_rows : int
    val mutable text_str : string
    val w : VX_types.window
  end

(*
** Very simple label text widget. For performance reasons outsourced from
** original class above, though orig class contains the label methods, too.
*)

class label :
  VX_types.container ->
  string ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    (*
    ** [Text_font,Text_style,Text_size,Align]
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
    method print_info : int -> unit
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    method set_name : string -> unit
    method set_parent : VX_types.container -> unit
    method set_text : string -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
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
    val id : int
    val mutable parent : VX_types.container
    val s : VX_types.screen_struct
    val mutable szhints : VX_types.szhints
    (*
    ** Text alignment 
    *)
    val mutable text_align : VX_types.align
    (*
    ** Default text font: generic and symbol of same size
    *)
    val mutable text_font : VX_types.font
    val mutable text_font_symbol : VX_types.font
    val mutable text_str : string
    val w : VX_types.window
  end

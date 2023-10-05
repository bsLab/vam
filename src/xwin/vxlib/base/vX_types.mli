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
**    $VERSION:     1.36
**
**    $INFO:
**
**  This module defines the  basic and common types shared by all 
**  VXlib classes.
**
**    $ENDOFINFO
**
*)

open Xlib
open Xtypes


val max : int -> int -> int
val min : int -> int -> int
type handler = unit -> unit


(*
** Default VXlib values
*)
val pixmap_path : string list ref
val double_click_delay : float ref
val delta_move_size : int ref

val default_background : string ref
val default_foreground : string ref
val default_font : string ref


(*
** A rectangular bounding box
**
**   1-------              +--> +X
**   |      |              |   
**   -------2              v +Y
*)
type bounding_box = {
    mutable x1: int;  
    mutable y1: int;       
    mutable x2: int;   
    mutable y2: int;       
}
 
(*
** => width/height: because minimal size is 1 pixel, the width/height
** is always 1 pixel larger!   
*)  

val bbox_to_xywh : bounding_box -> (int*int*int*int)

(*
** Make a bounding box
*)
val bbox_of_xywh : int -> int -> int -> int -> bounding_box

(*
** Check bounding box with specified coordinates (x,y) 
*)
val within_bbox : bounding_box -> int -> int -> bool
(*
** Expand bounding box to hold new point (x,y)
*)
val expand_bbox : bounding_box -> int -> int -> unit
(*
** Get a new empty bounding box
*)
val empty_bbox : unit -> bounding_box

(*
** Postscript printing
*)
type ps = {
    (*
    ** Current bounding box in X pixel units
    *)
    mutable ps_bbox : bounding_box;
    mutable ps_cont : string list;
    mutable ps_file_name : string;
    (*
    ** Print bounding box in micrometer units. The above ps_bbox must
    ** be finally scaled and translated to this print box.
    *)
    mutable ps_print_bbox : bounding_box;

    (*
    ** Scaling factor from X to PS
    *)
    mutable ps_scale : float;
} 


(*
** Window events handled by VXlib
*)

type event_desc =
  | EnterWindow
  | LeaveWindow
  | ButtonPress
  | KeyPress
  | KeyRelease
  | ButtonReleased
  | ButtonMotion
  | PointerMotion
  | Key of Xtypes.keySym * Xtypes.modifiers
  | Button of Xtypes.button * Xtypes.modifiers
  | GrabbedKey of Xtypes.keySym * Xtypes.modifiers
  | GrabbedButton of Xtypes.button * Xtypes.modifiers
  | FocusIn
  | FocusOut

val autorepeat_delay : float ref
val autorepeat_rate : float ref

type click = | Simple | Double | DeltaMove | Other

(*
** Align attribute
*)
type align = 
    (*
    ** Horizontal
    *)
    | Left  
    | Center 
    | Right
    (*
    ** Vertical
    *)
    | Top
    | Middle
    | Bottom


type box_desc = | Horizontal | Vertical

type color = {
    c_name : string;        (* color name *)
    c_pixel : Xtypes.pixel; (* X pixel id and RGB values *)
    c_red : int;  
    c_green : int;
    c_blue : int;
} 


type color_desc = 
    | Namedcolor of string 
    | RgbColor of int * int * int

type pixmap_desc =
    | FromFile of string
    | FromFunction of Xtypes.pixmap * Xtypes.pixmap
    | FromData of Xpm.pixmap_data

(*
** Image descriptor: buffer * width * height
*)

type image_desc =
  | FromGrayPacked of (string*int*int)             (* args: buf*width*height *)
  | FromGrayBuf of (Bytebuf.buffer*int*int)        (* args: buf*width*height *)
  | FromGrayMatrix of ((int array array)*int*int)
  | FromRGBPacked of  (string*int*int)                 
  | FromRGBMatrix of  (((int*int*int) array array)*int*int)

type cursor_desc =
  | FontCursor of int
  | BitmapCursor of string * string
  | NoCursor

(*
** Font descriptor
*)
type text_font =
    | Times             (* classic                  *)
    | Helvetica         (* modern                   *)
    | Courier           (* = Fixed                  *)
    | Fixed             (* fixed width typewriter   *)
    | Symbol
    | NoFont

type text_style =
    | Roman
    | Bold
    | Italic
    | NoStyle

(*
** Text size in pixel
*)
type text_size = int

type font_desc = {
    mutable text_font : text_font;
    mutable text_style : text_style;
    mutable text_size : text_size;
}

(*
** For internal font management
*)
type font = {
    font_class: text_font; 
    font_style: text_style;
    font_size: int;
    (*
    ** X related
    *)
    font_name: string;
    font_id: Xtypes.font;
    font_info: Xtypes.queryFontRep;
    font_ascent: int;
    font_width: int;
    font_height: int 
}

type cursor = { 
    curs_desc: cursor_desc; 
    curs_id: Xtypes.cursor 
}


val noCursor : cursor
val noColor : color
val defColor : color




(*
** Box side
*)
type box_side = B_top | B_bottom | B_left | B_right 

(*
** Widget Frames:
**
**  1. Flat line
**  2. Shadow
**  3. Relief
**
**  Shadows are asymmetric!
**
** bw
** +------+
** |      |||
** +------+||
** ======== sw
**
*)

type frame_type =
    | Flat
    | Plain
    | ShadowSunken
    | ShadowRaised
    | ReliefSunken
    | ReliefRaised

(*
** Frame shape
*)
type frame_shape =
    | S_Oval
    | S_Rect
    | S_Circ

type line_type =
    | L_Solid
    | L_Dotted 
    | L_Dashed 

type frame = {
    (*
    ** Type and shape
    *)
    mutable f_type      : frame_type;
    mutable f_shape     : frame_shape;

    (*
    ** Bounding box of frame.
    *)
    mutable f_bbox      : bounding_box;

    (*
    ** border size of frame (line,shadow,relief)
    *)
    mutable f_size      : int;      

    (*
    ** Foreground, Background (sourrounding) and fill (ground) color.
    **
    **
    ** Flat border: fg=Border  
    ** Shadow : fg=Shadow
    ** Relief: fg=Shadow, aux=Hilite               
    *)
    mutable f_foreground        : color;
    mutable f_background        : color;
    mutable f_fillground        : color;
    mutable f_auxiliary         : color;
    mutable f_black             : color;

    (*
    ** Border linetype 
    *)
    mutable f_line      : line_type;
    (*
    ** Border line sides
    *)
    mutable f_sides    : box_side list;
                            (*
                            ** Empty list: all box sides
                            *)
    (*
    ** f_depth = 1 -> one border line
    ** f_depth = 2 -> two border lines with 2*b_size bg color space 
    **                area between
    ** ...
    *)
    mutable f_depth    : int;
}

(*
** Default solid border and black color of given width.
*)

val default_frame : int -> frame
val empty_frame : frame


(* 
** Text baseline
*)
type text_baseline = {
    mutable tb_width: int;
    mutable tb_color: color;
    mutable tb_linetype : line_type;
} 

(*
** The size constrains of one widget, available public.
*)

type szhints = { 
    mutable requested_width: int;
    mutable requested_height: int;
    mutable min_width: int;
    mutable min_height: int;
    mutable max_width: int;
    mutable max_height: int;
    (*
    ** Allow size expansion if necessary
    *)
    mutable expand_x: bool;
    mutable expand_y: bool;
    (*
    ** Padding around this widget
    *)
    mutable pad_x : int;
    mutable pad_y : int;
    (*
    ** Position of this widget inside parent container (-1=not spec.)
    *)
    mutable pos_x : int; 
    mutable pos_y : int;


    mutable retract_x: bool;
    mutable retract_y: bool;
    mutable inc_x: int;
    mutable inc_y: int;
    mutable comp_timestamp: int }

(*
** Widget container operations
*)
type op = | Append | Replace | Remove


(*
** Generic size scaled symbols (not derived from symbol font). Used for
** example in buttons and checkfields.
*)

type symbol_type =
    | S_OK
    | S_WARN
    | S_ERR
    | S_QUEST
    | S_CROSS
    | S_ENTER
    | S_MINUS
    | S_PLUS
    | S_0
    | S_1
    | S_LOCK
    | S_R of int
    | S_C of int
    | S_UP
    | S_DOWN
    | S_LEFT
    | S_RIGHT
    | S_BUSY
    | S_BLANK

type symbol = {
    mutable sym_type : symbol_type;
    mutable sym_bbox : bounding_box;    (* size and position of symbol *)
    mutable sym_col : color;
    mutable sym_width: int;             (* line width *)
}

val default_symbol : symbol_type -> symbol

(*
** Status of a mutable widget and update actions.
*)
type status =
    | St_Failed
    | St_Modified
    | St_Submitted
    | St_Unknown
    | St_No_status
    | St_Locked
    | St_Busy



(*
** This is the list of attributes which can be used to configure a widget.
** Most of them are useful for all windows, but some may be only useful for
** particular ones. 
**  
** These attributes are given either at the creation of the widget,
** or after the creation with the "configure" method.
** Childs of leaf widgets, like the textable or checkbox
** widgets, can 1. be configured specifying attributes applied to all
** contained child objects (for example the IpadX attribute), or 2. individual
** by passing these attributes to the Rows and Cols array attributes.
** Rows and Cols attributes are only used by leaf widgets, not by node
** widgets like the box widget.   
**  
*)


  
type widget_attributes =
    (*
    ** SIZE HINTS 
    *)
    | Width of int          (* Min=Max fixed Width  *)
    | Height of int         (* Min=Max fixed Height *)
    | MinWidth of int
    | MinHeight of int
    | MaxWidth of int
    | MaxHeight of int
    (*
    ** Allow size expansion if necessary
    *)
    | ExpandX of bool
    | ExpandY of bool
    (*
    ** Dynamically calculation of inside object padding
    *)
    | AdjustX of bool
    | AdjustY of bool
    (*
    ** Outside padding
    *)
    | OpadX of int
    | OpadY of int
    (*
    ** Inside padding
    *)
    | IpadX of int
    | IpadY of int

    (*
    ** Position of this widget inside parent container.
    *)
    | PosX of int
    | PosY of int

    | Align of align
    (*
    ** reserved
    *)
    | IncX of int
    | IncY of int
    | RetractX of bool
    | RetractY of bool
    | Position of int * int
  
    (*
    ** Colors
    *)
    | Background of string
    | Foreground of string
    | Color of string       (* application specific *)
    (*
    ** Cursor
    *)
    | Cursor of cursor_desc  

    (*
    ** Widget border: flat (default),shadow,relief.
    ** Border attributes: Frame, Shape, Size, Color, Width, Line
    *)
    | Border of (widget_attributes list)
    | Frame  of frame_type
    | Shape of frame_shape
    | Line of line_type
    | Sides of (box_side list)

    (*
    ** Text attributes
    *)
    | Text_style of text_style
    | Text_font of text_font
    | Text_size of int
    (*
    ** Optional text baseline (Width, Color, Line)
    *)
    | Text_baseline of (widget_attributes list)

    (*
    ** Configure childs of a leaf widget. They are organized
    ** in rows and columns.
    **
    ** Rows = [| [row1];[row2};... |]
    ** Cols = [|
    **              [| [col1];[col2];... |]; <row1>
    **              [| [col1];[col2];....|]; <row2>
    **              ...
    **        |]
    ** => Cols.(row_i).(col_j)
    **
    ** ForAll [] -> attributes applied to all childs
    *)
    
    | Rows of (widget_attributes list) array
    | Cols of (widget_attributes list) array array
    | ForAll of (widget_attributes list)

    (*
    ** Table attributes (and mutline text...)
    *)
    | Rown of int
    | Coln of int
    (*
    ** Grouping of table entries. Attribute used inside Cols attribute.
    ** Unique group number must be specified.
    *)
    | Group of int

    (*
    ** Can we change the content of a widget/child of a widget ?
    ** For example text fields in texttable widget.
    *)
    | Mutable of bool
    (*
    ** Only one choice possible ? For example in checkbox.
    *)
    | Mutual of bool
    (*
    ** Events
    *)
    | EventMask of op * (eventMask list)
  

    (*
    ** BINDINGS 
    *)
    | Bindings of (event_desc * handler) list


    (*
    ** Button inside widgets can be configured independently with
    ** this list attribute.
    *)
    | But of (widget_attributes list)
    | Active of bool
    | Sym of symbol_type
    | Label of string   
    | Widget of contained

    (*
    ** Not width or height - auxillary size
    *)
    | Size of int

    (*
    ** Generic action functions with different interfaces
    *)
    | ActionUU   of (unit -> unit)
    | ActionSSS  of (string -> status -> status)  
    | ActionSS   of (string -> Amoeba.status) 
    | ActionSU   of (string -> unit)
    | ActionIU   of (int -> unit) 
    | ActionFU   of (float -> unit)
    (*
    ** Notify functions for popup widgets
    *)
    | ActionUP  of (unit -> unit)
    | ActionDOWN of (unit -> unit)

(*
** What a widget container can export
*)
and contained = <
  show : unit;
  hide : unit;
  realize : unit;
  destroy : unit; 
  (*
  ** What the widget want to have...
  ** These ones should be computed
  *)
  size_request : szhints; 
  win_request : window;
  
  (*
  ** Resize the widget. Allocate or modify window resources.
  ** In the case, the widget gets more space than required/requested,
  ** but at least one size dimension is not accepted, this function
  ** returns false (=not accepted, recalculate/request again).
  *)
  size_allocate : int -> int -> int -> int -> bool;
  (*
  ** Get size informations
  *)
  width : int; 
  height : int;

  (*
  ** Reconfigure widget
  *)
  configure : widget_attributes list -> widget_attributes list;
  print : ps -> int -> int -> unit;
  set_parent : container -> unit;

  (*
  ** Update clipping if any
  *)
  update_clipping : unit;
  name : string;
  >

and resize_widget = < update_size : unit >

and refresh_widget = < refresh : unit >

(*
** Type of gc to be cached.
*)
and  gc_cache_type =
        | Fg of Xtypes.pixel
        | Fg_bg of (Xtypes.pixel * Xtypes.pixel)
        (*
        ** line_style: fg,bg,width,style,dashes
        *)
        | Fg_bg_ls  of (Xtypes.pixel *
                        Xtypes.pixel *
                        int * lineStyle * int)
        | Fg_bg_font of (Xtypes.pixel *
                         Xtypes.pixel *
                         Xtypes.font)
        | Empty
(*
** Graphical context cache
*)
and gc_cache = {
        gcs : gc array;
        ids : gc_cache_type array;
        mutable next : int;
        dpy : Xtypes.display;
        root : Xtypes.window;
}

(*
** Screen descriptor
*)
and screen_struct =
  { s_display: Xtypes.display;
    s_screen: Xtypes.screen;
    s_colors: (color_desc, color) Hashtbl.t;
    s_default_color: color;
    s_pixmaps: (string, Xpm.pixmap) Hashtbl.t;
    s_images: (string, Ximage.t) Hashtbl.t;
    s_fonts: (string, font) Hashtbl.t;
    s_default_font: font;
    s_cursors: (cursor_desc, cursor) Hashtbl.t;
    s_gcs: gc_cache;
    mutable s_timestamp: int;
    mutable s_wait_resize: resize_widget list;
    mutable s_wait_refresh: refresh_widget list;
    mutable s_last_click: Xtypes.Xbutton.t;
    s_eloop: Eloop.display }
  
(*
** A widget container 
*)
and container =
  < color_make : string -> bool -> color;
    cursor_make : cursor_desc -> bool -> cursor; 
    default_font : font;
    display : Xtypes.display; 
    font_make : text_font -> text_style -> 
                text_size ->
                bool -> font;
    geometry : Xtypes.geometry; 
    getHilite : color -> color;
    getShadow : color -> color; 
    name : string;
    pixmap_make : string * pixmap_desc -> Xpm.pixmap; 
    pixmap_get : string -> Xpm.pixmap;    
    image_make : string * image_desc -> Ximage.t; 
    image_get : string -> Ximage.t;    
    screen : screen_struct;
    update_top_size : unit; 
    wait_resize : unit; 
    update : unit;
    win : window;
    window : Xtypes.window; >

and container_add =
  < color_make : string -> bool -> color;
    cursor_make : cursor_desc -> bool -> cursor; default_font : font;
    display : Xtypes.display; 
    font_make : text_font -> text_style -> 
                text_size ->
                bool -> font;
    geometry : Xtypes.geometry; getHilite : color -> color;
    getShadow : color -> color; name : string;
    pixmap_make : string * pixmap_desc -> Xpm.pixmap; 
    pixmap_get : string -> Xpm.pixmap; 
    image_make : string * image_desc -> Ximage.t; 
    image_get : string -> Ximage.t;    
    screen : screen_struct;    
    update_top_size : unit; 
    wait_resize : unit; 
    update : unit;
    window : Xtypes.window;
    container_add : contained -> unit;
  >

(*
** The winodw settings associated with a widget
*)
  
and window = { 
    (*
    ** Drawable window
    *)
    mutable w_window: Xtypes.window;
    mutable w_shown: bool;
    mutable w_clear: bool;
    mutable w_clipped : bool;
    w_geometry: Xtypes.geometry;
    w_clipping : Xtypes.geometry;    
    mutable w_override: bool;
    mutable w_background: color;
    mutable w_frame: frame;
    mutable w_cursor: cursor;
    mutable w_mask: Xtypes.eventMask list;
    mutable w_size_timestamp: int;
    mutable w_refresh_timestamp: int;
    mutable w_foreground: color;
    mutable w_inverse: bool;
    mutable w_enter_window: handler;
    mutable w_leave_window: handler;
    mutable w_button_press: handler;
    mutable w_key_press: handler;
    mutable w_key_release: handler;
    mutable w_button_released: handler;
    mutable w_button_motion: handler;
    mutable w_pointer_motion: handler;
    mutable w_focus_in: handler;
    mutable w_focus_out: handler;
    mutable w_actions: (event_desc * handler) list;

    (*
    ** Inner object Padding - the space between WOBs within
    ** a widget container, for example a hbox container.
    *)

    mutable w_ipad_x: int;
    mutable w_ipad_y: int;
    (*
    ** Automatic padding
    *)
    mutable w_adjust_x : bool;
    mutable w_adjust_y : bool;

    mutable w_size_modified: bool 
}

val hex : char -> int
val loop : unit -> unit
val not_grab_related : Xtypes.notifyMode -> bool
val last_click : Xtypes.Xbutton.t ref


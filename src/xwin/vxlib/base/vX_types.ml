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
**    $VERSION:     1.37
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

(*
** Default VXlib values
*)  
let default_background = ref "white"
let default_foreground = ref "black"
let default_font = ref "fixed"

let pixmap_path = ref ([] : string list)
let double_click_delay = ref 250.
let delta_move_size = ref 1

                                                                     

open Xtypes
  
let max (x : int) (y : int) = if x>y then x else y
let min (x : int) (y : int) = if x<y then x else y
type handler = unit -> unit

  
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

let bbox_to_xywh bbox = bbox.x1,bbox.y1,(bbox.x2-bbox.x1+1),
                                        (bbox.y2-bbox.y1+1)
let bbox_of_xywh x y w h = {x1=x;y1=y;x2=x+w-1;y2=y+h-1}
let within_bbox bbox x y = (x >= bbox.x1 && x <= bbox.x2) &&
                           (y >= bbox.y1 && y <= bbox.y2)

let expand_bbox bbox x y =
    bbox.x1 <- min bbox.x1 x;
    bbox.y1 <- min bbox.y1 y;
    bbox.x2 <- max bbox.x2 x;
    bbox.y2 <- max bbox.y2 y

let empty_bbox () =
    {
        x1=0;y1=0;
        x2=0;y2=0;
    }

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
    | Key of keySym * modifiers
    | Button of button * modifiers
    | GrabbedKey of keySym * modifiers
    | GrabbedButton of button * modifiers
    | FocusIn
    | FocusOut
  
  
let autorepeat_delay = ref 0.06
let autorepeat_rate = ref 0.05

type click = Simple | Double | DeltaMove | Other
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

type box_desc = Horizontal | Vertical  

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
    | FromGrayPacked of (string*int*int)
    | FromGrayBuf    of (Bytebuf.buffer*int*int)
    | FromGrayMatrix of ((int array array)*int*int)
    | FromRGBPacked  of (string*int*int)
    | FromRGBMatrix  of (((int*int*int) array array)*int*int)
  
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
** Size in pixel
*)
type text_size = int 

type font_desc = {
    mutable text_font : text_font;
    mutable text_style : text_style;
    mutable text_size : text_size;
}

type font = {
    font_class: text_font;
    font_style: text_style;
    font_size: int;
    (*
    ** X related
    *)
    font_name : string;
    font_id : Xtypes.font;
    font_info : Xtypes.queryFontRep;
    font_ascent : int;
    font_width : int;
    font_height : int;
}

type cursor = {
    curs_desc : cursor_desc;
    curs_id : Xtypes.cursor;
}

let noCursor = {
    curs_desc = NoCursor;
    curs_id = noCursor;
}

let noColor = {
    c_name = "";
    c_pixel = Xtypes.id_to_pixel 0;
    c_red = 0;
    c_green = 0;
    c_blue = 0;
}

let defColor = noColor

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
    mutable f_bbox       : bounding_box;

    
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
let default_frame width = 
    {f_type=Flat;
     f_shape=S_Rect;
     f_bbox=empty_bbox();
     f_size=width;
     f_foreground = noColor;
     f_background = noColor;
     f_fillground = noColor;
     f_black = noColor;
     f_auxiliary = noColor;
     f_line=L_Solid;
     f_sides=[];f_depth=1} 

let empty_frame =
    {f_type=Plain;
     f_shape=S_Rect;
     f_bbox=empty_bbox();
     f_foreground = noColor;
     f_background = noColor;
     f_fillground = noColor;
     f_auxiliary = noColor;
     f_black = noColor;
     f_size=0;
     f_line=L_Solid;
     f_sides=[];f_depth=0} 


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
    mutable requested_width : int;
    mutable requested_height : int;
    mutable min_width : int;
    mutable min_height : int;
    mutable max_width : int;
    mutable max_height : int;
    (*
    ** Allow size expansion if necessary
    *)
    mutable expand_x : bool;
    mutable expand_y : bool;
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

    mutable retract_x : bool;
    mutable retract_y : bool;
    mutable inc_x : int;
    mutable inc_y : int;
    mutable comp_timestamp : int;
}  

(*
** Widget container operations
*)
  
type op = Append | Replace | Remove




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

let default_symbol sym = {
    sym_type = sym;
    sym_bbox = bbox_of_xywh 0 0 0 0;
    sym_col = noColor;
    sym_width = 1;
}

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
  
and screen_struct = {
    s_display : Xtypes.display;
    s_screen : Xtypes.screen;
    
    s_colors : (color_desc, color) Hashtbl.t;
    s_default_color : color;
    
    s_pixmaps : (string, Xpm.pixmap) Hashtbl.t;
    s_images : (string, Ximage.t) Hashtbl.t;
    
    s_fonts : (string, font) Hashtbl.t;
    s_default_font : font;
    
    s_cursors : (cursor_desc, cursor) Hashtbl.t;
    s_gcs : gc_cache;
    
    mutable s_timestamp : int;
    mutable s_wait_resize : resize_widget list;
    mutable s_wait_refresh : refresh_widget list;
    
    mutable s_last_click : Xbutton.t;
    s_eloop : Eloop.display;
  }

and  container = <
  screen : screen_struct;
  color_make : string -> bool -> color;
  cursor_make : cursor_desc -> bool -> cursor;
  display : Xtypes.display; 
  font_make : text_font -> text_style -> 
              text_size ->
              bool -> font;
  pixmap_make : string * pixmap_desc -> Xpm.pixmap;
  pixmap_get : string -> Xpm.pixmap;
  image_make : string * image_desc -> Ximage.t; 
  image_get : string -> Ximage.t;    
  update_top_size : unit;
  window : Xtypes.window; 
  win : window;
  geometry : geometry;
  getShadow : color -> color;
  getHilite : color -> color;
  default_font : font;
  wait_resize : unit;
  update : unit;
  name : string;
  >

and  container_add = <
  screen : screen_struct;
  color_make : string -> bool -> color;
  cursor_make : cursor_desc -> bool -> cursor;
  display : Xtypes.display; 
  font_make : text_font -> text_style -> text_size -> bool -> font;
  pixmap_make : string * pixmap_desc -> Xpm.pixmap;
  pixmap_get : string -> Xpm.pixmap;
  image_make : string * image_desc -> Ximage.t; 
  image_get : string -> Ximage.t;    
  update_top_size : unit;
  window : Xtypes.window; 
  geometry : geometry;
  getShadow : color -> color;
  getHilite : color -> color;
  default_font : font;
  wait_resize : unit;
  update : unit;
  name : string;
  container_add : contained -> unit;
  >
(*
** The winodw settings associated with a widget
*)  
and window = {
    (*
    ** Drawable window
    *)
    mutable w_window : Xtypes.window;
    mutable w_shown : bool;
    mutable w_clear : bool;
    mutable w_clipped : bool;
    w_geometry : geometry;  
    w_clipping : geometry;
    mutable w_override : bool;
    mutable w_background : color;
    mutable w_frame : frame;
    mutable w_cursor : cursor;
    mutable w_mask : eventMask list;
    mutable w_size_timestamp : int;
    mutable w_refresh_timestamp : int;
    mutable w_foreground : color;
    mutable w_inverse : bool;
    
    mutable w_enter_window : handler;
    mutable w_leave_window : handler;
    mutable w_button_press : handler;
    mutable w_key_press : handler;
    mutable w_key_release : handler;
    mutable w_button_released : handler;
    mutable w_button_motion : handler;
    mutable w_pointer_motion : handler;
    mutable w_focus_in : handler;
    mutable w_focus_out : handler;
    mutable w_actions : (event_desc * handler) list;
    (*
    ** Inner object Padding - the space between WOBs within
    ** a widget container, for example a hbox container.
    *)
    mutable w_ipad_x : int;
    mutable w_ipad_y : int; 
    (*
    ** Automatical padding
    *)
    mutable w_adjust_x : bool;
    mutable w_adjust_y : bool;

    mutable w_size_modified : bool;
  }

let hex c =
  let c = Char.lowercase c in
  if c >='0' && c<='9' then Char.code c - Char.code '0' else
  if c >= 'a' && c<='f' then Char.code c - Char.code 'a' else
    raise Not_found

let loop = Eloop.event_loop 


(*
**  KeyPressMask; KeyReleaseMask;
**  ButtonPressMask; ButtonReleaseMask;
*)
  

let not_grab_related mode = mode <> NotifyGrab && mode <> NotifyUngrab
  
let last_click = ref
    {
    Xbutton.detail = 0;
    Xbutton.time = currentTime;
    Xbutton.root = noWindow;
    Xbutton.event = noWindow;
    Xbutton.child = noWindow;
    Xbutton.x_event = 0;
    Xbutton.y_event = 0;
    Xbutton.x_root = 0;
    Xbutton.y_root = 0;
    Xbutton.state = 0;
    Xbutton.same_screen = false;
  }
  


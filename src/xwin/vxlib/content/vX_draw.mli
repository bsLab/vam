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
**    $CREATED:     17.7.2005
**    $VERSION:     1.07
**
**    $INFO:
**
**  Generic draw widget. The geometry of the actual widget is hidden.
**  Instead, normalized coordinates in the range from 0.0 .. 1.0 are
**  used. The origin is natural like in a x-y plot:
**
**  ^y
**  +--------+ (1.0,1.0)  
**  |        |
**  |        |
**  +--------+-> x
**  (0.0,0.0)
**
**
**  Coordinates can be transformred: scaling, translation, rotation
**
**  Drawing elements are collected in so called paths. Transformations
**  are applied to these paths in the order they appear.
**
**    $ENDOFINFO
**
*)
(*
** Various draw and transformation descriptors
*)
open VX_types

type point = {
    mutable xp : float;
    mutable yp : float;
}
val pt : float -> float -> point

type circle = {
    mutable cp : point;
    mutable rx : float;
    mutable ry : float;    
} 

type text = {
    mutable t_rp : point;       (* reference point              *)
    mutable t_an : float;       (* clock direction angle preset *)
}


type frame = { 
    mutable f_bc : string;      (* background (fill) color  *)
    mutable f_fc : string;      (* border color             *)
    mutable f_bw : float;       (* border width             *)
    mutable f_bx : bounding_box option; (* initial None     *)
}

type translation = {
    mutable dx : float;
    mutable dy : float;
}

type rotation = {
    mutable rc : point;         (* rotation center          *)
    mutable phi : float;        (* clock direction angle !  *)
}


type scale = {
    mutable sc : point;         (* scaling center           *)
    mutable sx : float;         (* scaling factor in x-dir. *)
    mutable sy : float;         (* scaling factor in y-dir. *)
}
 
type transformation =
    | T of translation
    | R of rotation
    | S of scale
    | M         (* mirror y = 1.0 - y *)



type style = 
    (*
    ** Color style
    *)
    | PC of string          (* pen color                *)
    | FC of string          (* background fill color    *)
    (*
    ** Text style
    *)
    | Font_name of text_font
    | Font_style of text_style
    | Font_size of float    (* normalized to 1.0 ! *)

    | AL of (align list)        (* alignment *)

    | Up
    | Down

    (*
    ** Line style
    *)
    | Line of line_type
    | LW of float       (* normalited to 1.0 ! *)

    | FILL                (* fill with FC mode *)

(*
** The main draw type
*)
type draw = 
    (*
    ** Transformation of a path
    *)
    | X of (transformation * draw list)

    (*
    ** Line path
    *)
    | LP of (point list)
    (*
    ** Circle
    *)
    | CR of circle

    (*
    ** A Framebox around the content
    *)
    | F of (frame * draw list)

    (*
    ** Drawing style
    *)
    | Y of (style list * draw list)

    (*
    ** Text box
    *)
    | TX of (text * string) 


val pi : float
val d2r : float -> float
val fr2i : float -> int
(*
** Iterate a path descriptor. Apply all transformations in
** specified order (from right to left!). A new path descriptor
** is returned with removed transformation descriptors.
** Example:
**
**  [ R (r, [T (t, [LP [p1;p2;p3]])]) ] -> [ LP [p1';p2';p3'] ]
*)

val transform_path : draw list -> draw list

(*              
** Get an unmodified copy of the specified path.
*)
val path_copy : draw list -> draw list

(*
** Calculate frame boundaries (if any). Must be done after paths_t were
** calculated (due some uncertainy of text bounding boxes).
*)
val expand_frames : container -> draw list list -> unit

type draw_style = {
  mutable y_text_font : VX_types.font;
  mutable y_text_font_symbol : VX_types.font;
  mutable y_font_desc : VX_types.font_desc;
  mutable y_align_h : VX_types.align;
  mutable y_align_v : VX_types.align;
  mutable y_foreground : VX_types.color;
  mutable y_background : VX_types.color;
  mutable y_linewidth : int;
} 
(*
** Main drawing class
*)
class t :
  VX_types.container ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    (*
    ** Add a new drawing path.
    *)
    method add_path : draw list -> unit
    method background : VX_types.color
    (*
    ** Clear draw area
    *)
    method clear : unit
    method clear_paths : unit
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method contained : VX_types.contained
    method container : VX_types.container
    method cursor_make : VX_types.cursor_desc -> bool -> VX_types.cursor
    method default_font : VX_types.font
    (*
    ** Remove one path - restore drawing region below this path
    *)    
    method delete_path : int -> unit
    method destroy : unit
    method display : Xtypes.display
    (*
    ** Return frame box of draw area 
    *)
    method draw_bbox : VX_types.bounding_box
    method draw_frame : unit
    (*
    ** Draw one path. With 'ersae' flag set the background color of the widget
    ** is used for drawing. Needed by the delete_path method.
    *)
    method draw_path : draw list -> bool -> unit
    (*
    ** Draw all (already transformed) drawing paths.
    *)
    method draw_paths : unit
    (*
    ** Exchange a path
    *)
    method exchange_path : int -> draw list -> unit

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
    method private new_draw_style : draw_style
    method normal : unit
    method parent : VX_types.container
    (*
    ** Return style of parant environment from actual style list
    *)
    method parent_style : draw_style
    method pixmap_get : string -> Xpm.pixmap
    method pixmap_make : string * VX_types.pixmap_desc -> Xpm.pixmap
    (*
    ** Postscript printing 
    *)
    method print : VX_types.ps -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method print_paths : VX_types.ps -> int -> int -> unit
    method realize : unit
    method refresh : unit
    (*
    ** Restore previous style settings
    *)
    method private restore_style : unit
    method reverse : bool
    method root_coordinates : int * int
    (*
    ** Save current style settings.
    *)
    method private save_style : unit
    method screen : VX_types.screen_struct
    method set_name : string -> unit
    method set_parent : VX_types.container -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    method size_request : VX_types.szhints
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    (*
    ** Transform all paths to actual window geometry.
    *)
    method transform_paths : unit
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
    val mutable last_geom : Xtypes.geometry
    val mutable parent : VX_types.container
    val mutable path_transform : bool
    (*
    ** Original normalized paths
    *)
    val mutable paths : draw list list
    (*
    ** Copy of last transformed copy of original path.
    ** Stay actual as long as the draw geometry stays unchanged.
    *)
    val mutable paths_t : draw list list
    val s : VX_types.screen_struct
    (*
    ** Current style defintions
    ** May change during path evaluation!
    *)
    val mutable style : draw_style
    (*
    ** During path evaluation different style can be used in a stacked
    ** fashion. Remember style history. Top of list always our initial
    ** =  default settings.
    *)
    val mutable styles : draw_style list
    val mutable szhints : VX_types.szhints
    val w : VX_types.window
  end

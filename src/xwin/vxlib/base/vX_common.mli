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
**    $CREATED:     ??
**    $VERSION:     1.07
**
**    $INFO:
**
**
**    $ENDOFINFO
**
*)

open VX_types

val i2f : int -> float
val f2i : float -> int
val fmin : float -> float -> float
val fmax : float -> float -> float

val vx_warn : bool ref

(*
** Grahical Context Cache shared by several widgets (box, text, ...):
** Get a context with the following parameters:
**
**  foreground color    -> pixel_to_id col  -> Fg   
**  background color    -> pixel_to_id col  -> Fg_bg
**  font                -> font_to_id font_id   -> Fg_bg_font
**
**  linewidth           -> int -> Fg_bg_ls
**  linestyle           -> LineSolid,LineOnOffDash,LineDoubleDash
**  dashwidth           -> int (ignored when = 0)
*)

module GCCache :
  sig
    val size : int
    val create : Xtypes.display -> Xtypes.screen -> gc_cache
    val get_fg_bg_lw : gc_cache   -> Xtypes.pixel -> 
                                     Xtypes.pixel -> 
                                     Xtypes.pixel -> Xtypes.gc
    val get_fg_bg_font : gc_cache -> Xtypes.pixel -> 
                                     Xtypes.pixel -> 
                                     Xtypes.font -> Xtypes.gc
    val get_fg_bg : gc_cache -> Xtypes.pixel -> 
                                Xtypes.pixel -> Xtypes.gc
    val get_fg : gc_cache -> Xtypes.pixel -> Xtypes.gc
    val get_gc : gc_cache -> gc_cache_type -> Xtypes.gc
  end

(*
** Return a font descriptor of specified font
*)

val font_desc : VX_types.font -> VX_types.font_desc

(*   
** Return a X11 font name
*)
val font_name : VX_types.text_font -> VX_types.text_style ->
                VX_types.text_size -> string
(*
** Return nearest avaiable size (must be kept consistent with font_name sizing)
*)

val font_size : int -> int

(*
** Create a frame structure from Border attribute list.
*)

val create_frame : VX_types.window -> 
                   (string -> VX_types.color) ->
                   VX_types.widget_attributes list -> 
                   VX_types.frame

(*
** Create text baseline structure from attributes.
*)

val create_baseline : VX_types.window -> 
                      (string -> VX_types.color) ->
                      VX_types.widget_attributes list ->
                      VX_types.text_baseline

(*
** Does bounding box area overlaps with clipping region ?
*)

val clip_visible : Xtypes.geometry -> VX_types.bounding_box -> bool

val delta_move_size : int ref

(*
** Draw routines (some based on XMU library)
*)

val drawRoundedRectangle : Xtypes.display ->
                           Xtypes.window ->
                           Xtypes.gc ->
                           int -> int -> int -> int -> int -> int -> unit

val fillRoundedRectangle : Xtypes.display ->
                           Xtypes.window ->
                           Xtypes.gc ->
                           int -> int -> int -> int -> int -> int -> unit

(*
** Postscript printing
*)
val printRoundedRectangle : ps ->
                           int -> int ->
                           color -> float ->
                           float -> float -> float -> float -> 
                           float -> float -> unit

val printFilledRoundedRectangle : ps ->
                           int -> int ->
                           color ->
                           float -> float -> float -> float -> 
                           float -> float -> unit

val set_grabs :
  Xtypes.display -> Xtypes.window -> (event_desc * 'a) list -> unit

val unset_grabs :
  Xtypes.display -> Xtypes.window -> (event_desc * 'a) list -> unit

val mouse_x_event : int ref
val mouse_y_event : int ref
val button_event : int ref
val key_string : string ref
val key_sym : int ref
val modifiers_event : int ref

type null = <  >
val null_handler : unit -> unit

val default_click : Xtypes.Xbutton.t

(*
** Frame drawing : Border (flat line), Shadow, Relief.
** Main entry for frame drawing.
**
**  Flat (solid line) border: no background is filled
**  Shadow border: background is filled
**  Relief: no background is filled    
**
**  fill: if true, in all three cases the frame background is filled
**        with f_fillground color.
*)

val drawFrame :
  Xtypes.display ->
  window -> gc_cache -> frame -> bool -> unit

(*
** Main entry for frame printing
**
**  Flat (solid line) border: no background is filled
**  Shadow border: background is filled
**  Relief: no background is filled    
**
**  fill: if true, in all three cases the frame background is filled
**        with f_fillground color.
*)

val printFrame :
  ps -> window -> int -> int -> frame -> bool -> unit

(*
** Return total frame size (sum for both sides x/y)
*)
val frame_size : frame -> int

(*
** Return offset (x & y) of usable area inside depending on current 
** frame type
*)
val frame_offset : frame -> int

(*
** Generic symbols. Simple symbols like +,-,! are  generated by
** vector commands, other more complicated like ? with text fonts.
*)
val drawSymbol :
  Xtypes.display ->
  window -> gc_cache -> symbol -> unit

(*
** Postscript version
*)

val printSymbol :
  ps -> Xtypes.display -> window -> int -> int -> symbol -> unit

(*
** Text string utils
*)

(*
** Translate embedded tex symbol to corresponding character string
** from symbol font.
*)

val tex_translate : string -> string

(*
** Return the length of a string in pixel units. Depends on current
** font setting.
*)

val string_width : font -> string -> int

(*
** Same as above, but with optional embedded symbols in tex like style
**
**  "The distance in \\mu m."
**
** A symbol font of same size must be provided.
*)

val string_width_S : font -> font -> string -> int

(*
** Maximal width for text line list.
*)
val string_width_max : font -> string list -> int


(*
** Embedded symbol support version.
*)
val string_width_max_S : font -> font -> string list -> int


(*
** Mean width of a line with n times 'o' chars.
*)

val string_width_Xn : font -> int -> int

(*
** Draw a text string at specified position. Can contain embedded
** symbols.
*)
val draw_string_S : Xtypes.display -> Xtypes.window -> gc_cache ->
                  int -> int -> Xtypes.pixel -> Xtypes.pixel ->
                  VX_types.font -> VX_types.font ->
                  string -> unit

(*
** Print string (PS)
*)
val print_string_S : VX_types.ps -> int -> int ->
                   float -> float -> VX_types.color -> VX_types.color ->
                   VX_types.font -> VX_types.font ->
                   string -> unit

(*
** Print an attribute warning : message attribute myname
*)
val warn_attr : string -> widget_attributes -> string -> unit

(*
** Fatal error - abort with message
*)

val vx_error : string -> 'a

(*
** Remove newlines from a text string
*)

val no_nl : string -> string

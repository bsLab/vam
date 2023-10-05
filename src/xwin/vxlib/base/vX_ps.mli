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
**    $CREATED:     3.6.2005
**    $VERSION:     1.08
**
**    $INFO:
**
**  Main Postscript output module.
**
**  We must distinguish two different coordinate systems:
**
**  X --x      y 
**  |          |
**  y          P--x
**
**    $ENDOFINFO
**
*)

open VX_types

type paper_format =
    | A4


(*
** Draw a poly line.
** ps x0 y0 linewidth line_list
*)
val polyLine : ps -> int -> int -> float -> VX_types.color -> 
              (float*float) list -> unit

(*
** Fill an area surrounded by a poly line.
** ps x0 y0 linewidth line_list
*)

val fillPoly : ps -> int -> int -> VX_types.color -> 
               (float*float) list -> unit

(*
** Fill areas surrounded by a rectangle.
** ps x0 y0 color (x y w h) list
*)

val polyFillRectangle : ps -> int -> int -> VX_types.color -> 
                        (float * float * float * float) list -> unit



(*
** Create an Arc.
*)

val polyArc : ps -> int -> int -> float -> VX_types.color ->
               (float*float*float*float*float*float) list -> unit

(*
** Create a filled Arc.
*)
val polyFillArc : ps -> int -> int -> VX_types.color ->
               (float*float*float*float*float*float) list -> unit


(*
** Above Arc version can only handle non elliptic circles, but with
** arbitary start and end angles. The following functions can
** handle elliptic full circles.
*)

(*
** Create an Circle. Delta angle must be 360.0 !!
*)
  
val polyCircle : ps -> int -> int -> float -> VX_types.color ->
               (float*float*float*float*float*float) list -> unit

(*
** Create a filled Circle. Delta angle must be  360.0 !!
*)
val polyFillCircle : ps -> int -> int -> VX_types.color ->
               (float*float*float*float*float*float) list -> unit

(*
** Print a text string with specified font.
** ps x0 y0 tx ty fg bg text_font text_str
*)
val print_text : ps -> int -> int ->
                 float -> float -> color -> color ->
                 font -> 
                 string -> unit

(*
** Enable and disable clipping boxes.
**
** ps x0 y0 tx ty dx dy
*)
val enable_clipping : ps -> int -> int -> 
                      float -> float -> float -> float -> unit

val disable_clipping : ps -> int -> int -> unit

(*
** Print content of specified container and all his childs to the
** file specified. The image is scaled to desired paper size (including
** reasonable margins). Width and height in millimeters.
**
** Default scaling: 1 X pixel unit = 1/72 inch = 1 PS pixel unit
*)

val print_ps : string -> paper_format -> VX_types.contained -> unit

(*
** Print instead an encapsulated PS file. The scaling factor must be
** provided. Default: 1.0
*)

val print_eps : string -> float -> VX_types.contained -> unit


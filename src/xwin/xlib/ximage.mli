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
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) BSSLAB
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.04
**
**    $INFO:
**
** Generic image support. Derived from the Zpixmap module.
** Images are stored either in packed format in a generic string buffer,
** or with array matrixes.
**
** Each time an image is put to the screen, this image must
** be converted to the display depth and probably a color index
** transformartion must be performed (display_depth = 8,16 BPP).
**
** Packed images:
**
** Gray level images consume one byte per pixel, RGB images
** consume 3 bytes per pixel [R,G,B].
**
**
**    $ENDOFINFO
**
*)

(*
** Image format types.
*)

type image_type =
    Image_Gray_Packed of string             |
    Image_Gray_Matrix of int array array    |
    Image_RGB_Packed  of string             |    
    Image_RGB_Matrix  of (int*int*int) array array |
    Image_Gray8_Buf of Bytebuf.buffer |
    Image_Gray16_Buf of Bytebuf.buffer |
    Image_RGB24_Buf of Bytebuf.buffer  

type color_type =
    Color_Gray      |
    Color_RGB       

type image_store_type =
    Image_Packed    |
    Image_Matrix

open Xtypes

exception DepthNotSupported
type t =
  { 
    (*
    ** The real image of depth image_depth.
    *)
    mutable imagebuf: image_type;
    mutable image_store: image_store_type;

    (*
    ** The X pixmap
    *)

    mutable xpixmap: pixmap;

    (*
    ** The zpixmap raw image of depth display_depth.
    *)
    mutable rawimage: string;

    (* informations about the image and the display *)

    iwidth: Xtypes.size;
    iheight: Xtypes.size;
    idepth : int;

    image_bits_per_pixel: int;
    image_bytes_per_line: int;

    display_bits_per_pixel: int;
    display_bytes_per_line: int;

    (*
    ** Color index table (only needed for display depth 8,16) 
    *)

    color_map: int array;
    color_type: color_type;

    (* was the image modified ? *)
    mutable imodified: bool 
  }


(*
** Create a new Image. Setup the color index map array if
** needed.
** Arguments: 
**          dpy : X display
**          cmap: X color map to be used
**          width/height: -:-
**          color_format: Color_Gray ...
**          store_format: the real image storage format (Image_Packed...)
**
*)

val createImage : 
        display -> 
	window ->
        colormap ->
        Xtypes.size -> 
        Xtypes.size -> 
        int -> 
        color_type ->
        image_store_type ->
        t

val createImageFromGrayPacked : 
        display ->
	window -> 
        colormap ->
        Xtypes.size -> 
        Xtypes.size -> 
        int -> 
        string ->
        t

val createImageFromGrayBuf : 
        display ->
	window -> 
        colormap ->
        Xtypes.size -> 
        Xtypes.size -> 
        int -> 
        Bytebuf.buffer ->
        t

val createImageFromGrayMatrix : 
        display ->
	window -> 
        colormap ->
        Xtypes.size -> 
        Xtypes.size -> 
        int -> 
        int array array ->
        t

val createImageFromRGBPacked : 
        display ->
	window -> 
        colormap ->
        Xtypes.size -> 
        Xtypes.size -> 
        int -> 
        string ->
        t

val createImageFromRGBMatrix : 
        display -> 
	window ->
        colormap ->
        Xtypes.size -> 
        Xtypes.size -> 
        int -> 
        (int*int*int) array array ->
        t


(*
** Put the image to the screen.
*)

val putImage :
        display ->
        window ->
        gc ->
        coord ->
        coord ->
        t ->
        unit

 
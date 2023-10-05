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

open Bytebuf




(*
** Image format types. 
*)

type image_type = 
    Image_Gray_Packed of string             |
    Image_Gray_Matrix of int array array    |
    Image_RGB_Packed  of string             |
    Image_RGB_Matrix  of (int*int*int) array array  |
    Image_Gray8_Buf of buffer |
    Image_Gray16_Buf of buffer |
    Image_RGB24_Buf of buffer 
      
type color_type =
    Color_Gray      |
    Color_RGB

type image_store_type =
    Image_Packed    |
    Image_Matrix

open Xtypes;;


exception DepthNotSupported;;


type t = {

    (*
    ** The real image of depth image_depth.
    *)

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
    ** The pixmap raw image of depth display_depth.
    *)

    mutable rawimage : string;

    (* informations about the image and the display *)

    iwidth : size;
    iheight : size;
    idepth : int;
       
    image_bits_per_pixel : int;
    image_bytes_per_line : int;

    display_bits_per_pixel : int;
    display_bytes_per_line : int;

    (*
    ** Color index table (only needed for display depth 8,16)
    *)

    color_map: int array;
    color_type: color_type;


    (* was the image modified ? *)
    mutable imodified : bool 
  }


(*
** Bits per pixel desired by the server
*)
exception DepthNotFound;;
(* bits_for_depth *)

let bits_for_depth dpy depth =
  let rec iter depths =
    match depths with
      [] -> raise DepthNotFound
    |   { pxf_depth = d;
        pxf_bits_per_pixel = bits
      }::list ->
        if depth = d then
          bits
        else
          iter list
  in
  iter dpy.dpy_pixmap_formats

(*
** Bytes per line: we must take care about padding!
*)

let bytes_per_line bits pad width =
  (((( bits * width ) - 1 )
      / pad) + 1) *
  ( pad lsr 3)

(*
** Transform the image [image] to the X pixmap format [rawimage].
*)

let to_ximage image =
      if image.imodified then
      begin
        let cmap = image.color_map in

        match image.imagebuf with
        | Image_Gray_Packed i ->
        (
            match image.display_bits_per_pixel with
            | 8 ->  (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let ipos = y*image.image_bytes_per_line+x in
                                let rpos = y*image.display_bytes_per_line+x in
                                let col  = int_of_char i.[ipos] in
                                image.rawimage.[rpos] <-
                                            (char_of_int cmap.(col));
                            done
                        done;
            
                    );
            | 16 -> (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let ipos = y*image.image_bytes_per_line
                                           +x in
                                let rpos = y*image.display_bytes_per_line
                                           +x*2 in
                                let col  = int_of_char i.[ipos] in
                                image.rawimage.[rpos] <-
                                        (char_of_int (cmap.(col) land 0xff ));
                                image.rawimage.[rpos+1] <-
                                (char_of_int ((cmap.(col) lsr 8) land 0xff ));
                            done
                        done;

                    );
            | 24 -> (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let ipos = y*image.image_bytes_per_line
                                           +x in
                                let rpos = y*image.display_bytes_per_line
                                           +x*3 in
                                let col = i.[ipos] in
                                image.rawimage.[rpos]      <- col;
                                image.rawimage.[rpos+1]    <- col;
                                image.rawimage.[rpos+2]    <- col;
                            done
                        done;

                    );
            | 32 -> (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let ipos = y*image.image_bytes_per_line
                                           +x in
                                let rpos = y*image.display_bytes_per_line
                                           +x*4 in
                                let col = i.[ipos] in
                                image.rawimage.[rpos]      <- col;
                                image.rawimage.[rpos+1]    <- col;
                                image.rawimage.[rpos+2]    <- col;
                                image.rawimage.[rpos+3]    <- '\000';
                            done
                        done;

                    );
            | _ -> raise DepthNotSupported;
        );
        | Image_Gray8_Buf b ->
        (
            match image.display_bits_per_pixel with
            | 8 ->  (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let ipos = y*image.image_bytes_per_line+x in
                                let rpos = y*image.display_bytes_per_line+x in
                                let col  = buf_get b ipos in
                                image.rawimage.[rpos] <-
                                            (char_of_int cmap.(col));
                            done
                        done;
            
                    );
            | 16 -> (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let ipos = y*image.image_bytes_per_line
                                           +x in
                                let rpos = y*image.display_bytes_per_line
                                           +x*2 in
                                let col  = buf_get b ipos in
                                image.rawimage.[rpos] <-
                                        (char_of_int (cmap.(col) land 0xff ));
                                image.rawimage.[rpos+1] <-
                                (char_of_int ((cmap.(col) lsr 8) land 0xff ));
                            done
                        done;

                    );
            | 24 -> (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let ipos = y*image.image_bytes_per_line
                                           +x in
                                let rpos = y*image.display_bytes_per_line
                                           +x*3 in
                                let col = buf_getc b ipos in
                                image.rawimage.[rpos]      <- col;
                                image.rawimage.[rpos+1]    <- col;
                                image.rawimage.[rpos+2]    <- col;
                            done
                        done;

                    );
            | 32 -> (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let ipos = y*image.image_bytes_per_line
                                           +x in
                                let rpos = y*image.display_bytes_per_line
                                           +x*4 in
                                let col = buf_getc b ipos in
                                image.rawimage.[rpos]      <- col;
                                image.rawimage.[rpos+1]    <- col;
                                image.rawimage.[rpos+2]    <- col;
                                image.rawimage.[rpos+3]    <- '\000';
                            done
                        done;

                    );
            | _ -> raise DepthNotSupported;
        );

        | Image_Gray16_Buf b ->
        (
            match image.display_bits_per_pixel with
            | 8 ->  (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let ipos = y*image.image_bytes_per_line+x in
                                let rpos = y*image.display_bytes_per_line+x in
                                let col  = buf_get b (2*ipos) in
                                image.rawimage.[rpos] <-
                                            (char_of_int cmap.(col));
                            done
                        done;
            
                    );
            | 16 -> (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let ipos = y*image.image_bytes_per_line
                                           +x in
                                let rpos = y*image.display_bytes_per_line
                                           +x*2 in
                                let col  = buf_get b (2*ipos) in
                                image.rawimage.[rpos] <-
                                        (char_of_int (cmap.(col) land 0xff ));
                                image.rawimage.[rpos+1] <-
                                (char_of_int ((cmap.(col) lsr 8) land 0xff ));
                            done
                        done;

                    );
            | 24 -> (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let ipos = y*image.image_bytes_per_line
                                           +x in
                                let rpos = y*image.display_bytes_per_line
                                           +x*3 in
                                let col = buf_getc b (2*ipos) in
                                image.rawimage.[rpos]      <- col;
                                image.rawimage.[rpos+1]    <- col;
                                image.rawimage.[rpos+2]    <- col;
                            done
                        done;

                    );
            | 32 -> (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let ipos = y*image.image_bytes_per_line
                                           +x in
                                let rpos = y*image.display_bytes_per_line
                                           +x*4 in
                                let col = buf_getc b (2*ipos) in
                                image.rawimage.[rpos]      <- col;
                                image.rawimage.[rpos+1]    <- col;
                                image.rawimage.[rpos+2]    <- col;
                                image.rawimage.[rpos+3]    <- '\000';
                            done
                        done;

                    );
            | _ -> raise DepthNotSupported;
        );

        | Image_RGB24_Buf b ->
        (
            match image.display_bits_per_pixel with
            | 8 ->  (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let ipos = y*image.image_bytes_per_line+x in
                                let rpos = y*image.display_bytes_per_line+x in
                                let col  = buf_get b (3*ipos) in    (* Red *)
                                image.rawimage.[rpos] <-
                                            (char_of_int cmap.(col));
                            done
                        done;
            
                    );
            | 16 -> (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let ipos = y*image.image_bytes_per_line
                                           +x in
                                let rpos = y*image.display_bytes_per_line
                                           +x*2 in
                                let col  = buf_get b (3*ipos) in    (* Red *)
                                image.rawimage.[rpos] <-
                                        (char_of_int (cmap.(col) land 0xff ));
                                image.rawimage.[rpos+1] <-
                                (char_of_int ((cmap.(col) lsr 8) land 0xff ));
                            done
                        done;

                    );
            | 24 -> (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let ipos = y*image.image_bytes_per_line
                                           +x in
                                let rpos = y*image.display_bytes_per_line
                                           +x*3 in
                                (* RGB order ? *)
                                let r = buf_getc b (3*ipos) in
                                let g = buf_getc b (3*ipos+1) in
                                let b = buf_getc b (3*ipos+2) in
                                image.rawimage.[rpos]      <- r;
                                image.rawimage.[rpos+1]    <- g;
                                image.rawimage.[rpos+2]    <- b;
                            done
                        done;

                    );
            | 32 -> (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let ipos = y*image.image_bytes_per_line
                                           +x in
                                let rpos = y*image.display_bytes_per_line
                                           +x*4 in
                                (* RGB order ? *)
                                let r = buf_getc b (3*ipos) in
                                let g = buf_getc b (3*ipos+1) in
                                let b = buf_getc b (3*ipos+2) in
                                image.rawimage.[rpos]      <- r;
                                image.rawimage.[rpos+1]    <- g;
                                image.rawimage.[rpos+2]    <- b;
                                image.rawimage.[rpos+3]    <- '\000';
                            done
                        done;

                    );
            | _ -> raise DepthNotSupported;
        );

        | Image_Gray_Matrix i ->
        (
            match image.display_bits_per_pixel with
            | 8 ->  (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let rpos = y*image.display_bytes_per_line+x in
                                let col  = i.(y).(x) in
                                image.rawimage.[rpos] <-
                                            (char_of_int cmap.(col));
                            done
                        done;
            
                    );
            | 16 -> (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let rpos = y*image.display_bytes_per_line
                                           +x*2 in
                                let col  = i.(y).(x) in
                                image.rawimage.[rpos] <-
                                        (char_of_int (cmap.(col) land 0xff ));
                                image.rawimage.[rpos+1] <-
                                (char_of_int ((cmap.(col) lsr 8) land 0xff ));
                            done
                        done;

                    );
            | 24 -> (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let rpos = y*image.display_bytes_per_line
                                           +x*3 in
                                let col = char_of_int (i.(y).(x)) in
                                image.rawimage.[rpos]      <- col;
                                image.rawimage.[rpos+1]    <- col;
                                image.rawimage.[rpos+2]    <- col;
                            done
                        done;

                    );
            | 32 -> (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let rpos = y*image.display_bytes_per_line
                                           +x*4 in
                                let col = char_of_int (i.(y).(x)) in
                                image.rawimage.[rpos]      <- col;
                                image.rawimage.[rpos+1]    <- col;
                                image.rawimage.[rpos+2]    <- col;
                                image.rawimage.[rpos+3]    <- '\000';
                            done
                        done;

                    );
            | _ -> raise DepthNotSupported;
        );
        | Image_RGB_Packed i ->
        (
            match image.display_bits_per_pixel with
            | 8 ->  (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let ipos = y*image.image_bytes_per_line+x*3 in
                                let rpos = y*image.display_bytes_per_line+x in
                                let rcol  =  int_of_char i.[ipos]  in
                                let gcol  =  int_of_char i.[ipos+1] in
                                let bcol  =  int_of_char i.[ipos+2] in
                                let colind = (rcol lsl 4) lor
                                             (gcol lsl 2) lor
                                              bcol
                                in
                                image.rawimage.[rpos] <-
                                            (char_of_int cmap.(colind));
                            done
                        done;
            
                    );
            | 16 -> (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let ipos = y*image.image_bytes_per_line
                                           +x*3 in
                                let rpos = y*image.display_bytes_per_line
                                           +x*2 in

                                let rcol  =  int_of_char i.[ipos]  in
                                let gcol  =  int_of_char i.[ipos+1] in
                                let bcol  =  int_of_char i.[ipos+2] in
                                let colind = (rcol lsl 10) lor
                                             (gcol lsl 5) lor
                                              bcol
                                in
                                let col = cmap.(colind) in
                                image.rawimage.[rpos] <-
                                        (char_of_int (col land 0xff ));
                                image.rawimage.[rpos+1] <-
                                (char_of_int ((col lsr 8) land 0xff ));
                            done
                        done;

                    );
            | 24 -> (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let ipos = y*image.image_bytes_per_line
                                           +x*3 in
                                let rpos = y*image.display_bytes_per_line
                                           +x*3 in

                                image.rawimage.[rpos]      <- i.[ipos];
                                image.rawimage.[rpos+1]    <- i.[ipos+1];
                                image.rawimage.[rpos+2]    <- i.[ipos+2];
                            done
                        done;

                    );
            | 32 -> (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let ipos = y*image.image_bytes_per_line
                                           +x*3 in
                                let rpos = y*image.display_bytes_per_line
                                           +x*4 in

                                image.rawimage.[rpos]      <- i.[ipos];
                                image.rawimage.[rpos+1]    <- i.[ipos+1];
                                image.rawimage.[rpos+2]    <- i.[ipos+2];
                                image.rawimage.[rpos+3]    <- '\000';
                            done
                        done;

                    );
            | _ -> raise DepthNotSupported;
        );

        | Image_RGB_Matrix i ->
        (
            match image.display_bits_per_pixel with
            | 8 ->  (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let rpos = y*image.display_bytes_per_line+x in
                                let (rcol,gcol,bcol)  = i.(y).(x) in
                                let colind = (rcol lsl 10) lor
                                             (gcol lsl 5) lor
                                              bcol
                                in
                                let col = cmap.(colind) in
                                
                                image.rawimage.[rpos] <-
                                            (char_of_int cmap.(col));
                            done
                        done;
            
                    );
            | 16 -> (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let rpos = y*image.display_bytes_per_line
                                           +x*2 in
                                let (rcol,gcol,bcol)  = i.(y).(x) in
                                let colind = (rcol lsl 10) lor
                                             (gcol lsl 5) lor
                                              bcol
                                in
                                let col = cmap.(colind) in

                                image.rawimage.[rpos] <-
                                        (char_of_int (col land 0xff ));
                                image.rawimage.[rpos+1] <-
                                (char_of_int ((col lsr 8) land 0xff ));
                            done
                        done;

                    );
            | 24 -> (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let rpos = y*image.display_bytes_per_line
                                           +x*3 in
                                let (rcol,gcol,bcol)  = i.(y).(x) in
                                image.rawimage.[rpos]      <- char_of_int rcol;
                                image.rawimage.[rpos+1]    <- char_of_int gcol;
                                image.rawimage.[rpos+2]    <- char_of_int bcol;
                            done
                        done;

                    );
            | 32 -> (
                        for y = 0 to image.iheight -1 
                        do
                            for x = 0 to image.iwidth -1 
                            do
                                let rpos = y*image.display_bytes_per_line
                                           +x*4 in
                                let (rcol,gcol,bcol)  = i.(y).(x) in
                                image.rawimage.[rpos]      <- char_of_int rcol;
                                image.rawimage.[rpos+1]    <- char_of_int gcol;
                                image.rawimage.[rpos+2]    <- char_of_int bcol;
                                image.rawimage.[rpos+3]    <- '\000';
                            done
                        done;

                    );
            | _ -> raise DepthNotSupported;
        );

      end;                    
      image.imodified <- false



let gray_cmap = ref [||]
let rgb_cmap = ref [||]


(*
** Put the image to the specified drawable win.
*)
  
let putImage 
    dpy
    win
    gc
    x
    y
    image
    =


    (* Update the raw image buffer if needed *)

    to_ximage image;

    let gg = X.getGeometry dpy win in
    let zimage = image.rawimage in
    let len = String.length zimage in
    let perline = len / image.iheight in
    assert (perline * image.iheight = len);
    let maxlines = (dpy.dpy_max_request_length - 50) / perline in
    let depth = image.idepth in

    let rec iter y vy dy =
        let ny = min maxlines dy in
        X.putImage dpy gc win x y image.iwidth ny
            0 depth ZPixmap 
            (String.sub zimage (vy * perline) (ny * perline));
        if ny < dy then
            iter (y+ny) (vy+ny) (dy-ny)
    in

    if (image.iheight <= maxlines) then
        X.putImage dpy gc win x y image.iwidth image.iheight
                   0 depth ZPixmap zimage  
    else 
        iter y 0 image.iheight


(*
** Return a new image structure. Allocate color map if needed.
** This function doesen't allocate the image storage.
*)
    
let newImage 
    dpy 
    win
    cmap
    width 
    height 
    depth
    color_format
    store_format 
    image
  =

  let img_bits_per_pixel = match color_format with
                    | Color_Gray -> 8;
                    | Color_RGB  -> 24;
  in

  let dpy_bits_per_pixel =  bits_for_depth dpy depth in 
  let dpy_bytes_per_line =  bytes_per_line dpy_bits_per_pixel 
                                           dpy.dpy_bitmap_format_scanline_pad 
                                           width in


  let img_bytes_per_line =  (img_bits_per_pixel / 8) * width in

  (*
  ** Build a colormap if needed
  *)  

  let img_cmap =
      if (dpy_bits_per_pixel < 24 ) then
      begin
        match color_format with
        | Color_Gray ->
          if(!gray_cmap = [||]) then
          (
            let gcmap = Array.create 256 (id_to_pixel (-1)) in 
            for i = 0 to 255 
            do
                gcmap.(i) <-
                            let ac = X.allocColor dpy cmap
                                    (i*256) (i*256) (i*256)
                            in
                            ac.ac_pixel;
            done;
            gray_cmap := gcmap;
            gcmap
          )
          else
            !gray_cmap;
        | Color_RGB ->
          if(!rgb_cmap = [||]) then
          (
            let bits_per_col =
                match dpy_bits_per_pixel with
                | 8  -> 2;
                | 16 -> 5;
                | _ -> raise DepthNotSupported;
            in
            let cmap_size = 2 lsl (bits_per_col*3) in

            let rgbcmap = Array.create cmap_size (id_to_pixel (-1)) in 

            for r = 0 to bits_per_col-1 do
            for g = 0 to bits_per_col-1 do
            for b = 0 to bits_per_col-1 do
                let i = (r lsl (bits_per_col*2)) +
                        (g lsl bits_per_col) +
                        b
                in
                rgbcmap.(i) <-
                            let ac = X.allocColor dpy cmap
                                    (r*256) (g*256) (b*256)
                            in
                            ac.ac_pixel;
            done; done; done;
            rgb_cmap := rgbcmap;
            rgbcmap
          )
          else
            !rgb_cmap;
      end
      else
        [||]
  in

  let img_temp =
  {
    imagebuf = image;

    image_store = store_format;

    xpixmap = X.createPixmap dpy win 
                             width height depth; (* dpy_bits_per_pixel; *)

    rawimage = String.create (dpy_bytes_per_line * height);

    iwidth = width;
    iheight = height;
    idepth = depth;

    image_bits_per_pixel = img_bits_per_pixel;
    image_bytes_per_line = img_bytes_per_line;

    display_bits_per_pixel = dpy_bits_per_pixel;
    display_bytes_per_line = dpy_bytes_per_line;

    color_map = img_cmap;
    color_type = color_format;

    imodified = true
  }
  in
  (* push the image into the xpixmap *)

  let gc = X.createGC dpy img_temp.xpixmap  
    [GCforeground (id_to_pixel 1); GCbackground (id_to_pixel 0)] 
  in  
  putImage dpy img_temp.xpixmap gc 0 0 img_temp;
  X.freeGC dpy gc;

  img_temp
  



(*
** Create a new Image. Setup the color index map array if
** needed.
**
** Arguments: 
**          dpy : X display
**          cmap: X color map to be used
**          width/height: -:-
**          color_format: Color_Gray ...
**          store_format: the real image storage format (Image_Packed...)
** 
*)

let createImage 
    dpy
    win 
    cmap
    width 
    height 
    depth 
    color_format
    store_format
  =

  let img_bits_per_pixel = match color_format with
                    | Color_Gray -> 8;
                    | Color_RGB  -> 24;
  in
 
  let img_bytes_per_line =  (img_bits_per_pixel / 8) * width in

  let image = 
        (match color_format with 
        | Color_Gray ->
          (
            match store_format with
            | Image_Packed ->
                (
                    Image_Gray_Packed 
                        (String.create (img_bytes_per_line * height))
                ); 
            | Image_Matrix ->
                (
                    Image_Gray_Matrix
                        (Array.make_matrix width height 0) 
                );
          );
        | Color_RGB ->
          (
            match store_format with
            | Image_Packed ->
                (
                    Image_RGB_Packed 
                        (String.create (img_bytes_per_line * height))
                ); 
            | Image_Matrix ->
                (
                    Image_RGB_Matrix
                        (Array.make_matrix width height (0,0,0)) 
                );

          );
        )
    in
    newImage dpy win cmap width height depth color_format store_format image


let createImageFromGrayPacked
    dpy 
    win
    cmap
    width 
    height 
    depth 
    image
  =
    newImage dpy win cmap width height depth 
             Color_Gray Image_Packed (Image_Gray_Packed image)

  
let createImageFromGrayMatrix
    dpy
    win 
    cmap
    width 
    height 
    depth 
    image
  =
    newImage dpy win cmap width height depth 
             Color_Gray Image_Matrix (Image_Gray_Matrix image)
  
let createImageFromRGBPacked
    dpy 
    win
    cmap
    width 
    height 
    depth 
    image
  =
    newImage dpy win cmap width height depth 
             Color_RGB Image_Packed (Image_RGB_Packed image)

  
let createImageFromRGBMatrix
    dpy 
    win
    cmap
    width 
    height 
    depth 
    image
  =
    newImage dpy cmap win width height depth 
             Color_RGB Image_Matrix (Image_RGB_Matrix image)
  

let createImageFromGrayBuf
    dpy 
    win
    cmap
    width 
    height 
    depth 
    image
  =
    newImage dpy win cmap width height depth 
             Color_Gray Image_Packed (Image_Gray8_Buf image)


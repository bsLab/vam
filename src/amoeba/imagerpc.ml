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
**    $INITIAL:     (C) 2004
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.01
**
**    $INFO:
**
** Generic Image Transfer Interface for Digital Image Processing. 
** Uses standard Amoeba RPC Interface for transferring Images
** of arbitary formats and sizes.
**
** Supports several image formats  (RGB24, GRAY8,...)
** All Servers and Clients should use this header file.
**
** Warning: currently no byte swapping is performed on transfers
** between different ending machines!!!
**
** RPC: IMAGEGET:
**      Request:
**              h_offset=expected image format
**              h_size=x-size in pixel units
**              h_extra=y-size in pixel units
** 
**      Reply:
**              h_status=STD_OK
**                      =IMNOFRAME      No frame available
**                      =IMTYPEBAD      File format not supported
**                      =IMSIZEBAD      Illegal size/can't convert to
**                                      requested size
**              h_size=x-size in pixel units
**              h_extra=y-size in pixel units
**              h_offset=image type
**              buf=frame[i]
** RPC: IMAGEPUT:
**
**      Request:
**              h_offset=expected image format
**              h_size=x-size in pixel units
**              h_extra=y-size in pixel units
**              buf=frame; 
**      Reply:
**              h_status=STD_OK
**                      =IMNOFRAME      No frame available/can't receive frame
**                      =IMTYPEBAD      File format not supported
**                      =IMSIZEBAD      Illegal size
**              Optional if an image is returned:
**              h_size=x-size in pixel units
**              h_extra=y-size in pixel units
**              h_offset=image type
**              buf=frame[i]
**
** RPC: IMAGEINFO:
**
**      Request:
**              h_command=IMAGEINFO
**              other values don't care
**      Reply:
**              h_size=x-size in pixel units
**              h_extra=y-size in pixel units
**              h_offset=type:
**
** RPC: IMAGEFRAMESEL:
**  Request:
**      h_offset=frame number   [set: !!! 1 !!! .. MAX avail frames]
**          =0      [only get the actual frame number]
**  Reply:
**      h_offset=actual frame number [after set] 
**      h_size=total count of available frames
**
**
**    $ENDOFINFO
**
*)

open Amoeba
open Rpc
open Stderr
open Stdcom
open Cmdreg
open Buffer
open Bytebuf

(*
** Must be changed and inserted in Cmdreg
*)
let unregistered_FIRST_COM = 15000
let unregistered_FIRST_ERR = -unregistered_FIRST_COM

let image_RPCFIRSTCOM = unregistered_FIRST_COM+500
let image_RPCFIRSTERR = (-image_RPCFIRSTCOM)

(*
** Get Info about the actual supported image format
*)
let image_INFO       =  Command (image_RPCFIRSTCOM+1)




(*
** Supported Image types: 
*)

(*
** Gray scaled images with ## bits per pixel [intensity] 
** Linear grayscale 
*)
let         gray8   =          10
let         gray16  =          11
let         y8      =          gray8

(*
** Color RGB Images with ##/3  bits per pixel per color [Red,Green,Blue] 
** First ##/3 pixels give the Red, the next Green,and the last Blue value.
*)

let         rgb24   =          20      (* True-Color *)

(* 565 16 bit RGB *)
let         rgb16   =          30
(* 555 15bit RGB *)
let         rgb15   =          31



(*
** Get and put an image of/to server with specified type
*)


let image_GET        = Command (image_RPCFIRSTCOM+2)
let image_PUT        = Command (image_RPCFIRSTCOM+3) 

(*
** Request return status 
*)

let         image_NOFRAME       = (image_RPCFIRSTERR-10)
let         image_TYPEBAD       = (image_RPCFIRSTERR-11)
let         image_SIZEBAD       = (image_RPCFIRSTERR-12)

(*
** Select a special frame if several are available
** Frame Number range starts with number 1.  
*)
 
let     image_FRAMESEL        = Command (image_RPCFIRSTCOM+4)


(*
** Get one frame with specified frame number and image type
** from image server (for example a frame grabber device).
** The image is stored in the bytebuffer starting at 
** specified position.
*)

let image_get_frame ~server
                    ~width
                    ~height
                    ~frame
                    ~format
                    ~buf
                    ~pos
    =
  try
  begin

    
    (*
    ** Check the size of the buffer...
    *)
    let blen = (buf_len buf)-pos in
    let size = width * height in

    
    let isize =
        match format with
        | f when (f=gray8) -> 
                if blen < size     then raise (Error std_OVERFLOW);
                size;
        | f when (f=gray16) ->
                if blen < (2*size) then raise (Error std_OVERFLOW);
                2*size;
        | f when (f=rgb24) ->
                if blen < (3*size) then raise (Error std_OVERFLOW);
                3*size;
        | f when (f=rgb16) ->
                if blen < (2*size) then raise (Error std_OVERFLOW);
                2*size;
        | f when (f=rgb15) ->
                if blen < (2*size) then raise (Error std_OVERFLOW);
                2*size;
        | _ -> raise (Error std_NOTNOW);
        in

    (*
    ** Now set the desired frame number
    *)

    if frame < 0 then raise (Error std_ARGBAD);
    
    let hdr = header_new () in
    hdr.h_priv <- server.cap_priv;
    hdr.h_port <- server.cap_port;
    hdr.h_offset <- frame;    
    hdr.h_command <- image_FRAMESEL;

    let stat,_,hdr' = trans(hdr,nilbuf,0,nilbuf,0) in
    if stat <> std_OK then raise (Error stat)
    else if hdr'.h_status <> std_OK then raise (Error hdr'.h_status);
    
    (*
    ** Now transfer the image data.
    ** Note: the image type requested and the server supported image
    ** type must be the same or an error is returned!
    ** The server side image type must be set with the std_params
    ** interface.
    *)
                               
    hdr.h_priv <- server.cap_priv;
    hdr.h_port <- server.cap_port;
    hdr.h_offset <- format;    
    hdr.h_size <- width;
    hdr.h_extra <- height;
    hdr.h_command <- image_GET;

    let stat,n,hdr' = transo(hdr,nilbuf,0,0,buf,pos,isize) in
    if stat <> std_OK then raise (Error stat)
    else if hdr'.h_status <> std_OK then raise (Error hdr'.h_status);

    
    std_OK
  end
    with | Error stat -> stat



(*
** Get info about currently image size and selected image format.
**
** Returns:
**  width,
**  height,
**  imagetypestring
*)

let image_info ~server =
    let image_type_tostr f =
        match f with
        | f when (f=gray8) -> 
                "GRAY8"
        | f when (f=gray16) ->
                "GRAY16"
        | f when (f=rgb24) ->
                "RGB24"
        | f when (f=rgb16) ->
                "RGB16"
        | f when (f=rgb15) ->
                "RGB15"
        | _ -> "UNKNOWN"
        in

    let hdr = header_new () in
    hdr.h_priv <- server.cap_priv;
    hdr.h_port <- server.cap_port;
    hdr.h_command <- image_INFO;

    let stat,_,hdr' = trans(hdr,nilbuf,0,nilbuf,0) in
    if stat <> std_OK then stat,0,0,""
    else if hdr'.h_status <> std_OK then hdr'.h_status,0,0,""
    else
        (std_OK,
         hdr'.h_size,
         hdr'.h_extra,
         (image_type_tostr hdr'.h_offset))

    
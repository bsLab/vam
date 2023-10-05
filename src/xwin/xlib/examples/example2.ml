
(*
** Open the needed Xwin modules
*)

open X
open Xtypes
open Xlib


(* Open the main display *)

let dpy = openDisplay "" ;;


(* Get some defaults for this display *)

let white= defaultWhite dpy;;
let black= defaultBlack dpy;;
let font = openFont dpy "fixed" ;;
let depth = defaultDepth dpy ;;
let defcolormap = defaultColormap dpy;;

let { anc_pixel = red } = allocNamedColor dpy defcolormap "red";;
let { anc_pixel = green } = allocNamedColor dpy defcolormap "green";;

(* the root window *)

let rootwin = defaultRoot dpy ;;

(* the default event handler mask we need *)

let default_event_mask = 
    [
    ExposureMask; 
    KeyPressMask;
    KeyReleaseMask;
    StructureNotifyMask;
    ButtonPressMask; 
    ButtonReleaseMask; 
    OwnerGrabButtonMask 
    ]
;;

(* the size of our drawable window *)

let width = 400 ;;
let height = 350 ;;

(* create a generic graphical context *)

let gc = X.createGC dpy rootwin [
        GCforeground black;
	GCfont font;
      ]
;;

(* the draw window ... *)

let imwin = createSimpleWindow dpy rootwin 
		0 0 width height 1 
		[ 
		  CWBackPixel white ; 
		  CWEventMask default_event_mask;
		]
;;


(* ... mapped into our display *)

mapWindow dpy imwin ;;

(* Clear the store field *)

changeGC dpy gc [GCforeground white];;
fillRectangle dpy imwin gc 0 0 width height;;
changeGC dpy gc [GCforeground black];;

(*
** Draw a simple rectangle in this window.
*)

changeGC dpy gc [GCforeground red];;
drawRectangle dpy imwin gc 9 29 257 257 ;;
changeGC dpy gc [GCforeground black];;


(*
** Create gray level color map
*)

let graymap = Array.init 256
	(fun i -> allocColor dpy defcolormap (i*255) (i*255) (i*255)) ;;


(* Image size *)
let gwidth = 256 ;;
let gheight = 256 ;;

let dp2   = depth / 8;;

(* image buffer - use a string buffer *)
let libuf = String.create (gwidth*gheight*dp2);;

(* create a gray level gradient *)

for j = 0 to 255
do
	for i = 0 to 255
	do
		(* get the colormap index from the colormap *)
		let col = graymap.(i).ac_pixel  in

		(* the position within the buffer *)
		let pos = j*gwidth*dp2 + i*dp2   in

		match depth with
		| 8 -> () ;
		| 16 -> libuf.[pos]   <- (char_of_int (col land 0xff));
			libuf.[pos+1] <- (char_of_int (col lsr 8    ));
		| _ -> ();
	done;
done;;

(* put the image ... *)
 
putImage dpy gc imwin 10 30 256 256 0 depth ZPixmap libuf;

(*
** and a simple event loop
*)

try
while true do
    let ev = nextEvent dpy in

    match ev.ev_event with
	
      	| KeyPressEvent k when ev.ev_window == imwin -> 
		raise Exit;
		(* somebody pressed a key during the pointer was within the win *)

	| ExposeEvent _ ->
	  	(* we must restore the window *)

		changeGC dpy gc [GCforeground red];
		drawRectangle dpy imwin gc 9 29 257 257 ;
		changeGC dpy gc [GCforeground black];
		putImage dpy gc imwin 10 30 256 256 0 depth ZPixmap libuf;
	

    	| _ -> ()
done
with Exit -> () ;
| _ -> failwith "Loop failure" ;;



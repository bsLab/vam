
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
let defcolor= defaultColormap dpy;;

let { anc_pixel = red } = allocNamedColor dpy defcolor "red";;
let { anc_pixel = green } = allocNamedColor dpy defcolor "green";;

(* the root window *)

let rootwin = defaultRoot dpy ;;

(* the default event hanfler mask we need *)

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

let width = 100 ;;
let height = 100 ;;

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

(*
** because the client is responsible to restore damaged
** image regions, we need a hidden copy of our draw window
*)

let store = createPixmap dpy rootwin width height depth ;;

(* Clear the store field *)

changeGC dpy gc [GCforeground white];;
fillRectangle dpy store gc 0 0 width height;;
changeGC dpy gc [GCforeground black];;

(*
** Draw a simple rectangle in this window; both, the X window and 
** our backing store window.
*)
drawRectangle dpy imwin gc 20 30 40 40 ;;
drawRectangle dpy store gc 20 30 40 40 ;;


(*
** and a filled one 
*)

changeGC dpy gc [GCforeground red];;
fillRectangle dpy imwin gc 22 32 38 38 ;;
fillRectangle dpy store gc 22 32 38 38 ;;
changeGC dpy gc [GCforeground black];;

(*
** and a simple event loop
*)

while true do
    let ev = nextEvent dpy in

    match ev.ev_event with
	
      	| KeyPressEvent k when ev.ev_window == imwin -> raise Exit;
	  (* somebody pressed a key during the pointer was within the win *)

	| ExposeEvent _ ->
		copyArea dpy gc store 0 0 imwin 0 0 width height;
	  (* we must restore the window from our hidden field *)
    	| _ -> ()
done
;;


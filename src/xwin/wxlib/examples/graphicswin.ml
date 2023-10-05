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
**    $AUTHORS:     
**                  Stefan Bosse
**    $INITIAL:     (C) 2004 BSSLAB
**    $CREATED:     
**    $VERSION:     1.02
**
**    $INFO:
**
**  Simple example for a graphics widget (in his own window) with
**  a mouse pointer following crosshair and viewport management.
**
**    $ENDOFINFO
**
*)


open WX_types
open XGraphics
open Xtypes
open Thread
open Printf

let root = new WX_root.from_display "" 0

let win1 = new WX_wmtop.t root [MinWidth 100]

let vbar_main = new WX_bar.v win1#container 
                    [IpadX 10;
                     IpadY 10]
let but_quit = new WX_button.with_label 
                    win1#container "Quit" [
                                           MinWidth 80; ExpandX true] 



let win2 = new WX_wmtop.t root []
let hbar = new WX_bar.h win2#container []
let vbar = new WX_bar.v win2#container []

let hbar_info  = new WX_bar.h win2#container [IpadX 5; IpadY 5]
let x_text = new WX_label.t hbar_info#container "x:    0" 
                [MinWidth 50]
let y_text = new WX_label.t hbar_info#container "y:    0" 
                [MinWidth 50]

(*
** Virtual graphic window size
*)
let vir_width = 1000
let vir_height = 1000

let gra  = new WX_Graphics.t hbar#container [] vir_width vir_height

let adx = new WX_adjust.t ()
let ady = new WX_adjust.t ()
(*
** Physical window size (viewport)
*)
let phy_width = 800
let phy_height = 600

let vport = new WX_viewport.t hbar#container adx ady 
            [MinWidth phy_width; MinHeight phy_height; 
             MaxWidth phy_width; MaxHeight phy_height; 
             Background "black"]
let scrollx = new WX_scrollbar.h hbar#container adx []
let scrolly = new WX_scrollbar.v hbar#container ady []


(*
** Draw the cross hair
*)

let old_x = ref 0
let old_y = ref 0


let drawrect () =
    let x,y,dx,dy = 100,(vir_height - 100 - 200),400,200 in
    set_color red;
    draw_rect x y dx dy;
    update ()

let drawcircle1 () =
    let x,y,r = 100,(vir_height - 100 - 200),50 in
    set_color blue;
    draw_circle x y r;
    update ()

let drawcircle2 () =
    let x,y,r = 150,400,200 in
    set_color blue;
    draw_circle x y r;
    update ()

let objs = ref [drawrect;drawcircle1;drawcircle2]
let drawobjs () = List.iter (fun o -> o ()) !objs 

let drawcross x y =
    set_color black;
    (*
    ** Draw new cross hair lines. Draw with Xor pixel function
    ** to enable restoring of current foreground content.
    *)
    let draw () =
        set_gc [GCfonction GXxor;GCforeground (foreground lxor
                                               background)];
        moveto 0 y;
        lineto vir_width y;
        moveto x 0;
        lineto x vir_height;
        old_x := x;
        old_y := y;
        set_gc [GCfonction GXcopy];
        in

    (*
    ** Clear old one and restore previous foreground (for example
    ** a line).
    *)
    let clear () =
        set_gc [GCfonction GXxor;GCforeground (foreground lxor
                                               background)];
        moveto 0 !old_y;
        lineto vir_width !old_y;
        moveto !old_x 0;
        lineto !old_x vir_height;
        set_gc [GCfonction GXcopy];
        in


    clear ();
    draw ();
    
    update ()
    

    
let _ =
    vbar_main#container_add_s [but_quit#contained];
    win1#container_add vbar_main#contained ;
    vport#container_add gra#contained;
    hbar#container_add_s [vport#contained;scrolly#contained];
    hbar_info#container_add_s [x_text#contained;
                               y_text#contained];
    vbar#container_add_s [hbar_info#contained;
                          hbar#contained;
                          scrollx#contained];
    
    win2#container_add vbar#contained;

    win1#show;
    win2#show;
    set_update_style FlushAll;
    drawobjs (); 

    but_quit#set_action (fun () ->
                exit 0
            );
    (*
    ** User interaction: the crosshair follows the mouse pointer!
    ** mouse_?_event: external variable set by WXLIB event handler.
    *)

    gra#configure [
            Bindings
            [
                PointerMotion,(fun _ ->
                    let x,y= !mouse_x_event,
                             (vir_height - !mouse_y_event) in

                    x_text#set_string  (sprintf "x:%4d" x);
                    x_text#update;
                    y_text#set_string  (sprintf "y:%4d" y);
                    y_text#update;
                    drawcross x y;) 
            ]];

    loop ();



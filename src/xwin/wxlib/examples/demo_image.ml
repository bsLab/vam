(*
** Example desktop with buttons, editable lines and an
** image.
*)
 

open Xtypes
open WX_types

open X
open KeyBind
open Display

open Gc

(* some GC settings *)

let gcc = Gc.get () 
let _ = gcc.max_overhead <- 100;
Gc.set gcc


let str1 = ref "## " 
let str2 = ref "## " 

(* Image size *)
let iwidth = 20
let iheight = 20 

(* image buffer - use a string buffer - gray scale 8 Bit *)
let imagebuf = String.create (iwidth*iheight*2)   

(* fill the image buffer with new random values *)

let fillrand () =
        for y = 0 to iheight-1
        do
            for x = 0 to iwidth-1 
            do
                imagebuf.[y*iwidth+x] <- (char_of_int (Random.int 255)) ;
            done;
        done

let modrand () =
        for y = 0 to iheight-1
        do
            for x = 0 to iwidth-1 
            do
                imagebuf.[y*iwidth+x] <- char_of_int 
                    ( 
                        ((int_of_char imagebuf.[y*iwidth+x]) + x) land 0xff
                    )
				 	;
            done;
        done

(*
** The user defined desktop class 
*)


class topwx root attr =
    (* the toplevel widget *)
    let top = new WX_wmtop.t root attr in

    (* the main vertical container *)
    let vbar = new WX_bar.v top#container 
                        [ ExpandX true; ExpandY true; IpadX 10;IpadY 10 ] in

    (* the first horizotnal container *)
    let hbar1 = new WX_bar.h vbar#container [] in

    (* add a label for the readline to the container *)
    let label_line1 = new WX_label.t hbar1#container "Input 1: " 
                        [IpadY 4] in

	(* and a read line *)
    let read_line1 = new WX_ledit.t hbar1#container !str1 
                        [ ExpandX true; IpadY 4] in

    (* the second horizontal container *)
    let hbar2 = new WX_bar.h vbar#container [] in

    (* with a lable, too *)
    let label_line2 = new WX_label.t hbar2#container "Input 2: " 
                        [IpadY 4] in

    (* and the second read line *)
    let read_line2 = new WX_ledit.t hbar2#container !str2 
                        [ ExpandX true; IpadY 4] in

    (* The third horizontal container *)
    let hbar = new WX_bar.h	vbar#container 
                        [ExpandX true; IpadX 4;] in

    (* with a EXIT button *) 
    let exit_but = new WX_button.with_label hbar#container "EXIT" 
                        [ IpadX 4; IpadY 2] in

    (* and the RANDOM button *)
    let rand_but = new WX_button.with_label hbar#container " RANDOM " 
                        [ IpadX 4; IpadY 2] in


    (* now the image *)
   

    let randimage = new WX_image.t vbar#container 
            ("Random",FromGrayPacked (imagebuf,iwidth,iheight)) 
            [Relief ReliefRidge]
    in


    (* On EXIT close the window *)

    let closewindow () =
        str1 := read_line1#string;
        str2 := read_line2#string;

        closeDisplay (top#display);
        raise Exit;
    in

    (* remember the last focus container the user has used *)
    let last_focus = ref read_line1 in
   
    let _ =
   
        (* set actions for the EXIT and RANDOM button *)

        exit_but#set_action ( fun () -> closewindow () );

        rand_but#set_action 
            ( fun () ->
                modrand ();
                randimage#modified;
                randimage#refresh;
            );

        top#setWM_NAME "Widget";

        (* configure the widget elements *)

        top#configure [ 
                        Bindings 
                        [
                          FocusIn,(fun _ ->
                                     if (X.getInputFocus top#display).gif_win 
                                         == top#window then
                                         !last_focus#focus);
                          ButtonPress, (fun _ -> !last_focus#focus);
                        ]
                      ];

        read_line1#configure [ 
                               Bindings 
                               [
                                 ButtonPress, (fun _ -> read_line1#focus);
                                 FocusIn, (fun _ -> last_focus := read_line1);
                               ]
                             ];

        read_line2#configure [ 
                               Bindings 
                               [
                                  ButtonPress, (fun _ -> read_line2#focus);
                                  FocusIn, (fun _ -> last_focus := read_line2);
                               ]
                             ];

        (* put all together to one desktop *)

        top#container_add vbar#contained;

        hbar#container_add_s [
                                exit_but#contained;
                                rand_but#contained;
                             ];

        hbar1#container_add_s [
                                label_line1#contained;
                                read_line1#contained;
                              ];

        hbar2#container_add_s [
                                label_line2#contained;
                                read_line2#contained;
                              ];

        vbar#container_add_s [
                                hbar#contained;
                                hbar1#contained;
                                hbar2#contained;
                                randimage#contained;
                             ];
        in
        object (self)
            inherit WX_deleg.wx_object (top :> WX_object.t)
            inherit WX_deleg.wmtop top         

end


let wx () =
    fillrand ();

    let root = new WX_root.from_display "" 0 in
    let mywx = new topwx root [] in
    
    mywx#show;

    try
        loop () 
    with
        Exit -> ()

let _ = wx () 




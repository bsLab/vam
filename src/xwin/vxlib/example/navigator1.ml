open Amoeba
open Stderr

open X
open VX_types
open VX_box
open VX_ps
open VX_navigator
open VX_common

let display = new VX_display.t ""
let root = new VX_root.t display 0 
let top = new VX_wmtop.t root      
          [
           MinHeight 300; MinWidth 500;Background "white"]

let closewindow () = 
        Display.closeDisplay (top#display)


let s111 = [Text "The goals of the project..."]
let s112 = [Text "This section deals with settings."]
let s113 = [Text (no_nl
"
Bash is an sh-compatible command language interpreter that
executes  commands  read from the standard input or from a
file.  Bash also incorporates  useful  features  from  the
Korn and C shells (ksh and csh).
")]

let input1 = [Input {
    in_desc = "Enter design name";
    in_attr = [Border []];
    in_action = (fun str -> 
        print_string str; print_newline ();
        if str = "" then std_ARGBAD else std_OK
        );
    in_str = "<Empty>";
}]

let s11 = [S2 ("Goals",s111);
           S2 ("Settings",s112);
           S2 ("Start",s113)]

let s12 = [S2 ("Start",s111 @ input1 @ input1);
           S2 ("Admin",s113 @ s112@s111@s113);
          ]

let s13 = [S2 ("Basics",s111);
           S2 ("Lists",s112);
           S2 ("Administration",s113)]

let desc = [S1 ("Intro",s11);
            S1 ("Main",s12);
            S1 ("Help",s13);
            S1 ("Exit",[Action
                           (fun () ->
                                closewindow ();
                                exit 0;
                            )])
            ]

let nav = new VX_navigator.t root 
                             top#container 
                desc
                [
                ExpandX true;
                ExpandY true; 
                ] 


let _ =
    top#setWM_NAME "VX test";
    top#container_add nav#contained;
    top#show;

(*
    let printed = ref false in

    top#configure [Bindings [
        EnterWindow,(fun _ -> 
            if not !printed then
            begin
                printed := true;
                print_eps "box1.eps" 0.5 vbox#contained;
                print_ps "box1.ps" A4 vbox#contained;
            end;
            );
        ]];
*)
    try
      loop ();
    with Exit -> ();

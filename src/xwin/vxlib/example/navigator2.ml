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


let s11 = text "The goals of the project..."
let s12 = text "This section deals with settings."
let s13 = text (no_nl
"
Bash is an sh-compatible command language interpreter that
executes  commands  read from the standard input or from a
file.  Bash also incorporates  useful  features  from  the
Korn and C shells (ksh and csh).
")

let s21 = text "The goals of the project..."
let s22 = text "This section deals with settings."
let s23 = text (no_nl
"
Bash is an sh-compatible command language interpreter that
executes  commands  read from the standard input or from a
file.  Bash also incorporates  useful  features  from  the
Korn and C shells (ksh and csh).
")

let s31 = text "The goals of the project..."
let s32 = text "This section deals with settings."
let s33 = text (no_nl
"
Bash is an sh-compatible command language interpreter that
executes  commands  read from the standard input or from a
file.  Bash also incorporates  useful  features  from  the
Korn and C shells (ksh and csh).
")


let input1 = Input {
    inp_desc = 0,"Enter design name";
    inp_attr = [Border []];
    inp_buts = ["",ActionSS (fun str -> 
        print_string str; print_newline ();
        if str = "" then std_ARGBAD else std_OK
        );];
    inp_str = "<Empty>";
    inp_vx = None;
}

let input2 = Input {
    inp_desc = 0,"Enter source name";
    inp_attr = [Border []];
    inp_buts = ["",ActionSS (fun str -> 
        print_string str; print_newline ();
        if str = "" then std_ARGBAD else std_OK
        )];
    inp_str = "<Empty>";
    inp_vx = None;
}

let s1 = [s11;input1]
let s2 = [s21;s32]
let s3 = [s31;input2;s32;s33]


let desc = [S1 ("Intro",s1);
            S1 ("Main",s2);
            S1 ("Help",s3);
            S1 ("Exit",[Action
                           (fun () ->
                                closewindow ();
                                exit 0;
                            )])
            ]

let nav = new VX_navigator.s1 root 
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

    let printed = ref false in

    top#configure [Bindings [
        EnterWindow,(fun _ -> 
            (match s32 with
            | Text t ->
                let w = get_some t.text_vx in
                w#configure [Background "yellow"];
                w#update;
            | _ -> ();
            );
(*
            if not !printed then
            begin
                printed := true;
                print_eps "box1.eps" 0.5 vbox#contained;
                print_ps "box1.ps" A4 vbox#contained;
            end;
*)
            );
        ]];
    try
      loop ();
    with Exit -> ();

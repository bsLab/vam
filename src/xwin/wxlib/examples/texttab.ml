
open WX_types 

let display = new WX_display.t ""
let root = new WX_root.t display 0

(* The top widget *)
let top = new WX_wmtop.t root [MinWidth 100; MinHeight 100; MaxHeight 600]

(* Vertical and horizontal container widgets *)
let vbar1 = new WX_bar.v top#container []

let hbar1 = new WX_bar.h vbar1#container [IpadX 5;IpadY 5]
let exitbut = new WX_button.with_label top#container "Exit" 
                [ExpandX true]
let chval = ref 0 
let chbut = new WX_button.with_label top#container "Change" 
                [ExpandX true]


let rows = 3 
let cols = 3

(* text table *)
let tab = new WX_table.text vbar1#container 
                cols rows 100 20 [||] [IpadX 10; IpadY 10] 


let vals = [|
    [|"Nummer";"Datum";"Name"|];
    [|"0001";"23.10.2003-042171717";"Target"|];
    [|"0002";"23.11.2003";"Task"|];
|]


let _ =
    tab#set_table vals;
    for col = 0 to cols-1
    do 
        tab#set_style_cell col 0 [Background "grey"];
        tab#set_style_text col 0 [Background "grey"];
    done;
    tab#set_text 1 1 "Changed";
    hbar1#container_add_s [
                exitbut#contained;
                chbut#contained;
                        ];
    vbar1#container_add_s [ 
                tab#contained;
                hbar1#contained;
                        ];
    let closewindow () = Display.closeDisplay (top#display); in

    exitbut#set_action ( fun () -> closewindow () );
    chbut#set_action (fun () -> 
                        incr chval;
                        let str = "Value: "^(string_of_int !chval) in
                        tab#set_text 1 1 str;
            );

    top#container_add vbar1#contained;
    top#show;
    try
        loop ();
    with Exit -> ();
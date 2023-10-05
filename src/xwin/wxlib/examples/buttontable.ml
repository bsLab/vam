
open WX_types 

let display = new WX_display.t ""
let root = new WX_root.t display 0

(* The top widget *)
let top = new WX_wmtop.t root 
    [MinWidth 100; MinHeight 100; MaxHeight 600]

(* Vertical and horizontal container widgets *)
let vbar1 = new WX_bar.v top#container [Background "black";]

let rows = 3 
let cols = 3

(* text table *)
let tab1 = new WX_table.text vbar1#container 
                cols rows 98 20 [||] [Background "black";IpadX 10; IpadY 10] 


let vals = [|
    [|"Nummer";"Datum";"Name"|];
    [|"0001";"Value: 0";"Target"|];
    [|"0002";"23.11.2003";"Task"|];
|]


(* button table *)

let buts = [|
    [|"Exit";"+";"-"|];
    [|"0001";"Startup";"Target"|];
    [|"0002";"Shutdown";"Task"|];
|]

let tab2 = new WX_table.button vbar1#container 
                cols rows 100 20 buts [IpadX 10; IpadY 10; Background "black";] 

(* and the bsslab logo displayed with a table ... *)
let logo = new WX_table.text vbar1#container
                2 1 0 0 [||] [Background "black"]




let _ =
    (*
    ** First the sliders ...
    *)
    let vb = new WX_bar.v vbar1#container [Background "black";
                                IpadX 10; IpadY 10] in
    let hb1 = new WX_bar.h vb#container [ExpandX true;IpadX 10; IpadY 10]
        in
    let sb1 = new WX_slider.h hb1#container [MinWidth 200] in
    let tb1 = new WX_label.t hb1#container "" [] in
    hb1#container_add_s [sb1#contained;tb1#contained];
    let hb2 = new WX_bar.h vb#container [ExpandX true;IpadX 10; IpadY 10]
        in
    let sb2 = new WX_slider.h hb2#container [MinWidth 200] in
    let tb2 = new WX_label.t hb2#container "" [] in
    hb2#container_add_s [sb2#contained;tb2#contained];
    vb#container_add_s [hb1#contained;hb2#contained];
    sb1#set_value 0.0;
    sb1#set_action ( fun () ->
            tb1#set_string (Printf.sprintf "U1 = %4.1f V "
                            sb1#get_value);
        );    
    sb2#set_value 0.0;
    sb2#set_color "white";
    sb2#set_backcolor "darkblue";
    sb2#set_range (3.0,10.0);
    sb2#set_delta (0.1);
    sb2#set_action ( fun () ->
            tb2#set_string (Printf.sprintf "I1 = %4.1f A "
                            sb2#get_value);
        );    

    (*
    ** The text table   
    *)
    let v = ref 0 in
    tab1#set_table vals;
    for col = 0 to cols-1 
    do 
        tab1#set_style_text col 0 [Background "grey70"];
        tab1#set_style_cell col 0 [Background "grey70"];
    done;

    vbar1#container_add_s [ 
                tab2#contained;
                tab1#contained;
                vb#contained;
                logo#contained;
                        ];
    let closewindow () = Display.closeDisplay (top#display); in

    (*
    ** The button table
    *)
    tab2#set_action 0 0 ( fun () -> closewindow () );
    tab2#set_action 1 0 ( fun () -> incr v; 
                                   ignore(sb1#incr_value);
                                   tab1#set_text  1 1  ("Value: "^
                                        (string_of_int !v));
                    );
    tab2#set_action 2 0 ( fun () -> decr v; 
                                   ignore(sb1#decr_value);
                                   tab1#set_text 1 1 ("Value: "^
                                        (string_of_int !v));
                    );
    
    tab2#set_action 1 1 ( fun () -> 
                logo#set_style_cell 0 0 [Background "black";
                                        Foreground "white"];
                logo#set_style_text 0 0 [Background "black";
                                         Foreground "white"];
                logo#set_style_cell 1 0 [Foreground "black"];
                logo#set_style_text 1 0 [Foreground "black"];
            );                            

    tab2#set_action 1 2 ( fun () -> 
                logo#set_style_cell 0 0 [Background "blue";
                                        Foreground "white"];
                logo#set_style_text 0 0 [Background "blue";
                                         Foreground "white"];
                logo#set_style_cell 1 0 [Foreground "blue"];
                logo#set_style_text 1 0 [Foreground "blue"];
            );                            
    tab2#set_style_all [Cursor (FontCursor 60)];

    tab2#set_group [(0,1);(0,2)] (0,1);
    tab2#set_action 0 1 (fun () ->
            Printf.printf "0 1 call"; print_newline ();
        );
    tab2#set_action 0 2 (fun () ->
            Printf.printf "0 2 call"; print_newline ();
        );
                            
    tab2#set_bistable 2 1 true;

    (*
    ** The logo
    *)
    logo#set_text 0 0 "BSS";
    logo#set_text 1 0 "LAB";
    logo#set_style_cell 0 0 [Background "blue";Foreground "white";
                        BorderWidth 0];
    logo#set_style_text 0 0 [Background "blue";
                             Foreground "white";];
    logo#set_style_cell 1 0 [Foreground "blue";
                        BorderWidth 0];
    logo#set_style_text 1 0 [Foreground "blue";
                        BorderWidth 0];

    logo#set_font 0 0 Fonts.Helvetica.Bold.s24;
    logo#set_font 1 0 Fonts.Helvetica.Bold.s24;

    top#container_add vbar1#contained;
    top#show;
    try
        loop ();
    with Exit -> ();

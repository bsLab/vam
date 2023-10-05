open X
open VX_types
open VX_box
open VX_text

let display = new VX_display.t "" ;;
let root = new VX_root.t display 0 ;;
let top = new VX_wmtop.t root 
    [MinHeight 300; MinWidth 500] ;;

let vbox = new VX_box.v top#container [Border 10;
                                       Background "green";
                                       MinHeight 100;
                                       MinWidth 100;
                                       ExpandY true;
                                       ] ;;
let hbox1 = new VX_box.h top#container [MinWidth 150;
                                        MinHeight 120;
                                       Border 2;  
                                       Background "yellow";
                                       ExpandX true;
                                       ] ;;
let hbox2 = new VX_box.h top#container [MinWidth 50;
                                        MinHeight 50;
                                        Border 2;  
                                       Background "yellow";
                                       ExpandX true;
                                       ExpandY true;
                                       ] ;;

let vbox1 = new VX_box.v top#container [Border 1;
                                       Background "lightblue";
                                       MinWidth 70;
                                       MinHeight 50;
                                       ] ;;
let vbox2 = new VX_box.v top#container [Border 1;
                                       Background "lightblue";
                                       OpadX 30;
                                       Width 20;
                                       Height 30;
                                       ] ;;

let vbox3 = new VX_box.v top#container [Border 1;
                                       Background "lightblue";
                                       ExpandX true;
                                       ExpandY true;
                                       ] ;;

let vbox4 = new VX_box.v top#container [Border 1;
(*
                                       Width 200;
                                       Height 100;
                                       PosX 20;
                                       PosY 20;
*)
                                       ExpandX true;
                                       ExpandY true;

                                       ]  ;;
let str = "This is a test multiline text paragraph with several words and many more..." ;;

let text1 = new VX_text.edit top#container str [
                    Border 5;
                    Background "gray60";
                    IpadX 10;
                    IpadY 10;
                    Width 200;
                    ] [Text_size 12;
                         Text_align Left;
                         Text_font Fixed;
                         Text_rows 5;
                         Text_baseline] ;;

let text2 = new VX_text.edit top#container "Test2" [
                    Border 5;
                    IpadX 2;
                    IpadY 2;
                    OpadX 5;
                    OpadY 20;
                    ] 
                        [Text_style Bold;
                         Text_font Times;
                         Text_cols 20];;

let _ =

    vbox#container_add_s [hbox1#contained;
                          hbox2#contained];
    hbox1#container_add_s [vbox1#contained;
                           vbox2#contained;
                           vbox3#contained;];

    hbox2#container_add_s [vbox4#contained];

    vbox4#container_add_s [
            text1#contained; 
            text2#contained
    ];

    top#setWM_NAME "VX test";
    top#container_add vbox#contained;
    top#show;


(*    
    vbox4#print 4;
    text1#print 8; 
    text2#print 8;
*)

    try
        loop ();
    with Exit -> ();


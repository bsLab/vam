open X
open VX_types
open VX_box
open VX_text
open VX_ps
open VX_tree
open Thread
open Math
open Printf

let display = new VX_display.t ""
let root = new VX_root.t display 0 
let top = new VX_wmtop.t root      
          [MinHeight 200; MinWidth 300]


let vbox = new VX_box.v top#container 
                            [
                                Border [];
                                Background "lightblue";
                                IpadX 10;
                                IpadY 10;
                                ExpandX true;
                                ExpandY true;
                                AdjustY true;
                            ] 
let hbox = new VX_box.h vbox#container 
                            [
                                ExpandX true;
                                Background "lightblue";
                            ] 
 

let view1 = new VX_view.h hbox#container [
                                ExpandX true; 
                                ExpandY false;
                                Border [Size 2];
                                But [Frame ReliefRaised;
                                     Color "grey80";
                                     Size 14;
                                    ];
                                IpadX 2;
                                IpadY 2;
                                Background "white";
                                ]

let edit = new VX_text.edit view1#container  "test"
                            [
                                Width 500;
                                IpadX 2;
                                IpadY 2;
                            ]
let _ =
    
    view1#container_add edit#contained;  
    hbox#container_add_s [view1#contained];
    vbox#container_add_s [hbox#contained];


    top#setWM_NAME "VX test";
    top#container_add vbox#contained;
    top#show;

    let printed = ref false in
    __(top#configure [Bindings [
        EnterWindow,(fun _ -> 
            if not !printed then
            begin
                printed := true;
                print_eps "view4.eps" 1.0 vbox#contained;
            end;
            );
        ]]);

    try
      loop ();
    with Exit -> ();

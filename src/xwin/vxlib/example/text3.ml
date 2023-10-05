open X
open VX_types
open VX_box
open VX_text
open VX_ps

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
                                AdjustX true;
                                ExpandX true;
                                Background "lightblue";
                            ] 

let hbox_glue = new VX_box.h hbox#container 
                            [
                                Background "white";
                                Border [];
                            ] 
 

let text1 = new VX_text.text hbox_glue#container "m" 
                            [
                                Text_font Symbol;
                                Text_size 24;
                            ]

let text2 = new VX_text.text hbox_glue#container "m" 
                            [
                                Text_font Times;
                                Text_size 24;
                            ]

let _ =
    hbox_glue#container_add_s [text1#contained;
                               text2#contained;];
    hbox#container_add_s [hbox_glue#contained];
    vbox#container_add_s [hbox#contained];

    top#setWM_NAME "VX test";
    top#container_add vbox#contained;
    top#show;
    let printed = ref false in
    top#configure [Bindings [
        EnterWindow,(fun _ ->
            if not !printed then
            begin
                printed := true;
                print_eps "text3.eps" 1.0 vbox#contained;
            end;
            );   
        ]];   

    try
      loop ();
    with Exit -> ();

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
 
let text1 = new VX_text.text hbox#container "VAM ground" 
                            [
                                Background "white";
                                Border []; 
                                IpadX 5;
                                IpadY 5;
                                Text_style Bold;
                                Text_font Times;
                                Text_size 20;
                                Text_baseline [Size 5]; 
                            ]

let _ =
    hbox#container_add_s [text1#contained];
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
                print_eps "text4.eps" 1.0 vbox#contained;
            end;
            );   
        ]];   

    try
      loop ();
    with Exit -> ();

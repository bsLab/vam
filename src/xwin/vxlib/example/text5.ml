open X
open VX_types
open VX_box
open VX_text
open VX_base
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
 
let str = "This is a test multiline text paragraph with embedded \\mu m and \\Sigma , several words and many more..."
let text1 = new VX_text.text hbox#container str 
                            [
                                Background "white";
                                Border []; 
                                IpadX 5;
                                IpadY 5;
                                Width 200;
                                Text_style Bold;
                                Text_font Helvetica;
                                Text_size 12;
                                Rown 5;            
                                Text_baseline [Size 1;Color "grey50"];
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
                print_eps "text5.eps" 1.0 vbox#contained;
            end;
            );   
        ]];   

    try
      loop ();
    with Exit -> ();

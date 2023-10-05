open X
open VX_types
open VX_box
open VX_text
open VX_checkbox
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
                                ExpandX true;
                                AdjustX true;
                                Background "lightblue";
                            ] 

let labels = [|
    "First";
    "Second";
    "Third";
|]

let attr = [
    Border [];
    IpadX 5;
    IpadY 10;
    Rows [|
            [Active true; Sym S_UP; Sym S_ERR];
            [Size 20; Active true; Sym S_ERR; Sym S_OK];
            [Sym S_QUEST;Sym S_CROSS];
    |];

]


let cb = new VX_checkbox.v  hbox#container 
                            labels 
                            attr

let _ =
    hbox#container_add_s [cb#contained];
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
                print_eps "checkbox4.eps" 1.0 vbox#contained;
            end;
            );   
        ]];      

    try
      loop ();
    with Exit -> ();

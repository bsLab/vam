open X
open VX_types
open VX_box
open VX_text
open VX_texttable
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

let tab_cont = [|
    [| "Name:";"";|];
    [| "Street:";"";|];
|]



let attr = [
    Background "white";
    IpadX 5;
    IpadY 5;
    Text_font Times;
    Text_style Bold;
    Text_size 12;
    Border [];
    Rows [|
        [IpadY 5];
        [IpadY 5];
    |];
    Cols [|
        [| 
            [Align Right;
             Width 50;];
            [Text_font Courier; Align Left; Mutable true;
             Width 100;
             Text_baseline [Size 2;Line L_Dotted];
            ];
        |];
        [| 
            [Align Right;
             Width 50;];
            [Text_font Courier; Align Left; Mutable true; 
             Width 100;
             Text_baseline [Size 1; Color "grey50"];
            ];
        |];
    

    
    |];
]

let tab = new VX_texttable.t hbox#container tab_cont attr  

let _ =
    hbox#container_add_s [tab#contained];
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
                print_eps "texttable3.eps" 1.0 vbox#contained;
            end;
            );   
        ]];   

    try
      loop ();
    with Exit -> ();

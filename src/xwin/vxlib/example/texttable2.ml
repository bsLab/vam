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
    [| "Col1";"Col2";|];
    [| "Col3";"Col4";|];
    [| "Col5";"Col6";|];
|]


let attr = [
    Background "white";
    IpadX 10;
    IpadY 5;
    Text_font Times;
    Text_style Bold;
    Text_size 18;
    Rows [|
        [IpadY 5];
        [IpadY 0];
        [IpadY 0];
    |];
    Cols [|
        [| 
            [Text_font Courier;Text_size 18; Mutable true;
             Align Right; Border [Sides [B_left;B_top;B_right;B_bottom]];
             Width 140;];
            [Align Left; Border [Sides [B_right;B_top;B_right;B_bottom]];
             Width 70;];
        |];
        [| 
            [Align Center; Border [Sides [B_left;B_top;B_right]];
             Width 70;];
            [Text_font Courier;Text_size 18; Mutable true;
             Align Left; Border [Sides [B_top;B_right;]];
             Width 140;];
        |];
        [| 
            [Text_font Times;Text_style Roman;Text_size 12;
             Align Left; Border [Sides [B_left;B_top;B_right;B_bottom]];
             Width 70];
            [Text_font Courier;Text_style Roman;Text_size 12;
             Mutable true;
             Align Left; Border [Sides [B_top;B_right;B_bottom]];
             Width 140;];
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
                print_eps "texttable2.eps" 1.0 vbox#contained;
            end;
            );   
        ]];   

    try
      loop ();
    with Exit -> ();

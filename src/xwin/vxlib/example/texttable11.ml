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

let str = "This is a multiline paragraphe with several lines and words..."
let tab_cont = [|
    [| "Col1";"Col2";|];
    [| "Col3";str;|];
    [| "Col5";"Col6";|];
|]


let attr = [
    Background "white";
    IpadX 4;
    IpadY 4;
    Rows [|
        [IpadY 5;];
        [];
        [];
    |];
    Cols [|
        [| 
            [Border [Sides [B_left;B_top;B_right; B_bottom]];
             Width 80;];
            [Border [Sides [B_right;B_top;B_right; B_bottom]];
             Width 50;];
        |];
        [| 
            [Align Top; Border [Sides [B_left;B_top;]];
             Width 50;Text_style Bold];
            [Rown 0; Border [Size 2; Sides [B_left;B_top;B_right;]];
             Width 80;Mutable true];
        |];
        [| 
            [
             Border [Sides [B_left;B_top;B_bottom]];
             Width 50;Text_style Bold];
            [
              Border [Size 2; Sides [B_left;B_top;B_right;B_bottom]];
             Width 80;Mutable true];
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
                print_eps "texttable11.eps" 1.0 vbox#contained;
            end;
            );   
        ]];   

    try
      loop ();
    with Exit -> ();

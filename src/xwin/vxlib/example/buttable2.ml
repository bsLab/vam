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
                                Border [];
                                MinWidth 100;
                                ExpandX true;
                                Background "white";
                            ] 
let closewindow () = 
        Display.closeDisplay (top#display)

let tab_cont = [|
    [| "Start";"Exit";|];
    [| "Stop";"Col4";|];
    [| "Col5";"Col6";|];
|]


let attr = [
    IpadX 20;
    IpadY 5;
    ExpandX true;
    MinWidth 250;
    But [
        Shape S_Oval;
        Frame ShadowRaised];
    Cols [|
        [| 
            [ExpandX true];
            [ExpandX true;
             ActionUU (fun () -> closewindow(); exit 0)];
        |];
        [| 
            [ExpandX true];
            [ExpandX true];
        |];
        [| 
            [ExpandX true];
            [ExpandX true];
        |];
    |];
]

let tab = new VX_button.table hbox#container tab_cont attr  

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
                print_eps "buttable2.eps" 1.0 vbox#contained;
            end;
            );
        ]];

    try
      loop ();
    with Exit -> ();

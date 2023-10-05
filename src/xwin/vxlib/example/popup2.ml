open X
open VX_types
open VX_box
open VX_text
open VX_button
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
let hbox1 = new VX_box.h top#container 
                            [
                                Background "grey90";
                                IpadY 5;
                                Border [];
                                AdjustX true;
                                ExpandX true;
                                Background "grey90";
                            ] 
 
let popup1 = new VX_popup.t root top#container "Address" 
                            [
                                Background "lightblue";
                                IpadX 5;
                                IpadY 5;
                                But [Frame ReliefRaised];
                            ]

let closewindow () = 
        Display.closeDisplay (top#display)

let but2 = new VX_button.t top#container "EXIT" 
                            [
                                Background "lightblue";
                                IpadX 3;
                                IpadY 3;
                                Text_style Bold;
                                Text_font Helvetica;
                                Border [
                                    Shape S_Rect;
                                    Frame ReliefRaised;
                                    Size 2];
                                ActionUU (fun () ->
                                    closewindow ();
                                    exit 0;
                                    );
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
            [
            ];
            [Text_font Courier; Align Left; Mutable true;
             Width 200;
             Text_baseline [Color "grey50";];
            ];
        |];
        [| 
            [
            ];
            [Text_font Courier; Align Left; Mutable true; 
             Width 200;
             Text_baseline [Color "grey50";];
            ];
        |];
    

    
    |];
]

let tab = new VX_texttable.t top#container tab_cont attr  


let _ =
    hbox1#container_add_s [popup1#contained;but2#contained];
    vbox#container_add_s [hbox1#contained;];
    popup1#container_add tab#contained;

    top#setWM_NAME "VX test";
    top#container_add vbox#contained;
    top#show;
    let printed = ref false in  
    top#configure [Bindings [
        EnterWindow,(fun _ ->
            if not !printed then
            begin
                printed := true;
                print_eps "popup2.eps" 1.0 vbox#contained;
            end;
            );
        ]];

    try
      loop ();
    with Exit -> ();

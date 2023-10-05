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
let hbox1 = new VX_box.h vbox#container 
                            [
                                Background "grey90";
                                IpadY 5;
                                Border [];
                                AdjustX true;
                                ExpandX true;
                            ] 
 
let popup1 = new VX_popup.t root hbox1#container "Select" 
                            [
                                Background "lightblue";
                                IpadX 5;
                                IpadY 5;
                                But [Frame ReliefRaised];
                            ]

let closewindow () = 
        Display.closeDisplay (top#display)

let but2 = new VX_button.t hbox1#container "EXIT" 
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


let labels = [|
    "First";
    "Second";
    "Third";
|]

let attr = [
    Background "white";
    Border [];
    IpadX 5;
    IpadY 5;
    Rows [|
            [Active true; Sym S_UP; Sym S_DOWN];
            [Sym S_BLANK; Sym S_OK];
            [Sym S_QUEST;Sym S_CROSS];
    |];

]
let cb = new VX_checkbox.v  popup1#container 
                            labels 
                            attr


let _ =
    hbox1#container_add_s [popup1#contained;but2#contained];
    vbox#container_add_s [hbox1#contained;];
    popup1#container_add cb#contained;

    top#setWM_NAME "VX test";
    top#container_add vbox#contained;
    top#show;
    let printed = ref false in
    top#configure [Bindings [
        EnterWindow,(fun _ ->
            if not !printed then
            begin
                printed := true;
                print_eps "popup1.eps" 1.0 vbox#contained;
            end;
            );
        ]];

    try
      loop ();
    with Exit -> ();

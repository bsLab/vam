open X
open VX_types
open VX_box
open VX_text
open VX_button
open VX_ps
open Myenv
open VX_common

let _ = vx_warn := true

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
                                IpadY 5;
                                Border [];
                                AdjustX true;
                                ExpandX true;
                                Background "white";
                            ] 
 
let popup1 = new VX_popup.file_select_editW root hbox1#container 
                    "Startup file"
                    (Unix_path "/")
                    [
                        MinWidth 300;
                        IpadX 5;
                        IpadY 5;
                        But [Frame ReliefRaised;
                             Color "grey80"];
                    ]

let closewindow () = 
        Display.closeDisplay (top#display)

let but2 = new VX_button.t hbox1#container "EXIT" 
                            [
                                IpadX 3;
                                IpadY 3;
                                Text_style Bold;
                                Text_font Helvetica;
                                Border [
                                    Frame ReliefRaised;
                                    ];
                                Background "grey80";
                                ExpandY true;
                                ActionUU (fun () ->
                                    closewindow ();
                                    exit 0;
                                    );
                            ]



let _ =
    hbox1#container_add_s [popup1#contained;but2#contained];
    vbox#container_add_s [hbox1#contained;];

    top#setWM_NAME "VX test";
    top#container_add vbox#contained;
    top#show;

    let printed = ref false in  
    top#configure [Bindings [
        EnterWindow,(fun _ ->
            if not !printed then
            begin
                printed := true;
                print_eps "popup5.eps" 1.0 vbox#contained;
            end;
            );
        ]];

    try
      loop ();
    with Exit -> ();

open X
open VX_types
open VX_box
open VX_ps

let display = new VX_display.t ""
let root = new VX_root.t display 0 
let top = new VX_wmtop.t root      
          [Background "white";MinHeight 300; MinWidth 500]

let vbox = new VX_box.v top#container [Border [];
                                       Background "lightblue";
                                       IpadX 10;
                                       IpadY 10;
                                        ] 

let hbox1 = new VX_box.h vbox#container [Width 150;
                                        Height 120;
                                        Border [Frame ShadowRaised;
                                                Size 2];
                                        Background "gray70";
                                        ]

let hbox2 = new VX_box.h vbox#container [Width 100;
                                        Height 100;
                                        Border [Frame ShadowSunken];
                                        Background "gray70";
                                        ] 

let _ =
    vbox#container_add_s [hbox1#contained;  
                          hbox2#contained];  

    top#setWM_NAME "VX test";
    top#container_add vbox#contained;
    top#show;


    let printed = ref false in

    top#configure [Bindings [
        EnterWindow,(fun _ -> 
            if not !printed then
            begin
                printed := true;
                print_eps "border1.eps" 1.0 vbox#contained;
            end;
            );
        ]];

    try
      loop ();
    with Exit -> ();

open X
open VX_types
open VX_box
open VX_ps

let display = new VX_display.t ""
let root = new VX_root.t display 0 
let top = new VX_wmtop.t root      
          [MinHeight 300; MinWidth 500;Background "white"]

let vbox = new VX_box.v top#container [Border [Size 10];
                                       Background "green";
                                       IpadX 10;
                                       IpadY 10;
                                        ] 

let hbox1 = new VX_box.h vbox#container [Width 150;
                                        Height 120;
                                        Border [Size 2];
                                        Background "yellow";
                                        ]

let hbox2 = new VX_box.h vbox#container [Width 100;
                                        Height 100;
                                        Border [Size 2];
                                        Background "yellow";
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
                print_eps "box1.eps" 0.5 vbox#contained;
                print_ps "box1.ps" A4 vbox#contained;
            end;
            );
        ]];

    try
      loop ();
    with Exit -> ();

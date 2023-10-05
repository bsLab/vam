open X
open VX_types
open VX_box
open VX_text
open VX_ps
open Thread
open Math

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
 
let slider1 = new VX_slider.view_v 
                            hbox#container  
                            400
                            100
                            90
                            [
                                Background "grey90";
                                Border []; 
                                But [Frame ReliefRaised;
                                     Color "lightblue";
                                     Size 18;];
                                IpadX 5;
                                IpadY 5;
                                Height 200;  
                                ActionIU (fun pos ->
                                    Printf.printf "Position view %d" pos;
                                    print_newline ();
                                  );
                                ActionUU (fun () ->
                                    Printf.printf "All done.";
                                    print_newline ();
                                  );
                            ]

let _ =
    hbox#container_add_s [slider1#contained];
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
                print_eps "slider4.eps" 1.0 vbox#contained;
            end;
            );
        ]];

    try
      loop ();
    with Exit -> ();

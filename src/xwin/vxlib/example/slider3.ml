open X
open VX_types
open VX_box
open VX_text
open VX_ps
open Thread
open Math
open Printf

let display = new VX_display.t ""
let root = new VX_root.t display 0 
let top = new VX_wmtop.t root      
          [MinHeight 200; MinWidth 300;
           Background "white";
           IpadX 10;
           IpadY 10;]

let vbox = new VX_box.v top#container 
                            [
                                Border [];
                                Background "lightblue";
                                IpadX 10;
                                IpadY 5;
                                ExpandX true;
(*                                ExpandY true; *)
(*                                AdjustY true; *)
                            ] 
let hbox1 = new VX_box.h vbox#container 
                            [
                                AdjustX true;
                                ExpandX true; 
                                Background "lightblue";
                            ] 
let hbox2 = new VX_box.h vbox#container 
                            [
                                AdjustX true;
                                ExpandX true; 
                                Background "lightblue";
                            ] 

let valtext1 = new VX_text.label vbox#container "Angle \\alpha =  0\\mu m"
                            [
                                Background "white";
                                ExpandY true;
                                IpadX 10;
                                Text_font Fixed;
                                Border [];
                                Width 120;
                            ]
let slider1 = new VX_slider.val_h 
                            hbox1#container  
                            0       (* min *)
                            100     (* max *)
                            2       (* step small  *)
                            10      (* step large  *)
                            [
                                Background "grey90";
                                Border []; 
                                But [Frame ReliefRaised;
                                     Color "lightblue";
                                     Size 18;];
                                IpadX 6;
                                IpadY 5;
                                Width 200;  
                                ActionIU (fun pos ->
                                    valtext1#set_text
                                        (sprintf "Angle \\alpha =%3d\\mu m" pos);
                                  );
                                ActionUU (fun () ->
                                    Printf.printf "All done.";
                                    print_newline ();
                                  );
                            ]
let valtext2 = new VX_text.label vbox#container "Angle \\beta =  0\\mu m"
                            [
                                Background "white";
                                ExpandY true;
                                IpadX 10;
                                Text_font Fixed;
                                Border [];
                                Width 120;
                            ]
let slider2 = new VX_slider.val_h 
                            hbox2#container  
                            0       (* min *)
                            100     (* max *)
                            2       (* step small  *)
                            10      (* step large  *)
                            [
                                Background "grey90";
                                Border []; 
                                But [Frame ReliefRaised;
                                     Color "lightblue";
                                     Size 18;];
                                IpadX 6;
                                IpadY 5;
                                Width 200;  
                                ActionIU (fun pos ->
                                    valtext2#set_text
                                        (sprintf "Angle \\beta =%3d\\mu m" pos);
                                  );
                                ActionUU (fun () ->
                                    Printf.printf "All done.";
                                    print_newline ();
                                  );
                            ]

let _ =
    hbox1#container_add_s [slider1#contained;
                          valtext1#contained];
    hbox2#container_add_s [slider2#contained;
                          valtext2#contained];
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
                print_eps "slider3.eps" 1.0 vbox#contained;
            end;
            );
        ]];

    try
      loop ();
    with Exit -> ();

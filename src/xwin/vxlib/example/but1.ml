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
                                Border [Size 1];
                                Background "lightblue";
                                IpadX 10;
                                IpadY 10;
                                ExpandX true;
                                ExpandY true;
                                AdjustY true;
                            ] 
let hbox1 = new VX_box.h top#container 
                            [
                                IpadY 5;
                                Border [Size 1];
                                AdjustX true;
                                ExpandX true;
                                Background "white";
                            ] 
let hbox2 = new VX_box.h top#container 
                            [
                                IpadY 5;
                                Border [Size 1];
                                AdjustX true;
                                ExpandX true;
                                Background "white";
                            ] 
let hbox3 = new VX_box.h top#container 
                            [
                                IpadY 5;
                                Border [Size 1];
                                AdjustX true;
                                ExpandX true;
                                Background "white";
                            ] 
 
let but1 = new VX_button.t hbox1#container "DOIT" 
                            [
                                IpadX 3;
                                IpadY 3;
                                Text_style Bold;
                                Text_font Helvetica;
                                Border [Shape S_Rect; 
                                        Frame ShadowRaised;
                                        Size 1];
                                ActionUU (fun () ->
                                print_string "action"; print_newline ();        
                                        
                                    );
                            ]

let closewindow () = 
        Display.closeDisplay (top#display)

let but2 = new VX_button.t hbox1#container "EXIT" 
                            [
                                IpadX 3;
                                IpadY 3;
                                Text_style Bold;
                                Text_font Helvetica;
                                Border [Shape S_Rect; 
                                        Frame ShadowRaised;
                                        Size 1];
                                ActionUU (fun () ->
                                    closewindow ();
                                    exit 0;
                                    );
                            ]

let but3 = new VX_button.t hbox2#container "OVAL" 
                            [
                                IpadX 10;
                                IpadY 5;
                                Text_style Bold;
                                Text_font Helvetica;
                                Text_size 14;
                                Border [Shape S_Oval; 
                                        Frame ShadowRaised;
                                        Size 1];
                                ActionUU (fun () ->
                                print_string "action"; print_newline ();        
                                        
                                    );
                            ]
let but4 = new VX_button.t hbox2#container "OVAL2" 
                            [
                                IpadX 10;
                                IpadY 3;
                                Text_style Bold;
                                Text_font Helvetica;
                                Text_size 14;
                                Border [Shape S_Oval; 
                                        Frame ShadowRaised;
                                        Size 3];
                                ActionUU (fun () ->
                                print_string "action"; print_newline ();        
                                        
                                    );
                            ]

let but5 = new VX_button.t hbox3#container "OVAL" 
                            [
                                IpadX 10;
                                IpadY 3;
                                Text_style Bold;
                                Text_font Helvetica;
                                Text_size 12;
                                Border [Shape S_Oval; 
                                        Frame ShadowRaised;
                                        Size 2];
                                ActionUU (fun () ->
                                print_string "action"; print_newline ();        
                                        
                                    );
                            ]
let but6 = new VX_button.t hbox3#container "OVAL2" 
                            [
                                IpadX 10;
                                IpadY 3;
                                Text_style Bold;
                                Text_font Helvetica;
                                Text_size 12;
                                Border [Shape S_Oval; 
                                        Frame ShadowRaised;
                                        Size 2];
                                ActionUU (fun () ->
                                print_string "action"; print_newline ();        
                                        
                                    );
                            ]
open Thread

let _ =
    hbox1#container_add_s [but1#contained;but2#contained];
    hbox2#container_add_s [but3#contained;but4#contained];
    hbox3#container_add_s [but5#contained;but6#contained];
    vbox#container_add_s [hbox1#contained;
                          hbox2#contained;
                          hbox3#contained];

    top#setWM_NAME "VX test";
    top#container_add vbox#contained;
    top#show;

    let printed = ref false in

    top#configure [Bindings [
        EnterWindow,(fun _ -> 
            if not !printed then
            begin
                printed := true;
                print_eps "but1.eps" 1.0 vbox#contained;
                print_ps "but1.ps" A4 vbox#contained;
            end;
            );
        ]];
    try
      loop ();
    with Exit -> ();

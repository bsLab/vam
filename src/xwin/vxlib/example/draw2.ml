open X
open VX_types
open VX_box
open VX_text
open VX_ps
open VX_draw
open Thread
open Math
open Printf

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
                            ] 

let draw1 = new VX_draw.t vbox#container [
                                MinWidth 300;
                                MinHeight 300;
                                ExpandX true;
                                ExpandY true;
                                Border [Size 2];
                                Background "white";
                            ]
let _ =
    vbox#container_add_s [draw1#contained];

    top#setWM_NAME "VX test";
    top#container_add vbox#contained;
    top#show;

    draw1#clear;
    let path1 = [
        Y ([PC "blue";Font_size 0.05; AL [Center;Middle]],[
            TX ({t_rp=pt 0.3 0.8;t_an=0.0},"Teststring \\sigma X")]);
        LP [pt 0.25 0.8;pt 0.35 0.8];
        LP [pt 0.3 0.75;pt 0.3 0.85]; 
        ] in
    let path2 = [
        X (R {rc=pt 0.5 0.5;phi=10.0}, 
            [ 
            Y ([LW 0.01;PC "red";FC "grey90";FILL],[LP [
                            pt 0.1 0.1;
                            pt 0.5 0.1;
                            pt 0.5 0.5;
                            pt 0.1 0.5;
                            pt 0.1 0.1;
                            ]]
              )
            ]);
        ] in
    let path3 = [
        F ({f_fc="";f_bc="";f_bw=0.01;f_bx=None}, [
        X (T {dx=0.3;dy=0.3}, [X (R {rc=pt 0.2 0.3;phi=90.0}, [
            CR {cp=pt 0.2 0.3;
                rx=0.1;
                ry=0.2;}])])]);

        ] in
    
    draw1#add_path path1;
    draw1#add_path path2;
    draw1#add_path path3;


    let printed = ref false in
    __(top#configure [Bindings [
        EnterWindow,(fun _ -> 
            if not !printed then
            begin
                printed := true;
                print_eps "draw2.eps" 1.0 vbox#contained;
            end;
    draw1#delete_path 2;
            );
        ]]);

    try
      loop ();
    with Exit -> ();

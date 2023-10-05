open X
open VX_types
open VX_ps

let display = new VX_display.t "" 
let root = new VX_root.t display 0 
let top = new VX_wmtop.t root [MinHeight 300;
                               MinWidth 500;
                               Background "white"]

let vbox = new VX_box.v top#container [Border [Size 10];
                                       Background "green";
                                       Height 295;
                                       Width 250;
                                       ] 
let hbox1 = new VX_box.h vbox#container [MinWidth 150;
                                        MinHeight 120;
                                        Border [Size 2];
                                        Background "yellow";
                                        ExpandX true;
                                        ]
let hbox2 = new VX_box.h vbox#container [MinWidth 50;
                                        MinHeight 50;
                                        Border [Size 2];
                                        Background "yellow";
                                        ExpandX true;
                                        ExpandY true;
                                        ] 
let vbox1 = new VX_box.v hbox1#container [Border [Size 1];
                                        Background "lightblue";
                                        Width 70;
                                        Height 50;
                                        ] 
let vbox2 = new VX_box.v hbox1#container [Border [Size 1];
                                        Background "lightblue";
                                        OpadX 30;
                                        Width 20;
                                        Height 30;
                                        ] ;;
let vbox3 = new VX_box.v hbox1#container [Border [Size 1];
                                        Background "lightblue";
                                        ExpandX true;
                                        ExpandY true;
                                        ] 
let vbox4 = new VX_box.v hbox2#container [Border [Size 1];
                                        Background "red";
                                        Width 20;
                                        Height 30;
                                        PosX 40;
                                        PosY 20;
                                        ]

let _ =
    vbox#container_add_s [hbox1#contained;
                          hbox2#contained];
     hbox1#container_add_s [vbox1#contained;
                            vbox2#contained;
                            vbox3#contained;
                            ];
    hbox2#container_add_s [vbox4#contained];

    top#setWM_NAME "VX test";
    top#container_add vbox#contained;
    top#show;

    let printed = ref false in
    top#configure [Bindings [ 
        EnterWindow,(fun _ ->   
            if not !printed then
            begin
                printed := true;
                print_eps "box7.eps" 0.5 vbox#contained;
            end;
            );
        ]];

    try
         loop ();
    with Exit -> ();

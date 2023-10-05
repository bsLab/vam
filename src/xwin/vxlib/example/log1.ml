open X
open VX_types
open VX_box
open VX_text
open VX_ps
open Thread

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
                                ExpandX true;
                                Background "lightblue";
                            ] 
 

let lines = [
    "let hbox = new VX_box.h vbox#container";
    "let log1 = new VX_text.log hbox#container 10";
    "let vbox = new VX_box.v top#container";
]

let log1 = new VX_log.t hbox#container 10
                            [
                                ExpandX true;
                                Background "white";
                                Border []; 
                                IpadX 5;
                                IpadY 5;
                                Text_font Fixed;
                                Text_size 10;
                            ]

let _ =
    hbox#container_add_s [log1#contained];
    vbox#container_add_s [hbox#contained];

    log1#add_lines lines;
    top#setWM_NAME "VX test";
    top#container_add vbox#contained;
    top#show;

    __(thread_create (fun () ->
        while true
        do
            __(thread_delay 2 SEC);
            log1#add_lines ["more1...";"more2............";];
        done;
        ) ());

    let printed = ref false in
    __(top#configure [Bindings [
        EnterWindow,(fun _ -> 
            if not !printed then
            begin
                printed := true;
                print_eps "log1.eps" 1.0 vbox#contained;
            end;
            );
        ]]);

    try
      loop ();
    with Exit -> ();

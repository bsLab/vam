(*
** Editable log window
*)
open X
open VX_types
open VX_box
open VX_text
open VX_ps
open Thread
open Printf

let out str =
  print_string str;
  print_newline ()
  

let display = new VX_display.t ""
let root = new VX_root.t display 0 
let top = new VX_wmtop.t root      
          [MinHeight 200; MinWidth 400]

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
    ">> ";
]

let log1 = new VX_log.edit hbox#container 40 10
                            [
                                ExpandX true;
                                Background "white";
                                Border []; 
                                IpadX 5;
                                IpadY 5;
                                Text_font Fixed;
                                Text_size 12;
                            ]

let _ =
    hbox#container_add_s [log1#contained];
    vbox#container_add_s [hbox#contained];

    log1#add_lines lines;
    top#setWM_NAME "VX test";
    top#container_add vbox#contained;
    top#show;


    log1#set_action_left (fun row ->
        out (sprintf "LEFT: row %d" row);
        );
    log1#set_action_right (fun col row ->
        out (sprintf "RIGHT: col %d row %d" col row);
        );
    log1#set_action_up (fun row ->
        out (sprintf "UP: row %d" row);
        );
    log1#set_action_down (fun row ->
        out (sprintf "DOWN: row %d" row);
        );

    log1#set_cursor 3 10;    
    let cur_row = ref 3 in
    log1#set_editrange 3 !cur_row !cur_row;
    log1#set_action_nl (fun row ->
        let str = "OK." in
        log1#add_lines [str];
        log1#add_lines [">> "];
        cur_row := min (!cur_row + 2) 9;
        log1#set_editrange 3 !cur_row !cur_row;
      );
    
    try
      loop ();
    with Exit -> ();

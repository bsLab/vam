open Amoeba
open Stderr

open X
open VX_types
open VX_box
open VX_text
open VX_ps
open VX_tree
open VX_file
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
                                AdjustY true;
                            ] 
let hbox = new VX_box.h vbox#container 
                            [
                                AdjustX true;
                                ExpandX true;
                                Background "lightblue";
                            ] 
 

let view1 = new VX_view.hv hbox#container [
                                Width 300;
                                Height 300;
                                Border [Size 2];
                                But [Frame ReliefRaised;
                                     Color "grey80";
                                     Size 14;
                                    ];
                                IpadX 2;
                                IpadY 2;
                                Background "white"; 
                                ]

let tree1 = new VX_tree.t view1#container  
                            [
                                Border [];
                                IpadX 2;
                                IpadY 2;
                            ]
let _ =
    hbox#container_add_s [view1#contained];
    view1#container_add tree1#contained;  
    vbox#container_add_s [hbox#contained];

    let tree = tree1#get_root in
    tree.b_label <- "/";
    Db.set_level (-1); 

    tree1#set_action [
        ActionSU (fun str ->
            printf "Opening %s... " str;
            print_newline ();
            let node = tree1#get_node str in
            let stat = unix_file_tree tree1 str node in
            if stat <> std_OK then
                (printf "failed: %s" (err_why stat))
            else
                printf "Ok.";
            print_newline ();
            );
        ActionSU (fun str ->
            printf "Closing %s" str;
            let node = tree1#get_node str in
            node.b_child <- None;
            print_newline ();
            );
        ActionSU (fun str ->
            printf "Selected %s" str;
            print_newline ();
            );
            ];

    tree1#update;



    top#setWM_NAME "VX test";
    top#container_add vbox#contained;
    top#show;

    let printed = ref false in
    __(top#configure [Bindings [
        EnterWindow,(fun _ -> 
            if not !printed then
            begin
                printed := true;
                print_eps "file1.eps" 1.0 vbox#contained;
            end;
            );
        ]]);

    try
      loop ();
    with Exit -> ();

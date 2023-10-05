open X
open VX_types
open VX_box
open VX_text
open VX_ps
open VX_tree
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
 
let tree1 = new VX_tree.t hbox#container  
                            [
                                Background "white";
                                Border []; 
                                IpadX 5;
                                IpadY 5;
                                Width 120;
                                Height 150;
                                But [
                                    ActionSU (fun str ->
                                        printf "Opening %s" str;
                                        print_newline ();
                                        );
                                    ActionSU (fun str ->
                                        printf "Closing %s" str;
                                        print_newline ();
                                        );
                                    ActionSU (fun str ->
                                        printf "Selected %s" str;
                                        print_newline ();
                                        );
                                    ];
                            ]

let _ =
    hbox#container_add_s [tree1#contained];
    vbox#container_add_s [hbox#contained];

    let tree = tree1#get_root in
    tree.b_label <- "Filesystem";
    let b1 = tree1#new_node "Test" in
    
    let l1 = tree1#new_leaf "Entry1" in
    let l2 = tree1#new_leaf "Entry2" in
    let l3 = tree1#new_leaf "Main" in
    let l4 = tree1#new_leaf "Last" in
    tree1#tree_add_s tree [Leaf l3;(Node b1);Leaf l4]; 
    tree1#tree_add_s b1 [Leaf l1;Leaf l2];
    b1.b_opened <- true; 
    tree.b_opened <- true;
    tree1#update;

    top#setWM_NAME "VX test";
    top#container_add vbox#contained;
    top#show;


    let printed = ref false in
    top#configure [Bindings [
        EnterWindow,(fun _ -> 
            if not !printed then
            begin
                printed := true;
                print_eps "tree1.eps" 1.0 vbox#contained;
            end;
            );
        ]];

    try
      loop ();
    with Exit -> ();

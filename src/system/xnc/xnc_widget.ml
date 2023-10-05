

open Xtypes
open WX_types
open WX_tree
open Unix

open Printf
open Amoeba
open Ar
open Dir
open Name
open Stderr
open Stdcom
open Cap_env
open Thread

let _ = Db.set_level 1
let _ = get_env_cap "ROOT"
let dns_root = ref nilcap


(*
** <labeltext> <input text field> <button>
*)

type xw_label_input_button = {
    mutable xw_tib_hbar   : WX_bar.h;
    mutable xw_tib_label  : WX_label.t;
    mutable xw_tib_input  : WX_ledit.t;
    mutable xw_tib_button : WX_button.with_label;
    mutable xw_tib_str : string;
}

(*
** <text> <label1> <input1> <label2> <input2> ... <button>
*)

type xw_text_4label_4input_button = {
    mutable xw_t4lib_hbar   : WX_bar.h;
    mutable xw_t4lib_label  : WX_label.t;
    mutable xw_t4lib_label1  : WX_label.t;
    mutable xw_t4lib_input1  : WX_ledit.t;
    mutable xw_t4lib_str1    : string;
    mutable xw_t4lib_label2  : WX_label.t;
    mutable xw_t4lib_input2  : WX_ledit.t;
    mutable xw_t4lib_str2    : string;
    mutable xw_t4lib_label3  : WX_label.t;
    mutable xw_t4lib_input3  : WX_ledit.t;
    mutable xw_t4lib_str3    : string;
    mutable xw_t4lib_label4  : WX_label.t;
    mutable xw_t4lib_input4  : WX_ledit.t;
    mutable xw_t4lib_str4    : string;
    mutable xw_t4lib_button : WX_button.with_label;
}

(*
** <text> <label1> <input1> <button1> ...
*)

type xw_text_3label_3input_3button = {
    mutable xw_t3lib_hbar   : WX_bar.h;
    mutable xw_t3lib_label  : WX_label.t;
    mutable xw_t3lib_label1  : WX_label.t;
    mutable xw_t3lib_input1  : WX_ledit.t;
    mutable xw_t3lib_button1 : WX_button.with_label;
    mutable xw_t3lib_str1    : string;
    mutable xw_t3lib_label2  : WX_label.t;
    mutable xw_t3lib_input2  : WX_ledit.t;
    mutable xw_t3lib_button2 : WX_button.with_label;
    mutable xw_t3lib_str2    : string;
    mutable xw_t3lib_label3  : WX_label.t;
    mutable xw_t3lib_input3  : WX_ledit.t;
    mutable xw_t3lib_button3 : WX_button.with_label;
    mutable xw_t3lib_str3    : string;
}

(*
** <text> <label1> <value1> ...
*)

type xw_text_3label_3value = {
    mutable xw_t3lv_vbar : WX_bar.v;
    mutable xw_t3lv_text : WX_label.t;
    mutable xw_t3lv_hbar1  : WX_bar.h;
    mutable xw_t3lv_label1 : WX_label.t;
    mutable xw_t3lv_value1 : WX_label.t;
    mutable xw_t3lv_str1   : string;
    mutable xw_t3lv_hbar2  : WX_bar.h;
    mutable xw_t3lv_label2 : WX_label.t;
    mutable xw_t3lv_value2 : WX_label.t;
    mutable xw_t3lv_str2   : string;
    mutable xw_t3lv_hbar3  : WX_bar.h;
    mutable xw_t3lv_label3 : WX_label.t;
    mutable xw_t3lv_value3 : WX_label.t;
    mutable xw_t3lv_str3   : string;
}

(*
** <text> <label1> <value1> <but1> ...
*)

type xw_text_3label_3value_3button = {
    mutable xw_t3lvb_vbar : WX_bar.v;
    mutable xw_t3lvb_text : WX_label.t;
    mutable xw_t3lvb_hbar1  : WX_bar.h;
    mutable xw_t3lvb_label1 : WX_label.t;
    mutable xw_t3lvb_value1 : WX_label.t;
    mutable xw_t3lvb_but1   : WX_button.with_label;
    mutable xw_t3lvb_str1   : string;
    mutable xw_t3lvb_hbar2  : WX_bar.h;
    mutable xw_t3lvb_label2 : WX_label.t;
    mutable xw_t3lvb_value2 : WX_label.t;
    mutable xw_t3lvb_but2   : WX_button.with_label;
    mutable xw_t3lvb_str2   : string;
    mutable xw_t3lvb_hbar3  : WX_bar.h;
    mutable xw_t3lvb_label3 : WX_label.t;
    mutable xw_t3lvb_value3 : WX_label.t;
    mutable xw_t3lvb_but3   : WX_button.with_label;
    mutable xw_t3lvb_str3   : string;
}

(*
** <label>
** <but1> ...
*)

type xw_label_4but = {
    mutable xw_l4b_vbar : WX_bar.h;
    mutable xw_l4b_label : WX_label.t;
    mutable xw_l4b_hbar : WX_bar.h;
    mutable xw_l4b_but1 : WX_button.with_label;
    mutable xw_l4b_but2 : WX_button.with_label;
    mutable xw_l4b_but3 : WX_button.with_label;
    mutable xw_l4b_but4 : WX_button.with_label;
    mutable xw_l4b_state1 : bool;
    mutable xw_l4b_state2 : bool;
    mutable xw_l4b_state3 : bool;
    mutable xw_l4b_state4 : bool;
}

(*
** File tree
*)

type xw_tree = {
    mutable xw_t_hbar : WX_bar.h;
    mutable xw_t_vbar : WX_bar.v;
    mutable xw_t_view : WX_viewport.t;
    mutable xw_t_adj_h : WX_adjust.t;
    mutable xw_t_adj_v : WX_adjust.t;
    mutable xw_t_scroll_h : WX_scrollbar.h;
    mutable xw_t_scroll_v : WX_scrollbar.v;
    mutable xw_t_tree : WX_tree.t;
}

(*
** More buttons...
*)

type xw_button9 = {
    mutable xw_b9_hbar : WX_bar.h;
    mutable xw_b9_vbar : WX_bar.v;    
    mutable xw_b9_but1 : WX_button.with_label;
    mutable xw_b9_but2 : WX_button.with_label;
    mutable xw_b9_but3 : WX_button.with_label;
    mutable xw_b9_but4 : WX_button.with_label;
    mutable xw_b9_but5 : WX_button.with_label;    
    mutable xw_b9_but6 : WX_button.with_label;
    mutable xw_b9_but7 : WX_button.with_label;
    mutable xw_b9_but8 : WX_button.with_label;    
    mutable xw_b9_but9 : WX_button.with_label;    
}

(*
** Log window with scrollbars
*)

type xw_log = {
    mutable xw_log_hbar : WX_bar.h;
    mutable xw_log_adx : WX_adjust.t;
    mutable xw_log_ady : WX_adjust.t;
    mutable xw_log_scroll : WX_scrollbar.v;
    mutable xw_log_view : WX_viewport.t;
    mutable xw_log_text : WX_text.of_string;
    mutable xw_log_str : string;

}

(*
** Main widget:
**
** <cnc_server path input>
** <encoder path input>
** <p1>
** <p2>
** <p3>
** <p4>
** <p5>
** <p6>
** <p7>
** <p8>
** ...
**
*)

type xw_main = {
    mutable xw_dpy : WX_display.t;
    mutable xw_root : WX_root.t;
    mutable xw_top : WX_wmtop.t;
    mutable xw_quit : WX_button.with_label;
    mutable xw_vbar_server : WX_bar.v;
    mutable xw_cnc_server : xw_label_input_button;  
    mutable xw_enc_server : xw_label_input_button;  
    mutable xw_comms      : xw_label_input_button list;
    mutable xw_tool       : xw_text_4label_4input_button;
    mutable xw_solid      : xw_text_3label_3input_3button;
    mutable xw_cnc_coord   : xw_text_3label_3value_3button;
    mutable xw_machine_coord   : xw_text_3label_3value_3button;
    mutable xw_axis        : xw_label_4but;
    mutable xw_tree        : xw_tree;
    mutable xw_prog_tree   : xw_tree;
    mutable xw_buttons     : xw_button9;
    mutable xw_log              : xw_log;
}


let xw_init () = 
    (* 
    ** The top widget 
    *)
    let display = new WX_display.t "" in
    let root = new WX_root.t display 0 in
    let top = new WX_wmtop.t root 
                    [MinWidth 800; MinHeight 100;
                     Background "black"] in

    let vbar_left = new WX_bar.v top#container 
                        [MinWidth 780;
                         IpadX 10; IpadY 10; Background "black"] in
    let vbar_right = new WX_bar.v top#container 
                        [MinWidth 150;
                         IpadX 10; IpadY 10;] in


    (*
    ** Input fields (Servers)
    *)
    let vbar_server =  new WX_bar.v vbar_left#container 
                           [ExpandX true;
                            ] in



    let hbar = new WX_bar.h vbar_server#container 
                               [ExpandX true; IpadX 10; IpadY 10] in
    let label = new WX_label.t hbar#container "CNC server" 
                               [IpadX 10; MinWidth 120] in
    let path = "/server/cnc" in
    let input = new WX_ledit.t hbar#container path 
                               [ExpandX true; IpadX 2; IpadY 2;
                                MinWidth 300;] 
        in
    let but = new WX_button.with_label  hbar#container 
                                        "INIT"
                                        [MinWidth 100]
        in
    let xw_cnc_server = {
            xw_tib_hbar = hbar;
            xw_tib_label = label;
            xw_tib_input = input;
            xw_tib_button = but;
            xw_tib_str = path;
        } in

    hbar#container_add_s [
            label#contained;
            input#contained;
            but#contained;
        ];



    let hbar = new WX_bar.h vbar_server#container 
                               [ExpandX true; IpadX 10; IpadY 10] in
    let label = new WX_label.t hbar#container "Encoder server" 
                               [IpadX 10; MinWidth 120] in
    let path = "/hosts/XYZ/ser:01" in
    let input = new WX_ledit.t hbar#container path 
                               [ExpandX true; IpadX 2; IpadY 2;
                                MinWidth 300;] 
        in
    let but = new WX_button.with_label  hbar#container 
                                        "INIT"
                                        [MinWidth 100]
        in
    let xw_enc_server = {
            xw_tib_hbar = hbar;
            xw_tib_label = label;
            xw_tib_input = input;
            xw_tib_button = but;
            xw_tib_str = path;
        } in

    hbar#container_add_s [
            label#contained;
            input#contained;
            but#contained;
        ];

    vbar_server#container_add_s [
            xw_cnc_server.xw_tib_hbar#contained;
            xw_enc_server.xw_tib_hbar#contained;
        ];



    (*
    ** User defined commands (8)
    *)

    (*
    ** Input fields (Servers)
    *)
    let vbar_comms =  new WX_bar.v vbar_left#container 
                           [ExpandX true;
                            ] in



    let xw_comms = ref [] in
    let comms = [|
            "PO X 0";
            "PO Y 0";
            "PO Z 0";
            "DEL; ABS; MO X 0; GO";
            "DEL; ABS; MO Y 0; GO";
            "DEL; ABS; MO Z 0; GO";
            "HE";
            "DEL";
        |]
        in
    for i = 1 to 8
    do
        let hbar = new WX_bar.h vbar_server#container 
                               [ExpandX true; IpadX 5; IpadY 5] in
        let label = new WX_label.t hbar#container 
                                (sprintf "P%d" i) 
                               [IpadX 10; MinWidth 40] in
        let input = new WX_ledit.t hbar#container comms.(i-1) 
                               [ExpandX true; IpadX 2; IpadY 2;
                                MinWidth 300;] 
        in
        let but = new WX_button.with_label  hbar#container 
                                        "EVAL"
                                        [MinWidth 100]
        in
        xw_comms := !xw_comms @ [{
            xw_tib_hbar = hbar;
            xw_tib_label = label;
            xw_tib_input = input;
            xw_tib_button = but;
            xw_tib_str = path;
                }];

        hbar#container_add_s [
                label#contained;
                input#contained;
                but#contained;
            ];
        vbar_comms#container_add hbar#contained;
    done;





    (*
    ** Input fields (Tool and solid aligning parameters)
    *)
    let vbar_tools = new WX_bar.v vbar_left#container 
                           [ExpandX true;
                            ] in



    let hbar = new WX_bar.h vbar_tools#container 
                               [ExpandX true; IpadX 10; IpadY 10] in
    let label = new WX_label.t hbar#container "Tool" 
                               [IpadX 10; MinWidth 80] in
    let label1 = new WX_label.t hbar#container "TN X" 
                               [IpadX 10; MinWidth 30] in
    let label2 = new WX_label.t hbar#container "TN Y" 
                               [IpadX 10; MinWidth 30] in
    let label3 = new WX_label.t hbar#container "TN Z" 
                               [IpadX 10; MinWidth 30] in
    let label4 = new WX_label.t hbar#container "TD" 
                               [IpadX 10; MinWidth 30] in
    let input1 = new WX_ledit.t hbar#container "0" 
                               [ExpandX true; IpadX 2; IpadY 2;
                                MinWidth 50;] 
        in
    let input2 = new WX_ledit.t hbar#container "0" 
                               [ExpandX true; IpadX 2; IpadY 2;
                                MinWidth 50;] 
        in
    let input3 = new WX_ledit.t hbar#container "0" 
                               [ExpandX true; IpadX 2; IpadY 2;
                                MinWidth 50;] 
        in
    let input4 = new WX_ledit.t hbar#container "0" 
                               [ExpandX true; IpadX 2; IpadY 2;
                                MinWidth 50;] 
        in
    let but = new WX_button.with_label  hbar#container 
                                        "SET"
                                        [MinWidth 100]
        in
    let xw_tool = {
        xw_t4lib_hbar   = hbar;
        xw_t4lib_label  = label;
        xw_t4lib_label1 = label1;
        xw_t4lib_input1 = input1;
        xw_t4lib_str1   = "0"; 
        xw_t4lib_label2 = label2;
        xw_t4lib_input2 = input2;
        xw_t4lib_str2   = "0";
        xw_t4lib_label3 = label3;
        xw_t4lib_input3 = input3;
        xw_t4lib_str3   = "0";
        xw_t4lib_label4 = label4;
        xw_t4lib_input4 = input4;
        xw_t4lib_str4   = "0";
        xw_t4lib_button = but;
        } in


    hbar#container_add_s [
            label#contained;
            label1#contained;
            input1#contained;
            label2#contained;
            input2#contained;
            label3#contained;
            input3#contained;
            label4#contained;
            input4#contained;
            but#contained;
        ];


    vbar_tools#container_add hbar#contained;


    let hbar = new WX_bar.h vbar_tools#container 
                               [ExpandX true; IpadX 10; IpadY 10] in
    let label = new WX_label.t hbar#container "Solid" 
                               [IpadX 10; MinWidth 80] in
    let label1 = new WX_label.t hbar#container "SN X" 
                               [IpadX 10; MinWidth 30] in
    let label2 = new WX_label.t hbar#container "SN Y" 
                               [IpadX 10; MinWidth 30] in
    let label3 = new WX_label.t hbar#container "SN Z" 
                               [IpadX 10; MinWidth 30] in
    let input1 = new WX_ledit.t hbar#container "0" 
                               [ExpandX true; IpadX 2; IpadY 2;
                                MinWidth 50;] 
        in
    let input2 = new WX_ledit.t hbar#container "0" 
                               [ExpandX true; IpadX 2; IpadY 2;
                                MinWidth 50;] 
        in
    let input3 = new WX_ledit.t hbar#container "0" 
                               [ExpandX true; IpadX 2; IpadY 2;
                                MinWidth 50;] 
        in
    let but1 = new WX_button.with_label  hbar#container 
                                        "SET"
                                        [MinWidth 20]
        in
    let but2 = new WX_button.with_label  hbar#container 
                                        "SET"
                                        [MinWidth 20]
        in
    let but3 = new WX_button.with_label  hbar#container 
                                        "SET"
                                        [MinWidth 20]
        in

    let xw_solid = {
        xw_t3lib_hbar   = hbar;
        xw_t3lib_label  = label;
        xw_t3lib_label1 = label1;
        xw_t3lib_input1 = input1;
        xw_t3lib_button1 = but1;
        xw_t3lib_str1   = "0"; 
        xw_t3lib_label2 = label2;
        xw_t3lib_input2 = input2;
        xw_t3lib_button2 = but2;
        xw_t3lib_str2   = "0";
        xw_t3lib_label3 = label3;
        xw_t3lib_input3 = input3;
        xw_t3lib_button3 = but3;
        xw_t3lib_str3   = "0";
        } in


    hbar#container_add_s [
            label#contained;
            label1#contained;
            input1#contained;
            but1#contained;
            label2#contained;
            input2#contained;
            but2#contained;
            label3#contained;
            input3#contained;
            but3#contained;
        ];


    vbar_tools#container_add hbar#contained;



    (*
    ** CNC and machine coordinates
    *)

    let vbar_coords = new WX_bar.v top#container 
                               [ExpandX true; IpadX 5; IpadY 5;
                                Relief ReliefSunken] in

    let vbar = new WX_bar.v vbar_coords#container 
                               [ExpandX true; IpadX 5; IpadY 5] in
    let label = new WX_label.t vbar#container "CNC Coordinates" 
                               [IpadX 5; MinWidth 80] in
    let hbar1 = new WX_bar.h vbar#container [] in
    let label1 = new WX_label.t hbar1#container "X" 
                               [IpadX 5; MinWidth 30] in
    let value1 = new WX_label.t hbar1#container "0"
                               [IpadX 5; MinWidth 80] in
    let reset1 = new WX_button.with_label top#container
                        "0" [MinWidth 10] in


    hbar1#container_add_s [
        label1#contained;
        value1#contained;
        reset1#contained;
        ];

    let hbar2 = new WX_bar.h vbar#container [] in
    let label2 = new WX_label.t hbar2#container "Y" 
                               [IpadX 5; MinWidth 30] in
    let value2 = new WX_label.t hbar2#container "0"
                               [IpadX 5; MinWidth 80] in
    let reset2 = new WX_button.with_label top#container
                        "0" [MinWidth 10] in


    hbar2#container_add_s [
        label2#contained;
        value2#contained;
        reset2#contained;
        ];

    let hbar3 = new WX_bar.h vbar#container [] in
    let label3 = new WX_label.t hbar3#container "Z" 
                               [IpadX 5; MinWidth 30] in
    let value3 = new WX_label.t hbar3#container "0"
                               [IpadX 5; MinWidth 80] in
    let reset3 = new WX_button.with_label top#container
                        "0" [MinWidth 10] in


    hbar3#container_add_s [
        label3#contained;
        value3#contained;
        reset3#contained;
        ];

    vbar#container_add_s [
        label#contained;
        hbar1#contained;
        hbar2#contained;
        hbar3#contained;
        ];

    vbar_coords#container_add vbar#contained;


    let xw_cnc_coords = {
            xw_t3lvb_vbar = vbar;
            xw_t3lvb_text = label;
            xw_t3lvb_hbar1 = hbar1;
            xw_t3lvb_label1 = label1;
            xw_t3lvb_value1 = value1;
            xw_t3lvb_but1 = reset1;
            xw_t3lvb_str1 = "0";        
            xw_t3lvb_hbar2 = hbar2;
            xw_t3lvb_label2 = label2;
            xw_t3lvb_value2 = value2;        
            xw_t3lvb_but2 = reset2;
            xw_t3lvb_str2 = "0";
            xw_t3lvb_hbar3 = hbar3;
            xw_t3lvb_label3 = label3;
            xw_t3lvb_value3 = value3;        
            xw_t3lvb_but3 = reset3;
            xw_t3lvb_str3 = "0";
        } in


    let vbar = new WX_bar.v vbar_coords#container 
                               [ExpandX true; IpadX 5; IpadY 5] in
    let label = new WX_label.t vbar#container "Machine Coordinates" 
                               [IpadX 5; MinWidth 80] in
    let hbar1 = new WX_bar.h vbar#container [] in
    let label1 = new WX_label.t hbar1#container "X" 
                               [IpadX 5; MinWidth 30] in
    let value1 = new WX_label.t hbar1#container "0"
                               [IpadX 5; MinWidth 80] in

    let reset1 = new WX_button.with_label top#container
                        "0" [MinWidth 10] in

    hbar1#container_add_s [
        label1#contained;
        value1#contained;
        reset1#contained;
        ];

    let hbar2 = new WX_bar.h vbar#container [] in
    let label2 = new WX_label.t hbar2#container "Y" 
                               [IpadX 5; MinWidth 30] in
    let value2 = new WX_label.t hbar2#container "0"
                               [IpadX 5; MinWidth 80] in

    let reset2 = new WX_button.with_label top#container
                        "0" [MinWidth 10] in
    hbar2#container_add_s [
        label2#contained;
        value2#contained;
        reset2#contained;
        ];

    let hbar3 = new WX_bar.h vbar#container [] in
    let label3 = new WX_label.t hbar3#container "Z" 
                               [IpadX 5; MinWidth 30] in
    let value3 = new WX_label.t hbar3#container "0"
                               [IpadX 5; MinWidth 80] in

    let reset3 = new WX_button.with_label top#container
                        "0" [MinWidth 10] in

    hbar3#container_add_s [
        label3#contained;
        value3#contained;
        reset3#contained;
        ];


    vbar#container_add_s [
        label#contained;
        hbar1#contained;
        hbar2#contained;
        hbar3#contained;
        ];

    
    vbar_coords#container_add vbar#contained;

    let xw_machine_coords = {
            xw_t3lvb_vbar = vbar;
            xw_t3lvb_text = label;
            xw_t3lvb_hbar1 = hbar1;
            xw_t3lvb_label1 = label1;
            xw_t3lvb_value1 = value1;
            xw_t3lvb_but1 = reset1;
            xw_t3lvb_str1 = "0";        
            xw_t3lvb_hbar2 = hbar2;
            xw_t3lvb_label2 = label2;
            xw_t3lvb_value2 = value2;        
            xw_t3lvb_but2 = reset2;
            xw_t3lvb_str2 = "0";
            xw_t3lvb_hbar3 = hbar3;
            xw_t3lvb_label3 = label3;
            xw_t3lvb_value3 = value3;        
            xw_t3lvb_but3 = reset3;
            xw_t3lvb_str3 = "0";
        } in





    (*
    ** Motor Axis button matrix (bistable buttons!)
    *)

    let vbar_axis = new WX_bar.v top#container 
                   [ExpandX true; IpadX 5; IpadY 5] in
    let label = new WX_label.t vbar_axis#container "Motor Axis control"
                               [IpadX 5; MinWidth 80] in
    let hbar = new WX_bar.h vbar_axis#container
                   [ExpandX true; IpadX 5; IpadY 5] in
    let but1 = new WX_button.with_label hbar#container
                        "X" [MinWidth 20] in
    let but2 = new WX_button.with_label hbar#container
                        "Y" [MinWidth 20] in
    let but3 = new WX_button.with_label hbar#container
                        "Z" [MinWidth 20] in
    let but4 = new WX_button.with_label hbar#container
                        "*" [MinWidth 20] in
    hbar#container_add_s [
            but1#contained;
            but2#contained;
            but3#contained;
            but4#contained;
        ];
    vbar_axis#container_add_s [
            label#contained;
            hbar#contained;
        ];

    let axis_buts =
        {
            xw_l4b_vbar = vbar;
            xw_l4b_label = label;
            xw_l4b_hbar = hbar;
            xw_l4b_but1 = but1;
            xw_l4b_but2 = but2;
            xw_l4b_but3 = but3;
            xw_l4b_but4 = but4;
            xw_l4b_state1 = false;
            xw_l4b_state2 = false;
            xw_l4b_state3 = false;
            xw_l4b_state4 = false;
        } in

    (*
    ** File tree
    *)
    let vbar_tree = new WX_bar.v top#container 
                    [IpadX 5; IpadY 5] in
    let vbar = new WX_bar.v vbar_tree#container 
                    [Relief ReliefSunken;] in
    let hbar = new WX_bar.h vbar#container 
                    [Relief ReliefSunken] in
    let adj_h = new WX_adjust.t () in
    let adj_v = new WX_adjust.t () in
    let view = new WX_viewport.t hbar#container 
                   adj_h adj_v 
                   [MinHeight 220;MaxHeight 220; MinWidth 300; MaxWidth 300] in
    let scroll_v = new WX_scrollbar.v hbar#container 
                                      adj_v [] in
    let scroll_h = new WX_scrollbar.h hbar#container 
                                      adj_h [] in

    let tree = new WX_tree.t view#container [
                        MinWidth 250; MaxWidth 250;
                    ] in

    view#container_add tree#contained;

    hbar#container_add_s [
            view#contained;
            scroll_v#contained
        ];

    vbar#container_add_s [
            hbar#contained;
            scroll_h#contained
        ];

    vbar_tree#container_add vbar#contained;

    let file_tree = {
        xw_t_hbar = hbar;
        xw_t_vbar = vbar;
        xw_t_view = view;
        xw_t_adj_h = adj_h;
        xw_t_adj_v = adj_v;
        xw_t_scroll_h = scroll_h;
        xw_t_scroll_v = scroll_v;
        xw_t_tree = tree;
        } in

    (*
    ** Program tree
    *)
    let vbar_prog_tree = new WX_bar.v top#container 
                    [IpadX 5; IpadY 5] in
    let vbar = new WX_bar.v vbar_prog_tree#container 
                    [Relief ReliefSunken;] in
    let hbar = new WX_bar.h vbar#container 
                    [Relief ReliefSunken] in
    let adj_h = new WX_adjust.t () in
    let adj_v = new WX_adjust.t () in
    let view = new WX_viewport.t hbar#container 
                   adj_h adj_v 
                   [MinHeight 220;MaxHeight 220; MinWidth 300; MaxWidth 300] in
    let scroll_v = new WX_scrollbar.v hbar#container 
                                      adj_v [] in
    let scroll_h = new WX_scrollbar.h hbar#container 
                                      adj_h [] in

    let tree = new WX_tree.t view#container [
                        MinWidth 250; MaxWidth 250;
                    ] in

    view#container_add tree#contained;

    hbar#container_add_s [
            view#contained;
            scroll_v#contained
        ];

    vbar#container_add_s [
            hbar#contained;
            scroll_h#contained
        ];

    vbar_prog_tree#container_add vbar#contained;

    let prog_tree = {
        xw_t_hbar = hbar;
        xw_t_vbar = vbar;
        xw_t_view = view;
        xw_t_adj_h = adj_h;
        xw_t_adj_v = adj_v;
        xw_t_scroll_h = scroll_h;
        xw_t_scroll_v = scroll_v;
        xw_t_tree = tree;
        } in


    (*
    ** BSSLAB Logo!
    *)
    let logo_hbar = new WX_bar.h vbar_right#container
                    [IpadX 10; IpadY 20] in
  
    let logo = new WX_table.text logo_hbar#container
                    2 1 0 0 [||] [Background "black"] in

    logo_hbar#container_add logo#contained;
    logo#set_text 0 0 "BSS";
    logo#set_text 1 0 "LAB";
    logo#set_style_cell 0 0 [Background "blue";Foreground "white";
                            BorderWidth 0];
    logo#set_style_text 0 0 [Background "blue";
                             Foreground "white";];
    logo#set_style_cell 1 0 [Foreground "blue";
                        BorderWidth 0];
    logo#set_style_text 1 0 [Foreground "blue";
                        BorderWidth 0];

    logo#set_font 0 0 Fonts.Helvetica.Bold.s24;
    logo#set_font 1 0 Fonts.Helvetica.Bold.s24;

    (*
    ** Buttons
    *)
    
    let hbar_buts = new WX_bar.h top#container 
                      [IpadX 5; IpadY 5] in
    let vbar = new WX_bar.v hbar_buts#container
                   [IpadX 5; IpadY 5] in
    let but1 = new WX_button.with_label vbar#container
                        "LOAD" [MinWidth 50] in
    let but2 = new WX_button.with_label vbar#container
                        "RUN" [MinWidth 50] in
    let but3 = new WX_button.with_label vbar#container
                        "STOP" [MinWidth 50] in
    let but4 = new WX_button.with_label vbar#container
                        "CONT" [MinWidth 50] in
    let but5 = new WX_button.with_label vbar#container
                        "SEL +" [MinWidth 50] in
    let but6 = new WX_button.with_label vbar#container
                        "SEL -" [MinWidth 50] in
    let but7 = new WX_button.with_label vbar#container
                        "SHOW" [MinWidth 50] in
    let but8 = new WX_button.with_label vbar#container
                        "HELP" [MinWidth 50] in
    let quit_but = new WX_button.with_label vbar#container
                        "QUIT" [MinWidth 50] in

    vbar#container_add_s [
            but1#contained;
            but2#contained;
            but3#contained;
            but4#contained;
            but5#contained;
            but6#contained;
            but7#contained;
            but8#contained;
            quit_but#contained;
        ];
    hbar_buts#container_add vbar#contained;

    let xw_buts = {
            xw_b9_hbar = hbar_buts;
            xw_b9_vbar = vbar;
            xw_b9_but1 = but1;
            xw_b9_but2 = but2;
            xw_b9_but3 = but3;
            xw_b9_but4 = but4;
            xw_b9_but5 = but5;
            xw_b9_but6 = but6;
            xw_b9_but7 = but7;
            xw_b9_but8 = but8;
            xw_b9_but9 = quit_but;
        } in

    (*
    ** Log window
    *)
    let hbar_log = new WX_bar.h top#container 
                [IpadX 10;IpadY 10] in 

    let adx = new WX_adjust.t () in
    let ady = new WX_adjust.t () in
    let viewport = new WX_viewport.t hbar_log#container adx ady 
        [MinHeight 70; MaxHeight 70; MinWidth 50;ExpandX true; 
        Background "white"; Foreground "blue";] in
    let vscroll = new WX_scrollbar.v hbar_log#container ady 
                        [] in

    let log_msg = "Starting ...\n"  in
    let log_text = new WX_text.of_string viewport#container 
        log_msg
        [BorderWidth 0;Background "white";Foreground "blue"] in

    viewport#container_add (log_text#contained);
    hbar_log#container_add_s [
            viewport#contained; 
            vscroll#contained;
        ];
    let xw_log = {
            xw_log_hbar = hbar_log;
            xw_log_adx  = adx;
            xw_log_ady  = ady;
            xw_log_view = viewport;
            xw_log_scroll = vscroll;
            xw_log_text = log_text;
            xw_log_str = log_msg;
        } in

    (*
    ** Main structures
    *)

    let hbar_main = new WX_bar.h top#container
                               [ExpandX true; IpadX 5; IpadY 5;
                                Background "black"] in

    let hbar_bottom = new WX_bar.h vbar_left#container
                               [ExpandX true; IpadX 5; IpadY 5;
                                ] in
    hbar_bottom#container_add_s [
            vbar_tree#contained;
            hbar_buts#contained;
            vbar_prog_tree#contained;
        ];

    vbar_left#container_add_s [ 
            vbar_server#contained;
            vbar_comms#contained;
            vbar_tools#contained; 
            hbar_bottom#contained;
            hbar_log#contained;
        ];

    vbar_right#container_add_s [ 
            vbar_coords#contained; 
            vbar_axis#contained;
            logo_hbar#contained;
        ];

    hbar_main#container_add_s [
            vbar_left#contained;
            vbar_right#contained;
        ];
    let vbar_top = new WX_bar.v top#container
                               [ExpandX true; IpadX 5; IpadY 5;
                                Background "black"] in
    
    vbar_top#container_add_s [
            hbar_main#contained;
        ];

    top#container_add vbar_top#contained;

    {
        xw_dpy = display;
        xw_root = root;
        xw_top = top;
        xw_quit = quit_but;
        xw_vbar_server = vbar_server;
        xw_cnc_server  = xw_cnc_server; 
        xw_enc_server  = xw_enc_server;
        xw_comms       = !xw_comms;
        xw_tool        = xw_tool;
        xw_solid       = xw_solid;
        xw_cnc_coord    = xw_cnc_coords;
        xw_machine_coord = xw_machine_coords;
        xw_axis = axis_buts;
        xw_tree = file_tree;
        xw_prog_tree = prog_tree;
        xw_buttons = xw_buts;
        xw_log = xw_log;
    }

let xw_main = xw_init ()

(*
**      ==================================
**      OOOO   OOOO OOOO  O      O   OOOO
**      O   O  O    O     O     O O  O   O
**      O   O  O    O     O     O O  O   O
**      OOOO   OOOO OOOO  O     OOO  OOOO
**      O   O     O    O  O    O   O O   O
**      O   O     O    O  O    O   O O   O
**      OOOO   OOOO OOOO  OOOO O   O OOOO
**      ================================== 
**      BSSLAB, Dr. Stefan Bosse sci@bsslab.de
**
**    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF: 
**    Free Software Foundation-Europe, GNU GPL License, Version 2
**
**    $AUTHORS:     Stefan Bisse
**    $INITIAL:     (C) BSSLAB 2003-2005
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.04
**
**    $INFO:
**
** XAFS widgets.
**
**
**    $ENDOFINFO
**
*)




open Xtypes
open WX_types
open WX_tree
open Unix


open Amoeba
open Ar
open Dir
open Name
open Stderr
open Stdcom
open Cap_env
open Thread

open Xafs_file

let _ =
    Db.set_level 1

let _ = get_env_cap "ROOT"

let dns_root = ref nilcap 

type file_system = UNIX | AFS

let display = new WX_display.t ""
let root = new WX_root.t display 0

(* The top widget *)
let top = new WX_wmtop.t root [MinWidth 100; MinHeight 100; MaxHeight 600]

(* Vertical and horizontal container widgets *)
let vbar1 = new WX_bar.v top#container [Background "black"]
let hbar_comm1 = new WX_bar.h vbar1#container [Background "black"] 
let hbar_comm2 = new WX_bar.h vbar1#container [] 
let hbar_comm3 = new WX_bar.h vbar1#container [] 
let hbar2 = new WX_bar.h vbar1#container [IpadX 10; IpadY 10;
                        Background "black"] 
let hbar3 = new WX_bar.h vbar1#container 
                    [IpadX 10; IpadY 10;
                    MinWidth 600; MaxWidth 600;
                    Background "black"] 
let hbar4 = new WX_bar.h vbar1#container 
                    [Background "white";MinWidth 600; MaxWidth 600;
                    IpadX 10; IpadY 10;
                    Background "black"] 

(*
** File trees
*)
let hbar_trees = new WX_bar.h vbar1#container 
                    [IpadX 10; IpadY 10;
                     MinWidth 600; MaxWidth 600;]
let hbar_left = new WX_bar.h hbar_trees#container 
                    [Relief ReliefSunken] 
let hbar_right = new WX_bar.h hbar_trees#container 
                    [Relief ReliefSunken] 
let vbar_left = new WX_bar.v hbar_trees#container 
                    [Relief ReliefSunken] 
let vbar_right = new WX_bar.v hbar_trees#container 
                    [Relief ReliefSunken] 

(*
** Copy button table
*)
let cop_rows = 4
let cop_cols = 1

let cops = [|
    [|">"|];
    [|"O"|];
    [|"D"|];
    [|"<"|];
    
|]

let cop_tab =  new WX_table.button vbar1#container 
                cop_cols cop_rows 20 20 cops 
                [IpadX 5; IpadY 5] 


(*
** The buttons 
*)

let but_rows = 3
let but_cols = 4

let buts = [|
    [|"Exit";"Clear Log";"";"Verbose"|];
    [|"Delete File";"Delete Dir.";"Delete Obj.";"Del. & Destroy"|];
    [|"Copy";"Paste";"Std_info";"Std_status"|];
|]

let but_tab =  new WX_table.button vbar1#container 
                but_cols but_rows 100 20 buts 
                [Background "black";IpadX 10; IpadY 10] 


(*
** Text field
*)

let adx = new WX_adjust.t ()
let ady = new WX_adjust.t ()
let viewport = new WX_viewport.t hbar3#container adx ady 
    [MinHeight 100; MaxHeight 100; MinWidth 50;ExpandX true; 
    Background "white"; Foreground "blue"]
let vscroll = new WX_scrollbar.v hbar3#container ady []

let log_msg = ref "Starting ...\n" 
let log_text = new WX_text.of_string viewport#container 
    (!log_msg)
    [BorderWidth 0;Background "white";Foreground "blue"]
let do_text = new WX_text.of_string viewport#container 
    "Ready."
    [ExpandX true; 
     BorderWidth 0;Background "white";Foreground "red"]


(*
** Input fields
*)

let cur_path = ref ""

let hbar_path = new WX_bar.h vbar1#container [IpadX 10; IpadY 10;
                        Background "black"] 

let input_path = new WX_ledit.t vbar1#container !cur_path 
                        [ExpandX true; IpadX 4; IpadY 4] 

let make_newdir = new WX_button.with_label  vbar1#container 
                                             "Make new dir"
                                             [MinWidth 100]

let cur_sel = ref ""

let hbar_sel = new WX_bar.h vbar1#container [IpadX 10; IpadY 10;
                        Background "black"] 

let input_sel = new WX_ledit.t vbar1#container !cur_sel
                        [ExpandX true; IpadX 4; IpadY 4] 

let make_rename = new WX_button.with_label  vbar1#container 
                                             "Rename"
                                             [MinWidth 100]


(* 
** Because the file tree can be larger than the widget, we need
** a scrollbar together with a viewport widget controlled by 
** adjust widgets.
*)

let adx_left = new WX_adjust.t ()
let ady_left = new WX_adjust.t ()
let adx_right = new WX_adjust.t ()
let ady_right = new WX_adjust.t ()

let viewport_left = new WX_viewport.t hbar_left#container 
                                      adx_left ady_left 
                                      [MinHeight 300;MaxHeight 300]
let scrollbar_left_y = new WX_scrollbar.v hbar_left#container 
                                      ady_left []
let scrollbar_left_x = new WX_scrollbar.h hbar_left#container 
                                      adx_left []

let viewport_right = new WX_viewport.t hbar_right#container 
                                      adx_right ady_right 
                                      [MinHeight 300;MaxHeight 300]
let scrollbar_right_y = new WX_scrollbar.v hbar_right#container 
                                      ady_right []
let scrollbar_right_x = new WX_scrollbar.h hbar_right#container 
                                      adx_right []


(* and the bsslab logo displayed with a table ... *)
let logo_hbar = new WX_bar.h vbar1#container
                    [IpadX 10; IpadY 20; Background "black"]

let logo = new WX_table.text logo_hbar#container
                2 1 0 0 [||] [Background "black"]


let doing msg = 
        do_text#set_lines (msg);
        do_text#update

let print msg =
      log_msg := !log_msg ^ msg;
      log_text#set_lines (!log_msg);
      log_text#update;
      ady#set_pos 1 1

(*
** main entry directory tree
*)

let tree_left = new WX_tree.t viewport_left#container [
                        MinWidth 250; MaxWidth 250;
                    ]
let tree_right = new WX_tree.t viewport_right#container [
                        MinWidth 250; MaxWidth 250;
                    ]


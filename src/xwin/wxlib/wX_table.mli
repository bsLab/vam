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
**    $MODIFIEDBY:  BSSLAB
**    $AUTHORS:     Fabrice Le Fessant, Stefan Bosse
**    $INITIAL:     (C) 1999 INRIA
**    $CREATED:     Part II: 2003.11.28
**    $VERSION:     1.06
**
**    $INFO:
**
**  class t:
**
**  Generic table widget.
**  
**  NOT COMPLETELY IMPLEMENTED YET.
**  
**  The WX_table.t widget will allow to set widgets positions in a table.
**  
**  Clearly, for all containers, there is a need to separate size hints in two 
**  different kinds:
**  those which are user defined, and those which are computed from
**  contained widgets.
**
**
**  class text:
**
**  Text Table Widget.
**
**  Text strings can be placed within a table with # rows and # columns.
**  The height and width of each cell (in pixles) must be specified.
**  Value 0 forces auto adjusting of cell widths and heights.
**  The text can be set individual with the set_text method or with
**  the set_table expecting row array of col array string data or 
**  inital with the new method (but can also be an empty array here).
**
**  Example:
**
**  let rows = 3 
**  let cols = 3
**
**  let vals = [|
**      [|"Nummer";"Datum";"Name"|];
**      [|"0001";"23.10.2003-042171717";"Target"|];
**      [|"0002";"23.11.2003";"Task"|];|]
**  let tab = new wX_table.text vbar#container rows cols 150 50 vals [] in
**  
**  tab#set_table vals;
**  tab#set_text 1 1 "Changed";
**
**  vbar#container_add_s  [ 
**                tab#contained; ...
**
**  Attributes of individual cells can be set with the set_style_cell, and
**  the below text widget with the set_style_text method.
**
**
**  class button:
**
**  Table of button widgets. Here, the text can be specified at
**  creation time, too, or later with the set_string and
**  set_font, set_style methods. 
**  After, the individual button actions can be set with
**  the set#action method.
**  Button groups (with only one button activated each time) can be 
**  created with the set_group method. The [(col,row)...] list and the initial
**  (col,row) activated button must be specified. The set-group method
**  must be called before the set_action method!
**
**
**
**    $ENDOFINFO
**
*)


(*
** Generic table class
*)
class t :
  WX_types.container ->
  WX_types.base_attributes list ->
  int ->
  int ->
  bool ->
  object
    
    inherit WX_object.t
    
    val hints : WX_types.szhints array
    val wobs : (WX_types.contained * int * int * int * int) array array
    method container_add :
      WX_types.contained -> int -> int -> int -> int -> unit
  end

(*
** Text table class
*)

class text :
  WX_types.container ->
  int ->    (* # cols *)
  int ->    (* # rows *)
  int ->    (* one cell width *)
  int ->    (* one cell hight *)
  string array array ->     (* initial text string array x.(row).(col) *)
  WX_types.base_attributes list ->
  object
    
    inherit WX_object.t

    (*
    ** Set the style attributes of the text widget or the cell widget
    *)
    method set_style_text : int -> int -> WX_types.base_attributes list -> unit
    method set_style_cell : int -> int -> WX_types.base_attributes list -> unit
    (*
    ** Set the style for all cell and text widgets
    *)
    method set_style_all : WX_types.base_attributes list -> unit
    (*
    ** Initialize the text table
    *)
    method set_table : WX_text.SimpleTree.text array array -> unit
    (*
    ** Set one text cell
    *)
    method set_text : int -> int -> WX_text.SimpleTree.text -> unit
    (*
    ** Change the text font
    *)
    method set_font : int -> int -> string -> unit


    val tab_obj : t
    val tabo : WX_bar.h array array
    val tabs : WX_text.of_string array array
  end

class button :
  WX_types.container ->
  int ->                            (* # cols *)
  int ->                            (* # rows *)
  int ->                            (* one cell width *)
  int ->                            (* one cell hight *)
  string array array ->             (* button names array x.(row).(col) *)
  WX_types.base_attributes list ->
  object
    
    inherit WX_object.t
    (*
    ** Set the style attributes of the button (label) widget or the cell widget
    *)
    method set_style_cell : int -> int -> WX_types.base_attributes list -> unit
    method set_style_but : int -> int -> WX_types.base_attributes list -> unit
    (*
    ** Set the style for all cell and button widgets
    *)
    method set_style_all : WX_types.base_attributes list -> unit
    (*
    ** Specify the action related with the button
    *)
    method set_action : int -> int -> (unit -> unit) -> unit
    (*
    ** Change the label button font.
    *)
    method set_font : int -> int -> string -> unit
    (*
    ** Set the label text
    *)
    method set_text : int -> int -> string -> unit

    (*
    ** Define a button group. Only one button of the group
    ** can be activated each time.
    *)
    method set_group : (int*int) list -> (int*int) -> unit


    (*
    ** Mono- (wait_for_release) or bistable button (one action for
    ** Button press- and release)!
    ** Default: f=false -> Monoswitch
    *)
    method set_bistable : int -> int -> bool -> unit

    (*
    ** Set the state of the button (activated=true)
    *)

    method set_state : int -> int -> bool -> unit

    val tab_obj : t
    val tabo : WX_bar.h array array
    val tabb : WX_button.with_label array array
    val taba : (unit -> unit) array array
    val tabs : bool array array
    val tabm : bool array array

    val groups : ((int*int) array) list 
  end


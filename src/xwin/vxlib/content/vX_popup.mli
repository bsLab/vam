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
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2005-2006 BSSLAB
**    $CREATED:     
**    $VERSION:     1.01
**
**    $INFO:
**
** Popup widgets.
**
**    $ENDOFINFO
**
*)

type kind = Sticky | Window
class orig :
  kind ->
  VX_root.t ->
  VX_types.container ->
  string ->
  VX_types.widget_attributes list ->
  object
    inherit VX_object.t

    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method container_add : VX_types.contained -> unit
    method container_set : VX_types.contained -> unit
    method get_state : bool
    method popdown : unit
    method popup : int -> int -> unit
    method print : VX_types.ps -> int -> int -> unit
    method refresh : unit
    method set_action : (unit -> unit) -> unit
    method set_state : bool -> unit
    method size_request : VX_types.szhints
    method update : unit
    val mutable action_down : unit -> unit
    val mutable action_up : unit -> unit
    val mutable bold_font : VX_types.font
    val mutable but_active : bool
    val but_box : VX_types.frame
    val mutable but_size : int
    val mutable is_select : bool
    val mutable max_select_width : int
    val mutable normal_font : VX_types.font
    val mutable popup_wob : VX_types.contained option
    val mutable realization1 : VX_top.t option
    val mutable realization2 : VX_wmtop.t option
    val root : VX_root.t
    val mutable select_str : string
    val mutable symbol_font : VX_types.font
    val mutable text_str : string
    val mutable win_active : bool
  end

(*
** Generic popup widget. Any child widget can be displayed.
**
**  +--------------+
**  | Label |  But |
**  +--------------+
*)

class t :
  VX_root.t ->
  VX_types.container ->
  string ->
  VX_types.widget_attributes list ->
  object
    inherit orig
  end


(*
** Modified version: a select list widget is displayed on popup 
** request, and the selection is permanently displayed.
**
**  +----------------------+
**  | Label | But | Choice |
**  +----------------------+
**
** The 'choices' array contains text strings of all possible
** choices. The initial number specifies the intial selected
** item.
*)
  
  
class select_orig :
  kind ->
  VX_root.t ->
  VX_types.container ->
  string ->
  string array ->
  int ->
  VX_types.widget_attributes list ->
  object
    inherit orig
  end

class select :
  VX_root.t ->
  VX_types.container ->
  string ->
  string array ->
  int ->
  VX_types.widget_attributes list ->
  object
    inherit select_orig
  end

class selectW :
  VX_root.t ->
  VX_types.container ->
  string ->
  string array ->
  int ->
  VX_types.widget_attributes list ->
  object
    inherit select_orig
  end

val path_str : Myenv.path_arg -> string
val base : string -> string

class file_select_orig :
  kind ->
  VX_root.t ->
  VX_types.container ->
  string ->
  Myenv.path_arg ->
  VX_types.widget_attributes list ->
  object
    inherit orig

    method get_path : string
    method close_dir : string -> unit
    method open_dir : string -> unit
    val mutable path : string
    val mutable tree_wob : VX_tree.t option
  end

class file_select :
  VX_root.t ->
  VX_types.container ->
  string ->
  Myenv.path_arg ->
  VX_types.widget_attributes list ->
  object
    inherit file_select_orig
  end

class file_selectW :
  VX_root.t ->
  VX_types.container ->
  string ->
  Myenv.path_arg ->
  VX_types.widget_attributes list ->
  object
    inherit file_select_orig
  end

class file_select_edit_orig :
  kind ->
  VX_root.t ->
  VX_types.container ->
  string ->
  Myenv.path_arg ->
  VX_types.widget_attributes list ->
  object
    inherit orig
    method get_path : string
    method close_dir : string -> unit
    method open_dir : string -> unit
    val mutable path : string
    val mutable tree_wob : VX_tree.t option
  end

class file_select_edit :
  VX_root.t ->
  VX_types.container ->
  string ->
  Myenv.path_arg ->
  VX_types.widget_attributes list ->
  object
    inherit file_select_edit_orig
  end
class file_select_editW :
  VX_root.t ->
  VX_types.container ->
  string ->
  Myenv.path_arg ->
  VX_types.widget_attributes list ->
  object
    inherit file_select_edit_orig
  end

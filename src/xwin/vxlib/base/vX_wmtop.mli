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
**    $AUTHORS:     Fabrice Le Fessant, 
**                  Stefan Bosse
**    $INITIAL:     Derived from wXlib / INRIA, 2005 BSSLAB
**    $CREATED:     12.5.2005
**    $VERSION:     1.02
**
**    $INFO:
**
**  VXlib window manager interface.
**
**    $ENDOFINFO
**
*)
val hostname : string

class orig :
  VX_root.t ->
  VX_types.widget_attributes list ->
  object
    
    inherit VX_top.orig
    
    val mutable wm_class : string
    val mutable wm_class_app : string
    val mutable wm_command : string list
    val wm_hints : Xtypes.wm_hints
    val mutable wm_icon_name : string
    val mutable wm_name : string
    val wm_size_hints : Xtypes.wm_size_hints
    val mutable wm_transient_for : Xtypes.window

    method iconify : unit
    method deiconify : unit
    method setWM_CLASS : string -> string -> unit
    method setWM_ICON_NAME : string -> unit
    method setWM_NAME : string -> unit
    method setWM_TRANSIENT_FOR : VX_types.container -> unit
    method setWM_SIZE_HINTS : Xtypes.wm_size_hints -> unit
    method withdraw : unit
  end


class t :
  VX_root.t ->
  VX_types.widget_attributes list ->
  object
    
    inherit VX_top.t

    method iconify : unit
    method deiconify : unit
    method setWM_CLASS : string -> string -> unit
    method setWM_ICON_NAME : string -> unit
    method setWM_NAME : string -> unit
    method setWM_TRANSIENT_FOR : VX_types.container -> unit
    method setWM_SIZE_HINTS : Xtypes.wm_size_hints -> unit
    method withdraw : unit
  end



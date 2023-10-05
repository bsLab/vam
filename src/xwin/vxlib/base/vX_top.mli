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
**  VXlib top level widget.
**
**    $ENDOFINFO
**
*)

class orig :
  VX_root.t ->
  (int * int) option ->
  VX_types.widget_attributes list ->
  object
    
    inherit VX_object.t
    
    val mutable widget : VX_types.contained option
    method change_position : int -> int -> unit
    method container_add : VX_types.contained -> unit
    method grab_pointer : unit
    method top : VX_types.container
  end

class t :
  VX_root.t ->
  (int * int) option ->
  VX_types.widget_attributes list ->
  object
    
    inherit VX_object.t
    
    method change_position : int -> int -> unit
    method container_add : VX_types.contained -> unit
    method grab_pointer : unit
    method top : VX_types.container
  end

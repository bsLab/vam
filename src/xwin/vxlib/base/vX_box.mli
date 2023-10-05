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
**    $CREATED:     11.5.2005
**    $VERSION:     1.07
**
**    $INFO:
**
**  VXlib box container management. All widgets are arranged in
**  box container of either horizontal or vertial alignment. Both types
**  of box containers can be mixed.
**
**    $ENDOFINFO
**
*)


(*
** Common box class
*)

class orig :
  VX_types.box_desc ->
  VX_types.container ->
  VX_types.widget_attributes list ->
  object
    
    inherit VX_object.t
    
    (*
    ** Box alignment
    *)
    val mutable sens : VX_types.box_desc
    (*
    ** All the widget objects packed in the box container.
    *)
    val mutable wobs : VX_types.contained array
    
    method clear_items : unit
    (*
    ** Add a widget object to the box container.
    *)
    method container_add : VX_types.contained -> unit
    (*
    ** exchange container specified with index number 
    *)
    method container_exchange : int -> VX_types.contained -> unit
    (*
    ** Add a list of containers
    *)
    method container_add_s : VX_types.contained list -> unit
    (*
    ** Add a container to the end of the wob array
    *)
    method container_insert : int -> VX_types.contained -> unit
    (*
    ** Remove a container specfied with index number from wob array
    *)
    method container_remove : int -> unit
    (*
    ** Return wob array
    *)
    method items : VX_types.contained array
    (*
    ** Number of wobs
    *)
    method nitems : int
    (*
    ** Assign new wob array
    *)
    method set_items : VX_types.contained array -> unit

    (*
    ** Print box informations (sizes...) in tree format with
    ** childs
    *)
    method print_info : int -> unit

    (*
    ** Output postscript. The (x0,y0) origin is absolute relative to
    ** X origin (upper left corner) and in pixel units.
    *)
    method print : VX_types.ps -> int -> int -> unit
end

class t :
  VX_types.box_desc ->
  VX_types.container ->
  VX_types.widget_attributes list ->
  object
    
    inherit VX_object.t
    
    method clear_items : unit
    method container_add : VX_types.contained -> unit
    method container_exchange : int -> VX_types.contained -> unit
    method container_add_s : VX_types.contained list -> unit
    method container_insert : int -> VX_types.contained -> unit
    method container_remove : int -> unit
    method items : VX_types.contained array
    method nitems : int
    method set_items : VX_types.contained array -> unit
    method print_info : int -> unit
    method print : VX_types.ps -> int -> int -> unit
end


(*
** Vertical aligned box
*)

class v :
  VX_types.container ->
  VX_types.widget_attributes list -> t

(*
** Horizontal aligned box
*)
  
class h :
  VX_types.container ->
  VX_types.widget_attributes list -> t


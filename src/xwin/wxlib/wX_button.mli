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
**    $CREATED:     
**    $VERSION:     1.02
**
**    $INFO:
**
**  Button widget.
**  
**  The set_external_switch method can be used to change the default
**  "wait for release" behaviour. If true, only the external supplied
**  action function will be called. The external function is responsible to
**  set the state of the button with the activate or desactivate
**  method. This method allows button arrays with only one button currently
**  activated!
**
**
**    $ENDOFINFO
**
*)

val default_button_font : string ref

class orig :
  WX_types.container ->
  WX_types.base_attributes list ->
  object
    
    inherit WX_object.t
    
    val mutable wait_release : bool
    val mutable widget : WX_types.contained option
    val mutable old_relief : WX_types.relief    
    val mutable action : unit -> unit
    val mutable activated : bool
    val mutable external_switch : bool

    method action : unit -> unit
    (*
    ** Switch on button
    *)
    method activate : unit
    method container_add : WX_types.contained -> unit
    (*
    ** switch off button
    *)
    method desactivate : unit
    (*
    ** Set the button action
    *)
    method set_action : (unit -> unit) -> unit
    method set_wait_release : bool -> unit
    (*
    ** Do button control outside this class (true)
    *)
    method set_external_switch : bool -> unit
    (*
    ** Set mouse buttons (1,2,3) control
    *)
    method set_buttons : int list -> unit
end

class t :
  WX_types.container ->
  WX_types.base_attributes list ->
  object
    
    inherit WX_object.t
    
    method action : unit -> unit
    method activate : unit
    method container_add : WX_types.contained -> unit
    method desactivate : unit
    method set_action : (unit -> unit) -> unit
    method set_wait_release : bool -> unit
    method set_external_switch : bool -> unit
    
    method set_buttons : int list -> unit
end

class orig_with_label :
  WX_types.container ->
  string ->
  WX_types.base_attributes list -> 
  object
    inherit orig
    
    method set_string : string -> unit
    val mutable label : WX_label.t
    method label : WX_label.t
      end

class with_label :
  WX_types.container ->
  string ->
  WX_types.base_attributes list -> 
  object
    inherit t
    
    method set_string : string -> unit
      method label : WX_label.t
      
end

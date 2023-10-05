(***********************************************************************)
(*                                                                     *)
(*                             ____                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)


class orig :
  WX_types.container ->
  string * WX_types.image_desc ->
  WX_types.base_attributes list ->
  object
    
    val mutable dx : Xtypes.size
    val mutable dy : Xtypes.size
    val mutable pixmap : Xtypes.pixmap
    
      inherit WX_object.t
      method set_imagebuf: Ximage.image_type -> unit
      method modified: unit 
end

class t :
  WX_types.container ->
  string * WX_types.image_desc ->
  WX_types.base_attributes list ->
  object
      inherit WX_object.t
      method set_imagebuf: Ximage.image_type -> unit
      method modified: unit 
end

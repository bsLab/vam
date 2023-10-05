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
**      BSSLAB, Dr. Stefan Bosse www.bsslab.de
**
**    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF: 
**    Free Software Foundation-Europe, GNU GPL License, Version 2
**
**    $MODIFIEDBY:  BSSLAB
**    $AUTHORS:     Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt,
**                  Stefan Bosse
**    $INITIAL:     (C) 1999 Institut National de Recherche en Informatique
**                  et Automatique.  Distributed only by permission.
**    $CREATED:     
**    $VERSION:     1.02
**
**    $INFO:
**
**  This module implements the graphics widget (aka. XGraphics).
**
**    $ENDOFINFO
**
*)


class t :
  WX_types.container ->
  WX_types.base_attributes list ->
  int ->
  int ->
  object
    
    inherit WX_object.t
    
    val mutable gv : XGraphics.view option
    method active : unit

  end

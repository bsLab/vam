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
**    $CREATED:     10.5.2005
**    $VERSION:     1.02
**
**    $INFO:
**
**  VXlib display management
**
**    $ENDOFINFO
**
*)

open Xtypes
open VX_types
open VX_common

class from display = 
  object (self)
    val eloop = Eloop.add_display display (fun ev -> 
          match ev.ev_event with
          | SelectionRequestEvent e -> 
              Selection.handleSelectionRequest display e
          | SelectionClearEvent e ->
              Selection.handleSelectionClear display e
          | _ -> ()      
      )
    val display = display
    
    method display = display
    method eloop = eloop
    method close = 
      Eloop.remove_display eloop;
      Xlib.closeDisplay display
    method broken f =
      display.dpy_broken <- f
end


class t name =
  let display = Xlib.openDisplay name in
  from display

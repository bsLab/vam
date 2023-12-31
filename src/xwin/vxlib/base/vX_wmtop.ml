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

open Icccm
open Xtypes
open VX_types
open VX_common

let hostname = Unix.gethostname ()

class orig root attributes =
  
  object (self)
    
    inherit VX_top.orig root None attributes as super
    val root = root
    
    val mutable wm_name = "wXlib window"
    val mutable wm_icon_name = ""
    val mutable wm_class = "VXlib"
    val mutable wm_class_app = "wXlib"
    val mutable wm_command = []
    val wm_hints = newWM_HINTS ()
    val mutable wm_size_hints = Icccm.newWM_SIZE_HINTS ()
    val mutable wm_transient_for = noWindow
    
    method setWM_NAME n =
      wm_name <- n;
      if not (w.w_window == noWindow) then
        Icccm.setWM_NAME s.s_display w.w_window n
    
    method setWM_ICON_NAME n =
      wm_icon_name <- n;
      if not (w.w_window == noWindow) && n <> "" then
        Icccm.setWM_ICON_NAME s.s_display w.w_window n
    
    method setWM_CLASS app cls =
      wm_class <- cls;
      wm_class_app <- app;
      if not (w.w_window == noWindow) then
        Icccm.setWM_CLASS s.s_display w.w_window [app;cls]
    
    method setWM_TRANSIENT_FOR (top : VX_types.container) =
      wm_transient_for <- top#window;
      if not (w.w_window == noWindow) then
        Icccm.setWM_TRANSIENT_FOR s.s_display w.w_window wm_transient_for
    
    
    method realize =
      super#realize;
      let d = s.s_display in
      let w = w.w_window in
      Icccm.setWM_NAME d w wm_name;
      Icccm.setWM_CLIENT_MACHINE d w hostname;
      if wm_icon_name <> "" then Icccm.setWM_ICON_NAME d w wm_icon_name;
      if wm_command <> [] then Icccm.setWM_COMMAND d w wm_command;
      Icccm.setWM_CLASS d w [wm_class; wm_class_app];
      if not (wm_transient_for == noWindow) then
        Icccm.setWM_TRANSIENT_FOR d w wm_transient_for;
      Icccm.setWM_HINTS d w wm_hints;
      Icccm.setWM_NORMAL_HINTS d w wm_size_hints
    
    method withdraw =
      if not (w.w_window == noWindow) then
        Icccm.withdrawWindow s.s_display root#window w.w_window 
    
    method iconify =
      if not (w.w_window == noWindow) then
        Icccm.iconifyWindow s.s_display root#window w.w_window
    
    method deiconify =
      if not (w.w_window == noWindow) then
        X.mapWindow s.s_display w.w_window    
    
    method setWM_SIZE_HINTS sh =
      wm_size_hints <- sh;
      if not (w.w_window == noWindow) then 
        Icccm.setWM_NORMAL_HINTS s.s_display w.w_window sh

end

class t = orig

(*
**                                                                     
**                           xlib for Ocaml                            
**                                                                     
**       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       
**                                                                     
**  Copyright 1998 Institut National de Recherche en Informatique et   
**  Automatique.  Distributed only by permission.                      
**
** 
**  Modified and restructured by 
**
**        Stefan Bosse
**        sbosse@physik.uni-bremen.de
**
**  Last modified:  22/10/01
**
**  Changes:	-- more comments (in english)
**              -- XKB modifier fix
**
*)
                                                                     


exception EnableToGetKeysyms
exception EnableToGetModifierMapping

val uKeysymToModifiers: Xtypes.display -> Xtypes.keySym -> int
val computeMaskFromKeyTrans: Xtypes.display -> Xtypes.keyTrans -> unit
val recComputeMaskFromKeyTrans: Xtypes.display -> Xtypes.keyTrans list -> unit
val convertCase: int -> int * int
val uKeyCodeToKeySym: Xtypes.display -> int -> int -> Xtypes.keycode
val resetModMap: Xtypes.display -> unit
val initModMap: Xtypes.display -> unit
val uKeyInitialize: Xtypes.display -> unit
val keycodeToKeysym: Xtypes.display -> int -> int -> Xtypes.keycode

exception Found of int * int
val keysymToKeycode: Xtypes.display -> Xtypes.keycode -> int * int
val lookupKeysym: Xtypes.display -> Xtypes.event -> int -> Xtypes.keycode
val refreshKeyboardMapping: Xtypes.display -> Xtypes.event -> unit
val uTranslateKeySym: Xtypes.display -> Xtypes.keySym -> int -> string
val rebindKeysym: Xtypes.display -> Xtypes.keySym -> Xtypes.keySym array ->
                  string -> unit


(*
** Some functions checking the meaning of a keycode.
*)

val isKeypadKey: int -> bool
val isPrivateKeypadKey: int -> bool
val isCursorKey: int -> bool
val isPFKey: int -> bool
val isFunctionKey: int -> bool
val isMiscFunctionKey: int -> bool
val isModifierKey: int -> bool

val uTranslateKey: Xtypes.display -> int -> int -> int * Xtypes.keycode

(*
** Return a X keycode -> char code converted string
*)

val lookupString: Xtypes.display -> Xtypes.event -> string * Xtypes.keycode
                  * int

(*
** ModXX modifier fix
**
** Problem: Because of missing XKB stuff here, we have serious
** problems on various key mappings using the mode_switch modifier and
** probably other ModXX keys with XKB features enabled on the X server.
**
** On each KeyPressMask and KeyReleaseMask event, called from
** xlib event handlers, we scan the keycode, in case we found a ModXX
** key code, the modifier mask will be changed.
**
*)


val modifier_fix: Xtypes.display -> Xtypes.xevent -> Xtypes.xevent

(*
** Add a new keysymbol to string translation
**
** keysym: X keysymbol code (32 Bit unsigned int)
** mask:   Modifier mask:
**          shiftMask lor lockMask lor controlMask lor
**          mod1Mask lor mod2Mask lor mod3Mask lor mod4Mask lor
**          mod5Mask
**          -> Alternate German = Mod3 !!
*)

val addKeysym: Xtypes.display -> Xtypes.keySym -> int ->
                  string -> unit

(*
** Same as above, but several in one list:
**
** [ keysymbol,modmask,string ; .... ]
*)

val addKeysyms: Xtypes.display -> (Xtypes.keySym*int*string) list -> unit


(*
** Some national key mapping extensions avoiding "deadkeys" when using
** the XKB extension in the X server. Add new for your local keyboard 
** mapping. See keyBind.ml for details.
*)

val locale_keysyms: (Xtypes.keySym*int*string) list ref

val germanSyms: (Xtypes.keySym*int*string) list    



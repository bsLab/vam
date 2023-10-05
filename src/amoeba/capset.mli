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
**    $INITIAL:     (C) 2005 BSSLAB
**    $CREATED:     ?
**    $VERSION:     1.02
**
**    $INFO:
**
**  Capability set utilities
**
**    $ENDOFINFO
**
*)


val capset_MAX : int
val short_SIZE : int
val capset_SIZE : int

(*
** Cap set structure
*)

type suite = {
  mutable s_object : Amoeba.capability;
  mutable s_current : bool;
} 
and capset = {
  mutable cs_initial : int;
  mutable cs_final : int;
  mutable cs_suite : suite array;
} 

(*
** Convert a capability to a cap set
*)
val cs_singleton : Amoeba.capability -> capset

(*
** Get a useable capability from a capset, and retrun this cap.
** Returns the first capability in the set for which std_info returns STD_OK.
** If there are no caps in the set for which std_info returns STD_OK,
** then the last cap in the set is returned and  the err status std_INFO.
*)
val cs_goodcap : capset -> Amoeba.status * Amoeba.capability

(*
** Get a capability from a capset giving preference to a working capability.
** If there is only one cap in the set, this cap is returned.  If and only
** if there is more than one, try std_info on each of them to obtain one
** that is useable, and return this one.  If none of the multiple
** caps work, the last one is returned.  Callers who need to know whether the
** cap is useable should use cs_goodcap(), above.  Returns STD_OK, unless
** the capset has no caps, in which case, returns STD_SYSERR.
*)
val cs_to_cap : capset -> Amoeba.status * Amoeba.capability

(*
** Zero capset 
*)
val nilcapset : capset

(*
** Return a fresh capset and copy the original contents
*)
val cs_copy : capset -> capset


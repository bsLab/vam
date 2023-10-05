
(* 
** This file is part of the FIREBALL AMOEBA System.
**
** Written by:
**              Stefan Bosse
**              sbosse@physik.uni-bremen.de
**
** Last modified:
**              12/02/2002
**
** Changes:
**
**
**
** FIREBALL AMOEBA is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public License as 
** published by the Free Software Foundation; version 2.
**
** The FIREBALL AMOEBA is distributed in the hope that it will be usefull,
** but WITHOUT ANY WARRANTY; without even implied warranty of 
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
** General Public License for more details.
**
*) 


(*
** Progress bar widget. 
**
** This widget displays a progess bar. The default range and
** unit is 0 .. 100 %. Either use the decr/incr_progress methods or
** the set_progress method to change the value.
**
*)

class t:
  WX_types.container ->
  WX_types.base_attributes list ->

  object

    inherit WX_object.t

    val mutable backcolor : Xtypes.pixel
    val mutable color : Xtypes.pixel
    val mutable delta : float
    val mutable font : WX_types.font
    val mutable formatter : (float -> string, unit, string) format
    val mutable last_progress : float
    val mutable progress : float
    val mutable range_max : float
    val mutable range_min : float

    method refresh : unit
    method size_request : WX_types.szhints
    method update : unit
    method set_progress : float -> unit
    method incr_progress : float
    method decr_progress : float
    method set_delta : float -> unit
    method set_range : float * float -> unit
    method set_format : (float -> string, unit, string) format -> unit

    method set_backcolor : string ->  unit
    method set_color: string -> unit
end

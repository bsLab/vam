(* 
** This file is part of the FIREBALL AMOEBA System.
**
** Written by:
**		Stefan Bosse
**		sbosse@physik.uni-bremen.de
**
** Last modified:
**              29/09/2002
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
** This program is an adaption to the
** Proposed Federal Information Processing
**  Data Encryption Standard.
** See Federal Register, March 17, 1975 (40FR12134)
*)

val mod_des48_ver : float

(*
** Generate the key
*)

val des_OWsetkey: int array -> unit

(*
** The payoff: encrypt a block. (Now 48 bytes, 1 bit/byte).
** The result is stored in the input block.
*)

val des_OWcrypt48: int array -> unit
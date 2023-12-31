(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: digest.ml,v 1.6 2001/12/07 13:40:50 xleroy Exp $ *)

(* Message digest (MD5) *)

type t = string

external unsafe_string: string -> int -> int -> t = "md5_string"
external channel: in_channel -> int -> t = "md5_chan"

let string str =
  unsafe_string str 0 (String.length str)

let substring str ofs len =
  if ofs < 0 || len < 0 || ofs + len > String.length str
  then invalid_arg "Digest.substring"
  else unsafe_string str ofs len

let file filename =
  let ic = open_in_bin filename in
  let d = channel ic (in_channel_length ic) in
  close_in ic;
  d

let output chan digest =
  output chan digest 0 16

let input chan =
  let digest = String.create 16 in
  really_input chan digest 0 16;
  digest

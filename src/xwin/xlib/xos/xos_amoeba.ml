(* 
** This file is part of the FIREBALL AMOEBA System.
**
** Written by:
**		Stefan Bosse
**		sbosse@physik.uni-bremen.de
**
**
** Changes:
**      -- bytebuf introduced
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
** OS dependent layer. Limited to the kind of connection system
** used to communicate with the X-Server.
**
** AMOEBA: virtual circuit connections
**
*)

let mod_xos_amoeba_ver = 1.0

open Amoeba
open Bytebuf
open Virtcirc
open Thread
open Rpc
open Name
open Cmdreg
open Stderr
open Buf

open Xtypes
open Xauth
open Ll_trans
open Xos

let ax_CONNECT  = Command (x_FIRST_COM + 1)
let ax_REINIT   = Command (x_FIRST_COM + 2)
let ax_SHUTDOWN = Command (x_FIRST_COM + 3)

let max_AMBUFSIZE = max_TRANS

let e = BrokenConnection

(*
** VC management
*)

let vc_init = ref false 
let vc_max_num = 10
let vc_of_channel = ref [||]
let vc_num = ref 0 

let init_vc () =
    if (!vc_init == false) then
    begin
        vc_of_channel := Array.create vc_max_num nilvc;
        vc_init := true; 
    end
    
exception Found of int

let add_vc vc =
  let i = 
    try
    (
        for i = 0 to vc_max_num-1
        do
            if(!vc_of_channel.(i) == nilvc) then
                raise (Found i);
        done;
        -1
    )
    with
        Found i -> i;
  in

  if (i = -1) then 
    failwith "xos_amoeba: too much opened connections";
  (!vc_of_channel).(i) <- vc;
  incr vc_num;
  i

(*
** Open a X server connection and return channel id and the auth
** string tuple.
*)

let local_host = ""
  

let open_channel 
        ~server_name
        ~dpy_port
        ~dpy_num
  =        

  let auth =
    try Xauth.getBestAuth {
                  xauth_family = FamilyLocal;
                  xauth_address = local_host;
                  xauth_number = string_of_int dpy_num;
                  xauth_name = "";
                  xauth_data = "";
                }
    with
        Not_found -> "",""
  in
  

  (*
  ** Contact X server and get the I/O ports.
  *)
  let hdr_req = header_new () in

  (*
  ** lookup the X server cap
  *)

  let (err,svrcap) = name_lookup ("/profile/cap/x11svr/"^server_name^":"^
                    (string_of_int dpy_num))
  in
  if (err <> std_OK) then
    failwith ("open_channel: can't locate X server:"^err_why(err));

  hdr_req.h_port <- svrcap.cap_port;
  hdr_req.h_priv <- svrcap.cap_priv;

  hdr_req.h_command <- ax_CONNECT;

  let repbuf = buf_create (2*port_SIZE) in

  let (err,size,hdr_rep) = trans (hdr_req,nilbuf,0,
                                  repbuf,(2*port_SIZE))
  in

  if (err <> std_OK) then
    failwith ("open_channel: transaction to X server failed:"^(err_why err));

  if (hdr_rep.h_status <> std_OK) then
    failwith ("open_channel: X server response:"^err_why(hdr_rep.h_status));

  if (size < (2*port_SIZE)) then
    failwith "open_channel: inconsistent server reply";

  let (pos,oport) = buf_get_port ~buf:repbuf ~pos:0 in
  let (pos,iport) = buf_get_port ~buf:repbuf ~pos:pos in


  let vc = vc_create ~iport:iport
                     ~oport:oport
                     ~isize:max_AMBUFSIZE
                     ~osize:max_AMBUFSIZE
  in
  let ind = add_vc vc in
  (X_channel ind,auth)
  
(*
** Read data from the client-server connection
*)


let read_channel ~chan ~off ~buf ~len =
  let X_channel ind = chan in 
  let vc = (!vc_of_channel).(ind) in
  if(vc==nilvc) then
    failwith "Xos.read_channel: programming error";

  let nrecv = vc_reads vc ~str:buf ~pos:off ~len:len in
  nrecv 

(*
** Write data
*)

let write_channel ~chan ~off ~buf ~len =
  let X_channel ind = chan in 
  let vc = (!vc_of_channel).(ind) in
  if(vc==nilvc) then
    failwith "Xos.write_channel: programming error";

  let nsent = vc_writes vc ~str:buf ~pos:off ~len:len in
  nsent

(*
** Close the client-server connection.
*)


let close_channel chan =
  let X_channel ind = chan in 
  let vc = (!vc_of_channel).(ind) in
  if(vc==nilvc) then
    failwith "Xos.close_channel: programming error";

  vc_close vc vc_BOTH;
  (!vc_of_channel).(ind) <- nilvc;
  decr vc_num

(*
** Init this module. This module must be loaded after xlib was initialized!
*)


let init () =
    init_vc ();
    xos_fun.xos_open   <- open_channel;
    xos_fun.xos_close  <- close_channel;
    xos_fun.xos_read   <- read_channel;
    xos_fun.xos_write  <- write_channel

let _ =
    init ()

                      
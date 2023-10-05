(* 
** This file is part of the FIREBALL AMOEBA System.
**
** Written by:
**		Stefan Bosse
**		sbosse@physik.uni-bremen.de
**
** Last modified:
**              03/05/2002
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
** OS dependent layer. Limited to the kind of connection system
** used to communicate with the X-Server.
**
** UNIX: socket connections
**
*)


open Unix

open Xtypes
open Xauth
open Ll_trans
open Xos


(*
** Broken connection exception
*)


let e = Ll_trans.BrokenConnection


let local_host = Unix.gethostname ()


(*
** Socket management
*)

let sock_init = ref false 
let sock_max_num = 10
let sock_of_channel = ref [||]
let sock_num = ref 0 

let init_sock () =
    if (!sock_init == false) then
    begin
        sock_of_channel := Array.create sock_max_num nilsock;
        sock_init := true; 
    end
    
exception Found of int

let add_sock sock =
  let i = 
    try
    (
        for i = 0 to sock_max_num-1
        do
            if(!sock_of_channel.(i) == nilsock) then
                raise (Found i);
        done;
        -1
    )
    with
        Found i -> i;
  in

  if (i = -1) then 
    failwith "xos_unix: too much opened connections";
  (!sock_of_channel).(i) <- sock;
  incr sock_num;
  i

(*
** Open the X server connection.
*)

let open_channel
        ~server_name
        ~dpy_port
        ~dpy_num
  =

    let (domain,server,auth) =
      if server_name = "" || server_name = local_host then (* Domaine unix *)
        (
         Unix.PF_UNIX,
         Unix.ADDR_UNIX ("/tmp/.X11-unix/X"^(string_of_int dpy_port)),
         (
          try
            try
              Xauth.getBestAuth 
              {
                xauth_family = FamilyLocal;
                xauth_address = "";
                xauth_number = string_of_int dpy_num;
                xauth_name = "";
                xauth_data = ""
              }         
            with
                Not_found ->
                    try
                        Xauth.getBestAuth 
                        {
                            xauth_family = FamilyLocal;
                            xauth_address = local_host;
                            xauth_number = string_of_int dpy_num;
                            xauth_name = "";
                            xauth_data = ""
                        }
                    with
                        Not_found -> 
                            "", ""
          with
            _ -> "", ""
        )
      )
      else  (* internet *)
      let addr =
        try
          Unix.inet_addr_of_string server_name
        with
          Failure _ -> 
            try
              (Unix.gethostbyname server_name).Unix.h_addr_list.(0)
            with
              Not_found ->
                failwith ("Unknown host"^server_name)
      in
      (
        Unix.PF_INET,
        (Unix.ADDR_INET (addr, (dpy_port+6000))),
        try
          Xauth.getBestAuth {
          xauth_family = FamilyIP;
          xauth_address = Obj.magic addr;
          xauth_number = string_of_int dpy_num;
          xauth_name = "";
          xauth_data = ""
        }
        with
          Not_found -> "", ""
      )
    in

    let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
    Unix.set_close_on_exec sock;
    Unix.connect sock server;

    let ind = add_sock sock in
    
    (X_channel ind,auth)    


(*
** Read data from the client-server connection
*)


let read_channel ~chan ~off ~buf ~len =
  let X_channel ind = chan in
  let sock = (!sock_of_channel).(ind) in
  if(sock==nilsock) then
    failwith "Xos.read_channel: programming error";

  let nrecv = Unix.read sock buf off len in
  nrecv


(*
** Write data 
*)

let write_channel ~chan ~off ~buf ~len =
  let X_channel ind = chan in
  let sock = (!sock_of_channel).(ind) in
  if(sock==nilsock) then
    failwith "Xos.write_channel: programming error";

  let nsent = Unix.write sock buf off len in
  nsent


(*
** Close the client-server connection.
*)

let close_channel chan =

  let X_channel ind = chan in
  let sock = (!sock_of_channel).(ind) in
  if(sock==nilsock) then
    failwith "Xos.close_channel: programming error";

(*
  Unix.close sock;
*)
Unix.shutdown sock SHUTDOWN_ALL;
  (!sock_of_channel).(ind) <- nilsock;
  decr sock_num


(*
** Init this module. This module must be loaded after xlib was initialized!
*)

let init () =
    init_sock ();
    xos_fun.xos_open   <- open_channel;
    xos_fun.xos_close  <- close_channel;
    xos_fun.xos_read   <- read_channel;
    xos_fun.xos_write  <- write_channel;
    xos_fun.xos_string_of_inet_addr <- 
        ( fun s ->
            Unix.string_of_inet_addr (Obj.magic s)
        );
    xos_fun.xos_gethostname <- Unix.gethostname

let _ =
    init ()

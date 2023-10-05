(* 
** This file is part of the FIREBALL AMOEBA System.
**
** Written by:
**		Stefan Bosse
**		sbosse@physik.uni-bremen.de
**
** Last modified:
**              02/05/2002
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

open Xtypes


(*
** The external Xos_XXX module must load the xos data structure
** with the OS/conenction specific functions:
*)

type xos_module = {
    (*
    ** Open the X server connection
    *)

    mutable xos_open: server_name:string -> 
                  dpy_port:int ->
                  dpy_num:int ->
                  x_channel * (string * string);

    (*
    ** Shutdown the connection
    *)

    mutable xos_close: x_channel -> unit;


    (*
    ** Read data from the server-client connection
    *)

    mutable xos_read: chan:x_channel -> 
                  off:int -> 
                  buf:string -> 
                  len:int -> int;

    mutable xos_write : chan:x_channel -> 
                   off:int -> 
                   buf:string -> 
                   len:int -> int;

    mutable xos_string_of_inet_addr: string -> string; 
    mutable xos_gethostname: unit -> string;
}


(*
** First loaded with dummy functions. Entries changed by xos_XXX modules.
*)

let xos_fun = {
    xos_open =      ( fun ~server_name ~dpy_port ~dpy_num -> 
                        failwith "xos not initialized";
                        ((X_channel 0),("",""))
                    );

    xos_close =     ( fun chan -> failwith "xos not initialized");

    xos_read =      ( fun ~chan ~off ~buf ~len -> 
                        failwith "xos not initialized";
                        0
                    );

    xos_write =     ( fun ~chan ~off ~buf ~len -> 
                        failwith "xos not initialized";
                        0
                    );
    xos_string_of_inet_addr = ( fun a -> "");
    xos_gethostname = ( fun () -> "");
}



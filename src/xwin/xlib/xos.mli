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
                  Xtypes.x_channel * (string * string);

    (*
    ** Shutdown the connection
    *)

    mutable xos_close: Xtypes.x_channel -> unit;


    (*
    ** Read data from the server-client connection
    *)

    mutable xos_read: chan:Xtypes.x_channel -> 
                  off:int -> 
                  buf:string -> 
                  len:int -> int;

    mutable xos_write: chan:Xtypes.x_channel -> 
                   off:int -> 
                   buf:string -> 
                   len:int -> int;

    mutable xos_string_of_inet_addr: string -> string; 
    mutable xos_gethostname: unit -> string;
    
}

val xos_fun: xos_module 

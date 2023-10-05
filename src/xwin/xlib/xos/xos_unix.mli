(*
** OS dependent layer. Limited to the kind of connection system
** used to communicate with the X-Server. 
**
** Under UNIX: socket connection
** Under Amoeba: virtual circuit connections
** 
*)



(*
** Init this module
*)

val init : unit -> unit


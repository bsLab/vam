(*
**  THIS SOFTWARE IS OWNED AND COPYRIGHTED BY
**
**    ###     ####   ####               #         ##         #####
**    #  #    #      #                 #         # #        #     #
**    #   #   #      #                #         #  #       #      #
**    #   #   #      #               #         #   #      #      #   
**    ####    ####   ####  ####     #         ######     ########
**    #   #      #      #          #         #     #    #      #
**    #   #      #      #         #         #      #   #       #
**    #  #       #      #        #         #       #  #       #
**    ###     ####   ####       ######### #        # #########
**
**    Stefan Bosse (c) 2003
**   
**  THIS SOFTWARE MAY NOT BE COPIED, EXTRACTED, MODIFIED, OR 
**  OTHERWISE USED IN A CONTEXT OUTSIDE OF THE VAM SYSTEM.
** 
*)


(*
** Ascii representation of ports and capabilities
*)

val mod_ar_ver : float

(*
** Convert a port to an array
*)
val port_to_array : Amoeba.port -> int array

(*
** Return an ASCII representation of a capability
*)
val ar_cap : Amoeba.capability -> string

(*
** Return an ASCII representation of an Amoeba port
*)
val ar_port : Amoeba.port -> string

(*
** Return an ASCII representation of an Amoeba private part
*)
val ar_priv : Amoeba.privat -> string

(*
** Convert a port string representation in the form
**
**  "x:x:x:x:x:x"       [x: hexadecimal]
**
** to an Amoeba port.
*)
val ar_toport : string -> Amoeba.port

(*
** Convert a capability string representation in the form
**
**  "x:x:x:x:x:x/d(x)/x:x:x:x:x:x"  [x: hexadecimal, d:decimal]
**  <- port ->objnum/rights<- random ->
**
** to an Amoeba port.
*)
val ar_tocap : string -> Amoeba.capability

(*
** Convert a private string representation in the form
**
**  "d(x)/x:x:x:x:x:x"  [x: hexadecimal, d:decimal]
**  objnum(rights)<- random ->
**
** to an Amoeba private structure.
*)
val ar_topriv : string -> Amoeba.privat

val ar_cs : Capset.capset -> string

(*
** Some synonymes
*)
val c2a : Amoeba.capability -> string
val a2c : string -> Amoeba.capability

(*
** Some pretty printers
*)
val print_amoeba_status : Amoeba.status -> unit
val print_amoeba_port : Amoeba.port -> unit
val print_amoeba_priv : Amoeba.privat -> unit
val print_amoeba_cap : Amoeba.capability -> unit


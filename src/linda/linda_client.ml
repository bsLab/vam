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
**    $CREATED:     4.7.2005
**    $VERSION:     1.04
**
**    $INFO:
**
**  Client interface for the distributed tuple space
**  ML-LINDA. Fully compatible to HP-Linda, the C implementation of
**  the tuple space system LINDA. 
**
**    $ENDOFINFO
**
*)


open Amoeba
open Cmdreg
open Bytebuf
open Thread
open Linda_common

type lk = linda_data_kind

external out_t1 : 'a -> unit
    = "ext_out_t1"
external out_t2 : ('a*'b)  -> unit
    = "ext_out_t2"
external out_t3 : ('a*'b*'c)  -> unit
    = "ext_out_t3"
external out_t4 : ('a*'b*'c*'d)  -> unit
    = "ext_out_t4"
external out_t5 : ('a*'b*'c*'d*'e)  -> unit
    = "ext_out_t5"
external out_t6 : ('a*'b*'c*'d*'e*'f)  -> unit
    = "ext_out_t6"
external out_t7 : ('a*'b*'c*'d*'e*'f*'g)  -> unit
    = "ext_out_t7"
external out_t8 : ('a*'b*'c*'d*'e*'f*'g*'h)  -> unit
    = "ext_out_t8"
external out_t9 : ('a*'b*'c*'d*'e*'f*'g*'h*'i)  -> unit
    = "ext_out_t9"
external out_t10 : ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j)  -> unit
    = "ext_out_t10"

external in_t1 : lk -> 'a -> 'a
    = "ext_in_t1"
external in_t2 : (lk*lk) -> ('a*'b)  -> ('a*'b)
    = "ext_in_t2"
external in_t3 : (lk*lk*lk) -> ('a*'b*'c)  -> ('a*'b*'c) 
    = "ext_in_t3"
external in_t4 : (lk*lk*lk*lk) -> 
                 ('a*'b*'c*'d)  -> 
                 ('a*'b*'c*'d) 
    = "ext_in_t4"
external in_t5 : (lk*lk*lk*lk*lk) -> 
                 ('a*'b*'c*'d*'e)  -> 
                 ('a*'b*'c*'d*'e) 
    = "ext_in_t5"
external in_t6 : (lk*lk*lk*lk*lk*lk) -> 
                 ('a*'b*'c*'d*'e*'f)  -> 
                 ('a*'b*'c*'d*'e*'f) 
    = "ext_in_t6"
external in_t7 : (lk*lk*lk*lk*lk*lk*lk) -> 
                 ('a*'b*'c*'d*'e*'f*'g)  -> 
                 ('a*'b*'c*'d*'e*'f*'g) 
    = "ext_in_t7"
external in_t8 : (lk*lk*lk*lk*lk*lk*lk*lk) -> 
                 ('a*'b*'c*'d*'e*'f*'g*'h)  -> 
                 ('a*'b*'c*'d*'e*'f*'g*'h) 
    = "ext_in_t8"
external in_t9 : (lk*lk*lk*lk*lk*lk*lk*lk*lk) -> 
                 ('a*'b*'c*'d*'e*'f*'g*'h*'i)  -> 
                 ('a*'b*'c*'d*'e*'f*'g*'h*'i) 
    = "ext_in_t9"
external in_t10 : (lk*lk*lk*lk*lk*lk*lk*lk*lk*lk) -> 
                 ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j)  -> 
                 ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j) 
    = "ext_in_t10"



external rd_t1 : lk -> 'a -> 'a
    = "ext_rd_t1"
external rd_t2 : (lk*lk) -> ('a*'b)  -> ('a*'b)
    = "ext_rd_t2"
external rd_t3 : (lk*lk*lk) -> ('a*'b*'c)  -> ('a*'b*'c) 
    = "ext_rd_t3"
external rd_t4 : (lk*lk*lk*lk) -> 
                 ('a*'b*'c*'d)  -> 
                 ('a*'b*'c*'d) 
    = "ext_rd_t4"
external rd_t5 : (lk*lk*lk*lk*lk) -> 
                 ('a*'b*'c*'d*'e)  -> 
                 ('a*'b*'c*'d*'e) 
    = "ext_rd_t5"
external rd_t6 : (lk*lk*lk*lk*lk*lk) -> 
                 ('a*'b*'c*'d*'e*'f)  -> 
                 ('a*'b*'c*'d*'e*'f) 
    = "ext_rd_t6"
external rd_t7 : (lk*lk*lk*lk*lk*lk*lk) -> 
                 ('a*'b*'c*'d*'e*'f*'g)  -> 
                 ('a*'b*'c*'d*'e*'f*'g) 
    = "ext_rd_t7"
external rd_t8 : (lk*lk*lk*lk*lk*lk*lk*lk) -> 
                 ('a*'b*'c*'d*'e*'f*'g*'h)  -> 
                 ('a*'b*'c*'d*'e*'f*'g*'h) 
    = "ext_rd_t8"
external rd_t9 : (lk*lk*lk*lk*lk*lk*lk*lk*lk) -> 
                 ('a*'b*'c*'d*'e*'f*'g*'h*'i)  -> 
                 ('a*'b*'c*'d*'e*'f*'g*'h*'i) 
    = "ext_rd_t9"
external rd_t10 : (lk*lk*lk*lk*lk*lk*lk*lk*lk*lk) -> 
                 ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j)  -> 
                 ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j) 
    = "ext_rd_t10"


(*
** Initialize linda server capability
*)
external linda_client_init : string -> status
    = "ext_linda_client_init"


external linda_stat : unit -> string
    = "ext_linda_stat"

(*
** Intialize single tuple space
*)

let init spacename =
    linda_client_init spacename

(*
** Linda OUT request. Each function t1..t10 supports a specific
** dimension space (n). 
** The only argument is the data tuple (at). Only pure data arguments are
** currently allowed - no formals!
**
** Example:
**
**  Out.t3 ("MyMatrix",100,100)
*) 

module Out = 
struct
    let t1 at = out_t1 at
    let t2 att = out_t2 att
    let t3 att = out_t3 att
    let t4 att = out_t4 att
    let t5 att = out_t5 att
    let t6 att = out_t6 att
    let t7 att = out_t7 att
    let t8 att = out_t8 att
    let t9 att = out_t9 att
    let t10 att = out_t10 att
end


(*
** Linda In request.
** Addtionally, the kind of each tuple arguement must be specified
** in the kind tuple (kt): D (data) or Q (formal, question). On
** success, the data/formal tuple (at) specified is returned with
** data fields updated. In the case of scalar data types (int,float,char
** and string), dummy values must be specified!
**
** Example:
**
**  let _,_,str = In.t3 (D,D,Q) ("myname",4047,"") 
**
*)

module In =
struct
    let t1 kt at = in_t1 kt at
    let t2 ktt att = in_t2 ktt att
    let t3 ktt att = in_t3 ktt att
    let t4 ktt att = in_t4 ktt att
    let t5 ktt att = in_t5 ktt att
    let t6 ktt att = in_t6 ktt att
    let t7 ktt att = in_t7 ktt att
    let t8 ktt att = in_t8 ktt att
    let t9 ktt att = in_t9 ktt att
    let t10 ktt att = in_t10 ktt att
end

(*
** Same as above, but non destructive read operation.
*)
module Rd =
struct
    let t1 kt at = rd_t1 kt at
    let t2 ktt att = rd_t2 ktt att
    let t3 ktt att = rd_t3 ktt att
    let t4 ktt att = rd_t4 ktt att
    let t5 ktt att = rd_t5 ktt att
    let t6 ktt att = rd_t6 ktt att
    let t7 ktt att = rd_t7 ktt att
    let t8 ktt att = rd_t8 ktt att
    let t9 ktt att = rd_t9 ktt att
    let t10 ktt att = rd_t10 ktt att
end


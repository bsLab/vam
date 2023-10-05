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
**    $VERSION:     1.01
**
**    $INFO:
**
**  Client interface for the distributed tuple space
**  ML-LINDA. Fully compatible to HP-Linda, the C implementation of
**  the tuple space system LINDA. 
**  Multiple space version.
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

external out_t1 : linda_tsid -> 'a -> unit
    = "ext_out_t1_ms"
external out_t2 : linda_tsid -> ('a*'b)  -> unit
    = "ext_out_t2_ms"
external out_t3 : linda_tsid -> ('a*'b*'c)  -> unit
    = "ext_out_t3_ms"
external out_t4 : linda_tsid -> ('a*'b*'c*'d)  -> unit
    = "ext_out_t4_ms"
external out_t5 : linda_tsid -> ('a*'b*'c*'d*'e)  -> unit
    = "ext_out_t5_ms"
external out_t6 : linda_tsid -> ('a*'b*'c*'d*'e*'f)  -> unit
    = "ext_out_t6_ms"
external out_t7 : linda_tsid -> ('a*'b*'c*'d*'e*'f*'g)  -> unit
    = "ext_out_t7_ms"
external out_t8 : linda_tsid -> ('a*'b*'c*'d*'e*'f*'g*'h)  -> unit
    = "ext_out_t8_ms"
external out_t9 : linda_tsid -> ('a*'b*'c*'d*'e*'f*'g*'h*'i)  -> unit
    = "ext_out_t9_ms"
external out_t10 : linda_tsid -> ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j)  -> unit
    = "ext_out_t10_ms"

external in_t1 : linda_tsid -> lk -> 'a -> 'a
    = "ext_in_t1_ms"
external in_t2 : linda_tsid -> (lk*lk) -> ('a*'b)  -> ('a*'b)
    = "ext_in_t2_ms"
external in_t3 : linda_tsid -> (lk*lk*lk) -> ('a*'b*'c)  -> ('a*'b*'c) 
    = "ext_in_t3_ms"
external in_t4 : linda_tsid -> (lk*lk*lk*lk) -> 
                               ('a*'b*'c*'d)  -> 
                               ('a*'b*'c*'d) 
    = "ext_in_t4_ms"
external in_t5 : linda_tsid -> (lk*lk*lk*lk*lk) -> 
                               ('a*'b*'c*'d*'e)  -> 
                               ('a*'b*'c*'d*'e) 
    = "ext_in_t5_ms"
external in_t6 : linda_tsid -> (lk*lk*lk*lk*lk*lk) -> 
                               ('a*'b*'c*'d*'e*'f)  -> 
                               ('a*'b*'c*'d*'e*'f) 
    = "ext_in_t6_ms"
external in_t7 : linda_tsid -> (lk*lk*lk*lk*lk*lk*lk) -> 
                               ('a*'b*'c*'d*'e*'f*'g)  -> 
                               ('a*'b*'c*'d*'e*'f*'g) 
    = "ext_in_t7_ms"
external in_t8 : linda_tsid -> (lk*lk*lk*lk*lk*lk*lk*lk) -> 
                               ('a*'b*'c*'d*'e*'f*'g*'h)  -> 
                               ('a*'b*'c*'d*'e*'f*'g*'h) 
    = "ext_in_t8_ms"
external in_t9 : linda_tsid -> (lk*lk*lk*lk*lk*lk*lk*lk*lk) -> 
                               ('a*'b*'c*'d*'e*'f*'g*'h*'i)  -> 
                               ('a*'b*'c*'d*'e*'f*'g*'h*'i) 
    = "ext_in_t9_ms"
external in_t10 : linda_tsid -> (lk*lk*lk*lk*lk*lk*lk*lk*lk*lk) -> 
                               ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j)  -> 
                               ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j) 
    = "ext_in_t10_ms"


external rd_t1 : linda_tsid -> lk -> 'a -> 'a
    = "ext_rd_t1_ms"
external rd_t2 : linda_tsid -> (lk*lk) -> ('a*'b)  -> ('a*'b)
    = "ext_rd_t2_ms"
external rd_t3 : linda_tsid -> (lk*lk*lk) -> ('a*'b*'c)  -> ('a*'b*'c) 
    = "ext_rd_t3_ms"
external rd_t4 : linda_tsid -> (lk*lk*lk*lk) -> 
                               ('a*'b*'c*'d)  -> 
                               ('a*'b*'c*'d) 
    = "ext_rd_t4_ms"
external rd_t5 : linda_tsid -> (lk*lk*lk*lk*lk) -> 
                               ('a*'b*'c*'d*'e)  -> 
                               ('a*'b*'c*'d*'e) 
    = "ext_rd_t5_ms"
external rd_t6 : linda_tsid -> (lk*lk*lk*lk*lk*lk) -> 
                               ('a*'b*'c*'d*'e*'f)  -> 
                               ('a*'b*'c*'d*'e*'f) 
    = "ext_rd_t6_ms"
external rd_t7 : linda_tsid -> (lk*lk*lk*lk*lk*lk*lk) -> 
                               ('a*'b*'c*'d*'e*'f*'g)  -> 
                               ('a*'b*'c*'d*'e*'f*'g) 
    = "ext_rd_t7_ms"
external rd_t8 : linda_tsid -> (lk*lk*lk*lk*lk*lk*lk*lk) -> 
                               ('a*'b*'c*'d*'e*'f*'g*'h)  -> 
                               ('a*'b*'c*'d*'e*'f*'g*'h) 
    = "ext_rd_t8_ms"
external rd_t9 : linda_tsid -> (lk*lk*lk*lk*lk*lk*lk*lk*lk) -> 
                               ('a*'b*'c*'d*'e*'f*'g*'h*'i)  -> 
                               ('a*'b*'c*'d*'e*'f*'g*'h*'i) 
    = "ext_rd_t9_ms"
external rd_t10 : linda_tsid -> (lk*lk*lk*lk*lk*lk*lk*lk*lk*lk) -> 
                               ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j)  -> 
                               ('a*'b*'c*'d*'e*'f*'g*'h*'i*'j) 
    = "ext_rd_t10_ms"


(*
** Initialize linda server capability
*)
external linda_init_ms : string -> status
    = "ext_linda_init_ms"


external linda_stat : string -> string
    = "ext_linda_stat_ms"

(*
** Initialize single tuple space
*)

let init spacename =
    linda_init_ms spacename

(*
** Get tuple space id from specified name
*)
external space_id : string -> linda_tsid
    = "ext_linda_getid"

    

(*
** Linda OUT request. Each function t1..t10 supports a specific
** dimension space (n). 
** The only argument is the data tuple (at). Only pure data arguments are
** currently allowed - no formals!
**
** Example:
**
**  init "control";;
**  init "log";;
**  let tid = linda_getid "control" ;;
**  Out.t3 tid ("MyMatrix",100,100)
*) 

module Out = 
struct
    let t1 tsid at = out_t1 tsid at
    let t2 tsid att = out_t2 tsid att
    let t3 tsid att = out_t3 tsid att
    let t4 tsid att = out_t4 tsid att
    let t5 tsid att = out_t5 tsid att
    let t6 tsid att = out_t6 tsid att
    let t7 tsid att = out_t7 tsid att
    let t8 tsid att = out_t8 tsid att
    let t9 tsid att = out_t9 tsid att
    let t10 tsid att = out_t10 tsid att
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
**  init "control";;
**  init "log";;
**  let tid = linda_getid "control" ;;
**  let _,_,str = In.t3 tid (D,D,Q) ("mytsid",4047,"") ;;
**
*)

module In =
struct
    let t1 tsid kt at = in_t1 tsid kt at
    let t2 tsid ktt att = in_t2 tsid ktt att
    let t3 tsid ktt att = in_t3 tsid ktt att
    let t4 tsid ktt att = in_t4 tsid ktt att
    let t5 tsid ktt att = in_t5 tsid ktt att
    let t6 tsid ktt att = in_t6 tsid ktt att
    let t7 tsid ktt att = in_t7 tsid ktt att
    let t8 tsid ktt att = in_t8 tsid ktt att
    let t9 tsid ktt att = in_t9 tsid ktt att
    let t10 tsid ktt att = in_t10 tsid ktt att
end

(*
** Same as above, but non destructive read operation.
*)
module Rd =
struct
    let t1 tsid kt at = rd_t1 tsid kt at
    let t2 tsid ktt att = rd_t2 tsid ktt att
    let t3 tsid ktt att = rd_t3 tsid ktt att
    let t4 tsid ktt att = rd_t4 tsid ktt att
    let t5 tsid ktt att = rd_t5 tsid ktt att
    let t6 tsid ktt att = rd_t6 tsid ktt att
    let t7 tsid ktt att = rd_t7 tsid ktt att
    let t8 tsid ktt att = rd_t8 tsid ktt att
    let t9 tsid ktt att = rd_t9 tsid ktt att
    let t10 tsid ktt att = rd_t10 tsid ktt att
end


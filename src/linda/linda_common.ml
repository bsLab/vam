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
**  Common data types and defintions for the distributed tuple space
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

(*
** the maximal tuple space dimension (argument number)
*)

let max_TS_DIM = 20

(*
** maximal allowed threads using the linda interface
*)

let max_TS_THREADS = 20


(*
** ML linda argument specifier (type or kind:D,Q).
** The types were adjusted slightly to ML.
*)
type linda_data_kind =
    | D         (* Is a data field                      *)
    | Q         (* Is a formal field (question)         *) 

(*
** Supported linda data types (automatically recognized)
** Chars are currently not available (can't be distinguished from
** integers in C).
*)
type linda_data_type =
    | S         (* string type                          *)
    | I         (* integer type                         *)
    | F         (* float type                           *)
    | IA        (* Integer array                        *)
    | FA        (* Float array                          *)


(*
** Multiple tuple space version - tuple space id
*)
type linda_tsid = int


external linda_cap : string -> capability
    = "ext_linda_cap"

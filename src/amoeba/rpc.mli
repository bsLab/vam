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
**    $MODIFIEDBY:  BSSLAB
**    $AUTHORS:     
**    $INITIAL:     (C)
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.03
**
**    $INFO:
**
**  Amoeba RPC module
**
**    $ENDOFINFO
**
*)



open Bytebuf

(*
** Amoeba Remote Procedure Call stub functions
*)


(*
** OLD RPC transfer restriction (not needed here !)
*)

val max_TRANS : int


(*
** Client -> Server transaction
**
** [err_stat,rep_size,rep_hdr ] = trans (hdr_req,
**                                       buf_req, req_size
**                                       buf_rep, rep_size)
**
** Zero transaction buffer -> nilbuf
*)

external trans :
  Amoeba.header * buffer * int * buffer * int ->
  Amoeba.status * int * Amoeba.header = "ext_rpc_trans"

(*
** Server get request function
**
** [ err_stat,rep_size,hdr_req ] = getreq (port_req,buf_req,req_size)
*)

external getreq :
  Amoeba.port * buffer * int -> Amoeba.status * int * Amoeba.header
  = "ext_rpc_getreq"

(*
** Server put reply function - must follow always a getreq.
**
** [ err_stat ] = putrep (hdr_rep,buf_rep,rep_size)
*)

external putrep : Amoeba.header * buffer * int -> Amoeba.status
  = "ext_rpc_putrep"



(*
** Client -> Server transaction
**
** [err_stat,rep_size,hdr_rep ] = transo (hdr_req,
**                                        buf_req,req_off,req_size,
**                                        buf_rep,rep_off,rep_size)
**
** An offset within the buffer (req & rep) can be specified.
**
*)


external transo :
  Amoeba.header * buffer * int * int * buffer * int * int ->
  Amoeba.status * int * Amoeba.header = "ext_rpc_transo"

(*
** Server get request function
**
** [ err_stat,rep_size,hdr_req ] = getreqo (port_req,buf_req,req_off,req_size)
**
** An offset within the request buffer can be specified.
*)

external getreqo :
  Amoeba.port * buffer * int * int ->
  Amoeba.status * int * Amoeba.header = "ext_rpc_getreqo"

(*
** Server put reply function - must follow always a getreq.
**
** [ err_stat ] = putrepo (hdr_rep,buf_rep,rep_off,rep_size)
**
** An offset within the reply buffer can be specified.
*)

external putrepo : Amoeba.header * buffer * int * int -> Amoeba.status
  = "ext_rpc_putrepo"


(*
** Set RPC timeout. Each thread has its own timeout value!
*)
external timeout: int -> int
    = "ext_rpc_timeout"

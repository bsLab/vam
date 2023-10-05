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




(*
** Amoeba Remote Procedure Call stub functions
*)


open Amoeba
open Bytebuf

(*
** OLD RPC transfer restriction
*)

let max_TRANS = 30000


(*
** Client -> Server transaction
**
** [err_stat,rep_size,rep_hdr ] = trans (hdr_req,
**                                       buf_req, req_size
**                                       buf_rep, rep_size)
**
** Zero transaction buffer -> nilbuf
*)

external trans: (header  *
                 buffer  * 
                 int     *
                 buffer  *
                 int)  
                -> status*int*header
    = "ext_rpc_trans"


(*
** Server get request function
**
** [ err_stat,rep_size,hdr_req ] = getreq (port_req,buf_req,req_size)
*)

external getreq:    (port   *
                     buffer *  
                     int) 
                -> status*int*header
    = "ext_rpc_getreq"


(*
** Server put reply function - must follow always a getreq.
**
** [ err_stat ] = putrep (hdr_rep,buf_rep,rep_size)
*)

external putrep:    (header *
                     buffer *
                     int) 
                -> status
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

external transo: (header * 
                  buffer * int * int * 
                  buffer * int * int ) 
                -> status*int*header
    = "ext_rpc_transo"


(*
** Server get request function
**
** [ err_stat,rep_size,hdr_req ] = getreqo (port_req,buf_req,req_off,req_size)
**
** An offset within the request buffer can be specified.
*)

external getreqo: (port * 
                   buffer * int * int) 
                -> status*int*header
    = "ext_rpc_getreqo"


(*
** Server put reply function - must follow always a getreq.
**
** [ err_stat ] = putrepo (hdr_rep,buf_rep,rep_off,rep_size)
**
** An offset within the reply buffer can be specified.
*)

external putrepo: (header * buffer * int *int) 
                -> status
    = "ext_rpc_putrepo"



external timeout: int -> int 
    = "ext_rpc_timeout"


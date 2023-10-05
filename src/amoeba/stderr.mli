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
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2003 BSSLAB
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.14
**
**    $INFO:
**
** stderr.ml
**
**        This file contains the standard errors that all servers/user
**        programs should understand.  This includes the RPC errors.
**        There is a special fucntion which can be called to get an
**        explanation of the standard error from the server.
**
**  
**
**    $ENDOFINFO
**
*)


(*
** Changes:
**
** - added std_IOERR
** - added std_WRONGSRV
*)


val std_OK : Amoeba.status

val rpc_FAILURE : Amoeba.status
val rpc_NOTFOUND : Amoeba.status
val rpc_BADADDRESS : Amoeba.status
val rpc_ABORTED : Amoeba.status
val rpc_TRYAGAIN : Amoeba.status
val rpc_COMBAD : Amoeba.status
val rpc_REMOTERPC : Amoeba.status
val rpc_BADPORT : Amoeba.status
val rpc_UNSAFE : Amoeba.status

val bc_FAIL : Amoeba.status
val bc_ABORT : Amoeba.status
val bc_TOOBIG : Amoeba.status
val bc_BADPORT : Amoeba.status
val bc_NOEXIST : Amoeba.status
val bc_TOOMANY : Amoeba.status
val bc_ILLRESET : Amoeba.status

val ipc_PORTINV : Amoeba.status
val ipc_FAILURE : Amoeba.status
val ipc_ABORTED : Amoeba.status
val ipc_NOTFOUND : Amoeba.status
val ipc_CLIENTGONE : Amoeba.status
val ipc_SERVERGONE : Amoeba.status

val std_CAPBAD : Amoeba.status
val std_COMBAD : Amoeba.status
val std_ARGBAD : Amoeba.status
val std_NOTNOW : Amoeba.status
val std_NOSPACE : Amoeba.status
val std_DENIED : Amoeba.status
val std_NOMEM : Amoeba.status
val std_EXISTS : Amoeba.status
val std_NOTFOUND : Amoeba.status
val std_SYSERR : Amoeba.status
val std_INTR : Amoeba.status
val std_OVERFLOW : Amoeba.status
val std_WRITEPROT : Amoeba.status
val std_NOMEDIUM : Amoeba.status
val std_IOERR : Amoeba.status
val std_WRONGSRV : Amoeba.status
val std_OBJBAD : Amoeba.status

val dns_UNAVAIL : Amoeba.status
val dns_NOTEMPTY : Amoeba.status
val dns_UNREACH : Amoeba.status
val dns_CLASH : Amoeba.status  


val stat_copy : Amoeba.status -> Amoeba.status
val error_name_list : (Amoeba.status * string) list

val err_why_find_name :
  (Amoeba.status * string) list -> Amoeba.status -> string

val err_why : Amoeba.status -> string

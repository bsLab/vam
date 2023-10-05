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


open Amoeba
open Cmdreg



(*
** The successful return status (no error) 
*)

let std_OK = Status 0

(*
** RPC failures and errors
*)

let rpc_FAILURE     = Status (-1)
let rpc_NOTFOUND    = Status (-2)
let rpc_BADADDRESS  = Status (-3)
let rpc_ABORTED     = Status (-4)
let rpc_TRYAGAIN    = Status (-5)

(*
** attempt to send error code as a command
*)

let rpc_COMBAD      = Status (-6)

(*
** server returned RPC error in hdr.h_status field
*)

let rpc_REMOTERPC   = Status (-7)

(*
** transaction on null port
*)

let rpc_BADPORT     = Status (-8)
(*
** server is only reachable through a untrusted path
*)

let rpc_UNSAFE      = Status (-9)

(*
** Group return values:
*)

let bc_FAIL         = Status (-20)
and bc_ABORT        = Status (-21)
and bc_TOOBIG       = Status (-22)
and bc_BADPORT      = Status (-23)
and bc_NOEXIST      = Status (-24)
and bc_TOOMANY      = Status (-25)
and bc_ILLRESET     = Status (-26)

(*
** Lokal IPC errors
*)

let ipc_PORTINV     = Status (-30)
and ipc_FAILURE     = Status (-31)
and ipc_ABORTED     = Status (-32)
and ipc_NOTFOUND    = Status (-33)
and ipc_CLIENTGONE  = Status (-34)
and ipc_SERVERGONE  = Status (-35)


(*
** Standard errors
*)

let std_CAPBAD      = Status (std_FIRST_ERR)
and std_COMBAD      = Status (std_FIRST_ERR-1) 
and std_ARGBAD      = Status (std_FIRST_ERR-2)
and std_NOTNOW      = Status (std_FIRST_ERR-3)
and std_NOSPACE     = Status (std_FIRST_ERR-4)
and std_DENIED      = Status (std_FIRST_ERR-5)
and std_NOMEM       = Status (std_FIRST_ERR-6)
and std_EXISTS      = Status (std_FIRST_ERR-7)
and std_NOTFOUND    = Status (std_FIRST_ERR-8)
and std_SYSERR      = Status (std_FIRST_ERR-9)
and std_INTR        = Status (std_FIRST_ERR-10)
and std_OVERFLOW    = Status (std_FIRST_ERR-11)
and std_WRITEPROT   = Status (std_FIRST_ERR-12)
and std_NOMEDIUM    = Status (std_FIRST_ERR-13) 
and std_IOERR       = Status (std_FIRST_ERR-14)
and std_WRONGSRV    = Status (std_FIRST_ERR-15)
and std_OBJBAD      = Status (std_FIRST_ERR-16)

(*
** AFS and DNS server
*)

let dns_UNAVAIL      = Status (dns_FIRST_ERR -1)
let dns_NOTEMPTY     = Status (dns_FIRST_ERR -2)
let dns_UNREACH      = Status (dns_FIRST_ERR -3)
let dns_CLASH        = Status (dns_FIRST_ERR -4)

(*
** Return a copy of the status
*)

let stat_copy stat =
    let (Status statid) = stat in
    (Status statid)
    

(*
** The routine for interpretting the above 
*)

let error_name_list = [
        (std_OK,            "status ok");       

        (rpc_FAILURE,       "rpc failure");
        (rpc_NOTFOUND,      "server not found");
        (rpc_BADADDRESS,    "illegal address");
        (rpc_ABORTED,       "rpc aborted");
        (rpc_TRYAGAIN,      "retry rpc");
        (rpc_BADPORT,       "null port"); 
        (rpc_UNSAFE,        "path to server is untrusted");

        (bc_FAIL,           "group send failure");
        (bc_ABORT,          "group abort");
        (bc_TOOBIG,         "group message too big");
        (bc_BADPORT,        "invalid port");
        (bc_NOEXIST,        "does not exist");
        (bc_TOOMANY,        "out of group resources");
        (bc_ILLRESET,       "illegal group reset");
        
        (std_CAPBAD,        "invalid capability");
        (std_COMBAD,        "invalid command");
        (std_ARGBAD,        "invalid argument");
        (std_NOTNOW,        "not now, please");
        (std_NOSPACE,       "no space");
        (std_DENIED,        "operation denied");
        (std_NOMEM,         "out of memory");
        (std_EXISTS,        "exists already");
        (std_NOTFOUND,      "not found");
        (std_SYSERR,        "internal inconsistency");
        (std_INTR,          "interrupted");
        (std_OVERFLOW,      "buffer overflow");
        (std_WRITEPROT,     "medium write protected");
        (std_NOMEDIUM,      "no medium present in drive");
        (std_IOERR,         "I/O Error");
        (std_WRONGSRV,      "Object from unexpected Server");
        (std_OBJBAD,        "Invalid object");
        
        (dns_UNAVAIL,        "DNS directory service unavailable");
        (dns_NOTEMPTY,       "DNS directory not empty");
        (dns_UNREACH,        "DNS object unreachable");
        (dns_CLASH,          "DNS serializability test failed");
        
(*
        (amex_NOPROG,       "no program given");
        (amex_PDLOOKUP,     "can't find program");
        (amex_PDREAD,       "can't read process descriptor");
        (amex_PDSHORT,      "not all bytes read from process descriptor");
        (amex_PDSIZE,       "inconsistent process size");
        (amex_NOCAPS,       "no capability environment");
        (amex_NOHOST,       "can't find suitable host processor");
        (amex_GETDEF,       "can't get MMU parameters");
        (amex_MALLOC,       "can't make local stack segment");
        (amex_STACKOV,      "env+args too big for stack");
        (amex_SEGCREATE,    "can't create stack segment");
*)        
        (ipc_PORTINV,       "invalid ipc port");
        (ipc_FAILURE,       "ipc failure");
        (ipc_ABORTED,       "ipc aborted");
        (ipc_NOTFOUND,      "ipc not found");
        (ipc_CLIENTGONE,    "ipc client gone");
        (ipc_SERVERGONE,    "ipc server gone");
        ]        


let rec err_why_find_name err_list err_stat =
    match (err_list) with
    | (hd::tl) -> let (err_id,err_str) = hd in
                  if (err_id=err_stat) then
                        err_str
                  else
                        err_why_find_name tl err_stat;
    | []       -> let (Status err_id) = err_stat in
                  ("Unknown Amoeba error: "^(string_of_int err_id))

 
                    

let err_why err_stat =
    err_why_find_name error_name_list err_stat

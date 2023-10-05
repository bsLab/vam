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
**    $INITIAL:     (C) 2003-2005 BSSLAB
**    $CREATED:     
**    $VERSION:     1.18
**
**    $INFO:
**
**        This file contains the list of first and last command codes assigned
**        to each registered server.  Only registered servers are listed.
**        If you wish to register a new servers then mail to
**
**                sci@bsslab.de
**
**        with your request for registration.
**
**        The set of error codes is the negative of the command codes.
**        Note that the RPC error codes are in the range RESERVED_FIRST to
**        RESERVED LAST.
**
**        Registered commands take numbers in the range
**                1000 to (NON_REGISTERED_FIRST - 1)
**        Developers may use command numbers in the range
**                NON_REGISTERED_FIRST to (NON_REGISTERED_LAST - 1)
**                You should make all your command numbers relative to these
**                constants in case they change in Amoeba 4.
**
**        Each server is assigned commands in units of 100.
**        If necessary a server may take more two or more consecutive quanta.
**        Command numbers 1 to 999 are reserved and may NOT be used.
**        The error codes that correspond to these command numbers are for
**        RPC errors.
**        Command numbers from 1000 to 1999 are reserved for standard commands
**        that all servers should implement where relevant.
**
**
**
**    $ENDOFINFO
**
*)


let reserved_FISRT  =    1
let reserved_LAST   =    999

(*
** The standard commands that all servers should support   
*)

let std_FIRST_COM   =   1000
let std_LAST_COM    =   1999

let std_FIRST_ERR   =   (-std_FIRST_COM)
let std_LAST_ERR    =   (-std_LAST_COM)

(*
** Old Bullet server 
*)

let bullet_FIRST_COM        = 2000
let bullet_LAST_COM         = 2099
let bullet_FIRST_ERR        = (-bullet_FIRST_COM)
let bullet_LAST_ERR         = (-bullet_LAST_COM)


(*
** Old Directory Server 
*)

let sp_FIRST_COM            = 2100
let sp_LAST_COM             = 2199

let sp_FIRST_ERR            = (-sp_FIRST_COM)
let sp_LAST_ERR             = (-sp_LAST_COM)

(*
** (New) Atomic File Service server  (AFS)
*)

let afs_FIRST_COM           = 2000
let afs_LAST_COM            = 2099
let afs_FIRST_ERR           = (-afs_FIRST_COM)
let afs_LAST_ERR            = (-afs_LAST_COM)



(*
** (New) Directory and Name Service Server  (DNS)
*)

let dns_FIRST_COM            = 2100
let dns_LAST_COM             = 2199

let dns_FIRST_ERR            = (-dns_FIRST_COM)
let dns_LAST_ERR             = (-dns_LAST_COM)

(*
** Disk server
*)

let disk_FIRST_COM          = 2200
let disk_LAST_COM           = 2299

let disk_FIRST_ERR          = (-disk_FIRST_COM)
let disk_LAST_ERR           = (-disk_LAST_COM)


(*
** System Process Server 
*)

let ps_FIRST_COM     	    = 2300
let ps_LAST_COM           = 2399

let ps_FIRST_ERR          = (-ps_FIRST_COM)
let ps_LAST_ERR           = (-ps_LAST_COM)



(*
** Virtual Circuit Server 
*)

let vc_FIRST_COM            = 3100
let vc_LAST_COM             = 3199

let vc_FIRST_ERR            = (-vc_FIRST_COM)
let vc_LAST_ERR             = (-vc_LAST_COM)


(*
** X Server(s) 
*)

let x_FIRST_COM             = 3400
let x_LAST_COM              = 3499

let x_FIRST_ERR             = (-x_FIRST_COM)
let x_LAST_ERR              = (-x_LAST_COM)

(*
** Kernel system requests
*)

let sys_FIRST_COM             = 2500 
let sys_LAST_COM              = 2599

let sys_FIRST_ERR             = (-sys_FIRST_COM)
let sys_LAST_ERR              = (-sys_LAST_COM)

(*
** TTY Server interface
*)

let tty_FIRST_COM           = 4000
let tty_LAST_COM            = 4050

let tty_FIRST_ERR             = (-tty_FIRST_COM)
let tty_LAST_ERR              = (-tty_LAST_COM)

(*
** Byte stream like interface
*)
let stream_FIRST_COM           = 4051
let stream_LAST_COM            = 4099

let stream_FIRST_ERR             = (-stream_FIRST_COM)
let stream_LAST_ERR              = (-stream_LAST_COM)


(*
** Kernel system resource managers:
**      sys::isr-server
**      sys::io-server
**      sys::ipcseg-server
*)

let sys_ISR_FIRST_COM       = 10300
let sys_ISR_LAST_COM        = 10319

let sys_ISR_FIRST_ERR       = (-sys_ISR_FIRST_COM)
let sys_ISR_LAST_ERR        = (-sys_ISR_LAST_COM)

let sys_IO_FIRST_COM        = 10300
let sys_IO_LAST_COM         = 10319

let sys_IO_FIRST_ERR        = (-sys_IO_FIRST_COM)
let sys_IO_LAST_ERR         = (-sys_IO_LAST_COM)

let sys_IPCSEG_FIRST_COM    = 10400
let sys_IPCSEG_LAST_COM     = 10499

let sys_IPCSEG_FIRST_ERR    = (-sys_IPCSEG_FIRST_COM)
let sys_IPCSEG_LAST_ERR     = (-sys_IPCSEG_FIRST_COM)

(*
** Kernel and network tracing
*)

let trace_FIRST_COM           = 4200
let trace_LAST_COM            = 4299

let trace_FIRST_ERR             = (-trace_FIRST_COM)
let trace_LAST_ERR              = (-trace_LAST_COM)

(*
** Linda parallel tuple space extension server/language 
*)
let linda_FIRST_COM           = 10200
let linda_LAST_COM            = 10299

let linda_FIRST_ERR             = (-linda_FIRST_COM)
let linda_LAST_ERR              = (-linda_LAST_COM)


(*
** Syslog servive
*)
let syslog_FIRST_COM           = 11000
let syslog_LAST_COM            = 11099

let syslog_FIRST_ERR             = (-syslog_FIRST_COM)
let syslog_LAST_ERR              = (-syslog_LAST_COM) 

(*
** The First Number For Non-Registered Servers
*)

let unregistered_FIRST_COM          = 15000
let unregistered_FIRST_ERR          = (-unregistered_FIRST_COM)

let unregistered_LAST_COM          = 31000
let unregistered_LAST_ERR          = (-unregistered_LAST_COM)


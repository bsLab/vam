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
**    $VERSION:     1.19
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



val reserved_FISRT : int
val reserved_LAST : int

(*
** The standard commands that all servers should support
*)
val std_FIRST_COM : int
val std_LAST_COM : int
val std_FIRST_ERR : int
val std_LAST_ERR : int

(*
** Old Directory Server
*)
val sp_FIRST_COM : int
val sp_LAST_COM : int
val sp_FIRST_ERR : int
val sp_LAST_ERR : int

(*
** (New) Directory and Name Server DNS
*)
val dns_FIRST_COM : int
val dns_LAST_COM : int
val dns_FIRST_ERR : int
val dns_LAST_ERR : int

(*
** (Kernel) process server
*)
val ps_FIRST_COM : int
val ps_LAST_COM : int
val ps_FIRST_ERR : int
val ps_LAST_ERR : int

(*
** Old Bullet Server
*)
val bullet_FIRST_COM : int
val bullet_LAST_COM : int
val bullet_FIRST_ERR : int
val bullet_LAST_ERR : int

(*
** (New) Atomic File Server AFS
*)
val afs_FIRST_COM : int
val afs_LAST_COM : int
val afs_FIRST_ERR : int
val afs_LAST_ERR : int


(*
** Disk Server
*)
val disk_FIRST_COM : int
val disk_LAST_COM : int
val disk_FIRST_ERR : int
val disk_LAST_ERR : int

(*
** Virtual Circuit Server
*)
val vc_FIRST_COM : int
val vc_LAST_COM : int
val vc_FIRST_ERR : int
val vc_LAST_ERR : int

(*
** X Server(s)
*)
val x_FIRST_COM : int
val x_LAST_COM : int
val x_FIRST_ERR : int
val x_LAST_ERR : int

(*
** Kernel system requests
*)
val sys_FIRST_COM : int
val sys_LAST_COM : int
val sys_FIRST_ERR : int
val sys_LAST_ERR : int

(*
** TTY Server 
*)
  
val tty_FIRST_COM : int
val tty_LAST_COM : int
val tty_FIRST_ERR : int
val tty_LAST_ERR : int

(*
** Byte stream server
*)
val stream_FIRST_COM : int
val stream_LAST_COM : int
val stream_FIRST_ERR : int
val stream_LAST_ERR : int

(*
** Kernel system resource managers:
**      sys::isr-server
**      sys::io-server 
**      sys::ipcseg-server
*)
  
val sys_ISR_FIRST_COM : int
val sys_ISR_LAST_COM : int
val sys_ISR_FIRST_ERR : int
val sys_ISR_LAST_ERR : int
val sys_IO_FIRST_COM : int
val sys_IO_LAST_COM : int
val sys_IO_FIRST_ERR : int
val sys_IO_LAST_ERR : int
val sys_IPCSEG_FIRST_COM : int
val sys_IPCSEG_LAST_COM : int

(*
** Kernel and network tracing 
*)
  
val trace_FIRST_COM : int
val trace_LAST_COM : int
val trace_FIRST_ERR : int
val trace_LAST_ERR : int

(*
** Syslog service
*)
  
val syslog_FIRST_COM : int
val syslog_LAST_COM : int
val syslog_FIRST_ERR : int
val syslog_LAST_ERR : int

(*
** Linda parallel tuple space extension server/language
*)
  
val linda_FIRST_COM : int
val linda_LAST_COM : int
val linda_FIRST_ERR : int
val linda_LAST_ERR : int

(*
**
*)

val unregistered_FIRST_COM : int
val unregistered_FIRST_ERR : int

val unregistered_LAST_COM  : int
val unregistered_LAST_ERR  : int

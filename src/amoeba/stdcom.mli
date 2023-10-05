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
**    $CREATED:     2001.00.00
**    $VERSION:     1.35
**
**    $INFO:
**
** Implementation of Amoeba's std commands
** NB: the maximum command number is STD_LAST_COM.
**
**    $ENDOFINFO
**
*)


open Bytebuf


val std_MONITOR : Amoeba.command
val std_AGE : Amoeba.command
val std_COPY : Amoeba.command
val std_DESTROY : Amoeba.command
val std_INFO : Amoeba.command
val std_RESTRICT : Amoeba.command
val std_STATUS : Amoeba.command
val std_TOUCH : Amoeba.command
val std_GETPARAMS : Amoeba.command
val std_SETPARAMS : Amoeba.command
val std_NTOUCH : Amoeba.command
val std_EXIT : Amoeba.command
val std_RIGHTS : Amoeba.command
val std_EXEC : Amoeba.command

(*
** Standard information rqeuest. Returns the server information string. The
** server capability is specified with 'cap'.
*)
val std_info : cap:Amoeba.capability -> bufsize:int -> Amoeba.status * string

(*
** Standard status request. Returns the server status. The
** server capability is specified with 'cap'.
*)
val std_status : cap:Amoeba.capability -> bufsize:int -> Amoeba.status * string

(*
** Standard exit request. Send the server the exit command.
** The server capability is specified with 'cap'.
*)
val std_exit : cap:Amoeba.capability -> Amoeba.status

(*
** Standard destroy request. Destroy a server object.
*)
val std_destroy : cap:Amoeba.capability -> Amoeba.status

(*
** Touch a server object. Tell the server, the object is still
** used.
*)

val std_touch : cap:Amoeba.capability -> Amoeba.status

(*
** Age all objects of a server. The live time of all objects owned
** by the server are decremented. Objects with live time equal to
** zero will be destroyed.
*)

val std_age : cap:Amoeba.capability -> Amoeba.status


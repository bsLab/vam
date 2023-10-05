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
**    $VERSION:     1.14
**
**    $INFO:
**
** Standard commands / Part 2
**
**
**    $ENDOFINFO
**
*)




(*
** Standard restrict request. Get a restricted version of
** the capability 'cap' from the server.
*)
val std_restrict : cap:Amoeba.capability -> 
  mask:Amoeba.rights_bits ->
  Amoeba.status * Amoeba.capability

(*
** Standard exec request. Execute a string on a server. The
** reply is returned in a string, too.
*)

val std_exec: srv:Amoeba.capability ->
              args:string list ->
              Amoeba.status * string



(*
** Set parameters for server administration.
**
** Format of argument list: <name>,<value>
*)

val std_set_params : srv:Amoeba.capability ->
                     args: (string * string) list ->
                     Amoeba.status

(*
** Get parameter list.
** Return string tuple format:
** <name>,<range and unit>,<desc>,<value>
*)

val std_get_params : srv:Amoeba.capability ->
                     Amoeba.status * (string*string*string*string) list 

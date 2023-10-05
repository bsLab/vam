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
**    $VERSION:     1.09
**
**    $INFO:
**
** Amoeba capability environment == local named capabilities
**
** Standard environment capabilities:
**
** ROOT             - the root directory
** WORK             - the working directory
** STDIN            - standard input
** STDOUT           - standard output
** STDERR           - standard error
** SESSION/_SESSION - session server
** TTY              - the current terminal
**
** Native Amoeba:
**      Environment is supplied by the process environment 
**      by the process erver
**
** AMUNIX:
**      All environment variables are defined in the user
**      shell environment, for example in the shell profile
**      file. The used format:
**
**          ROOT="0:0:0:0:0:0/0(0)/0:0:0:0:0:0"
**          export ROOT ...
**
**      or with an absolute path name prefixed with
**      /unix for a UNIX file, which holds
**      the capability (stored with buf_put_cap/write_cap). 
**
**          ROOT=/unix/amoeba/server/dns/.rootcap
**
**      or with an absolute path without this prefix. Then, the
**      cap is resolved by the directory server. This implicit that
**      the ROOT capability is already resolved (under UNIX only
**      possible with a direct cap or a unix cap file).
**
**
** In addition to the get_env_cap function, there is a 
** put_env_cap function to create or change environment variables -
** but only in the context of the current program. To perform this,
** an environment variable-capability hash is used.
**
** Note: environment capabilities and strings are supported     
**       on both the native Amoeva and AMUNIX VAM execution environment!
**
**    $ENDOFINFO
**
*)


(*
** VAM can access both, the UNIX file and the Amoeba file
** system - either native or within the AMUNIX environment.
** Therefore it's usefull to handle both, UNIX and Amoeba
** paths.  
*)

(*
** Different types of paths:
**
** /unix/.... -> UNIX host system, UNIX interface is used
** else       -> Amoeba directory interface is used
**
*)

type path_arg = 
    | Unix_path of string
    | Amoeba_path of string

val path_resolve : string -> path_arg

external am_getcap : string -> string = "am_getcap"

(*
** Try to get named environment cap 
*)
val get_env_cap : string -> Amoeba.status * Amoeba.capability

(*
** Store named env cap in environment table. Only valid in
** current process context!
*)
val put_env_cap : string -> Amoeba.capability -> Amoeba.status

(*
** Get all available environment capabilities.
*)

val get_env_caps : unit -> (string * Amoeba.capability) list

(*
** String environment
*)

(*
** Get string value of an environment variable
*)
val get_env_str : string -> string

(*
** Set the string value of an environment variable
*)
val put_env_str : string -> string -> unit
  
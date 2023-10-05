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
**    $VERSION:     1.01
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
**      by the process erver. A 'CAP prefix in the environment
**      name is here ignored!
**
** AMUNIX:
**      All environment variables are defined in the user
**      shell environment, for example in the shell profile
**      file. The used format:
**
**          ROOTCAP="0:0:0:0:0:0/0(0)/0:0:0:0:0:0"
**          export ROOTCAP ...
**
**      or with an absolute path name prefixed with
**      /unix for a UNIX file, which holds
**      the capability (stored with buf_put_cap/write_cap). 
**
**          ROOTCAP=/unix/amoeba/server/dns/.rootcap
**
**      or with an absolute path without this prefix. Then, the
**      cap is resolved by the directory server. This implicit that
**      the ROOT capability is already resolved (under UNIX only
**      possible with a direct cap or a unix cap file).
**
**      The CAP prefix is only present in the UNIX environment
**      name, not within VAM.
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

include Myenv

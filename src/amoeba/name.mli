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
**    $VERSION:     1.02
**
**    $INFO:
**
** Name stub interfaces
**  
**
**    $ENDOFINFO
**
*)


val name_lookup : string -> Amoeba.status * Amoeba.capability
val name_append : name:string -> cap:Amoeba.capability -> Amoeba.status
val name_delete : string -> Amoeba.status

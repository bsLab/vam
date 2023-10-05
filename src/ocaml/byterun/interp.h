/*
**      ==================================
**      OOOO   OOOO OOOO  O      O   OOOO
**      O   O  O    O     O     O O  O   O
**      O   O  O    O     O     O O  O   O
**      OOOO   OOOO OOOO  O     OOO  OOOO
**      O   O     O    O  O    O   O O   O
**      O   O     O    O  O    O   O O   O
**      OOOO   OOOO OOOO  OOOO O   O OOOO
**      ================================== 
**      BSSLAB, Dr. Stefan Bosse www.bsslab.de
**
**    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF: 
**    Free Software Foundation-Europe, GNU GPL License, Version 2
**
**    $MODIFIEDBY:  BSSLAB
**    $AUTHORS:     Xavier Leroy, projet Cristal, INRIA Rocquencourt,
**                  Stefan Bosse
**    $INITIAL:     (C) 1996 INRIA
**    $CREATED:     
**    $VERSION:     1.02
**
**    $INFO:
**
** The bytecode interpreter 
**
**
**    $ENDOFINFO
**
*/



#ifndef _interp_
#define _interp_


#include "misc.h"
#include "mlvalues.h"

value interprete (code_t prog, asize_t prog_size);
/*
** VM pc saved before executing external C code
*/
extern code_t extern_pc;

#endif

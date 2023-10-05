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
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2003 BSSLAB
**    $CREATED:     
**    $VERSION:     1.02
**
**    $INFO:
**
** Some new system dependent functions.
**
**
**    $ENDOFINFO
**
*/



#include "alloc.h"
#include "debugger.h"
#include "fail.h"
#include "instruct.h"
#include "mlvalues.h"
#include "signals.h"
#include "stacks.h"
#include "sys.h"

#include "osdeps.h"


/*
** Return the (OS,OS_ID,ARCH,MODEL,WORDSIZE) tuple. See Amakefile.sys for
** valid parameters.
*/

CAMLprim value sys_get_sys_config(value unit)   
{
  CAMLparam0 ();   /* unit is unused */
  CAMLlocal5 (result, ostype, osid, archtype, model);

  ostype = copy_string(OCAML_OS_TYPE);
  osid = copy_string (OCAML_OS_ID);
  archtype = copy_string(OCAML_ARCH_TYPE);
  model = copy_string(OCAML_MODEL_TYPE);
  
  result = alloc_small (5, 0);
  Field(result, 0) = ostype;
  Field(result, 1) = osid;
  Field(result, 2) = archtype;
  Field(result, 3) = model;
  Field(result, 4) = Val_long (8 * sizeof(value));
  CAMLreturn (result);
}


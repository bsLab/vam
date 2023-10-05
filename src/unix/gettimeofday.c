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
**    $AUTHORS:     Xavier Leroy, Stefan Bosse
**    $INITIAL:     (C) 1996 INRIA
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.01
**
**    $INFO:
**
**
**
**    $ENDOFINFO
**
*/


#include <mlvalues.h>
#include <alloc.h>
#include "unixsupport.h"
#include <fail.h>

#ifdef HAS_GETTIMEOFDAY

#include <sys/types.h>
#include <sys/time.h>

CAMLprim value unix_gettimeofday(value unit)
{
  struct timeval tp;
  if (gettimeofday(&tp, NULL) == -1) uerror("gettimeofday", Nothing);
  return copy_double((double) tp.tv_sec + (double) tp.tv_usec / 1e6);
}

#else

CAMLprim value unix_gettimeofday(value unit)
{ invalid_argument("gettimeofday not implemented"); }

#endif

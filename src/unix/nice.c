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
#include "unixsupport.h"
#include <errno.h>
#include <fail.h>

#ifdef HAS_NICE

#ifdef HAS_GETPRIORITY

#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>

CAMLprim value unix_nice(value incr)
{
  int prio;
  errno = 0;
  prio = getpriority(PRIO_PROCESS, 0);
  if (prio == -1 && errno != 0)
    uerror("nice", Nothing);
  prio += Int_val(incr);
  if (setpriority(PRIO_PROCESS, 0, prio) == -1)
    uerror("nice", Nothing);
  return Val_int(prio);
}

#else /* !HAS_GETPRIORITY */

CAMLprim value unix_nice(value incr)
{
  int ret;
  errno = 0;
  ret = nice(Int_val(incr));
  if (ret == -1 && errno != 0) uerror("nice", Nothing);
  return Val_int(ret);
}

#endif
#else   /* !HAS_NICE */
CAMLprim value unix_nice(value incr)
{
    invalid_argument("nice not implemented");
}

#endif

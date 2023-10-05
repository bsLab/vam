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


#include <sys/types.h>
#include <sys/stat.h>
#include <mlvalues.h>
#include <fail.h>
#include "unixsupport.h"

#ifdef HAS_MKFIFO

CAMLprim value unix_mkfifo(value path, value mode)
{
  if (mkfifo(String_val(path), Int_val(mode)) == -1)
    uerror("mkfifo", path);
  return Val_unit;
}

#else

#include <sys/types.h>
#include <sys/stat.h>

#ifdef S_IFIFO

CAMLprim value unix_mkfifo(value path, value mode)
{
  if (mknod(String_val(path), (Int_val(mode) & 07777) | S_IFIFO, 0) == -1)
    uerror("mkfifo", path);
  return Val_unit;
}

#else

CAMLprim value unix_mkfifo() { invalid_argument("mkfifo not implemented"); }

#endif
#endif

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
#include <fail.h>

#ifdef HAS_SOCKETS

#include <sys/socket.h>

static int shutdown_command_table[] = {
  0, 1, 2
};

CAMLprim value unix_shutdown(value sock, value cmd)
{
  if (shutdown(Int_val(sock), shutdown_command_table[Int_val(cmd)]) == -1)
    uerror("shutdown", Nothing);
  return Val_unit;
}

#else

CAMLprim value unix_shutdown(value sock, value cmd)
{ invalid_argument("shutdown not implemented"); }

#endif

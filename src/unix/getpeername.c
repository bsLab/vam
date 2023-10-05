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

#include "socketaddr.h"

CAMLprim value unix_getpeername(value sock)
{
  int retcode;
  union sock_addr_union addr;
  socklen_param_type addr_len;

  addr_len = sizeof(sock_addr);
  retcode = getpeername(Int_val(sock), &addr.s_gen, &addr_len);
  if (retcode == -1) uerror("getpeername", Nothing);
  return alloc_sockaddr(&addr, addr_len);
}

#else

CAMLprim value unix_getpeername(value sock)
{ invalid_argument("getpeername not implemented"); }
  
#endif

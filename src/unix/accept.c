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
#include <memory.h>
#include <signals.h>
#include "unixsupport.h"
#include <fail.h>

#ifdef HAS_SOCKETS

#include "socketaddr.h"

CAMLprim value unix_accept(value sock)
{
  int retcode;
  value res;
  value a;
  union sock_addr_union addr;
  socklen_param_type addr_len;

  addr_len = sizeof(addr);
  enter_blocking_section();
  retcode = accept(Int_val(sock), &addr.s_gen, &addr_len);
  leave_blocking_section();
  if (retcode == -1) uerror("accept", Nothing);
  a = alloc_sockaddr(&addr, addr_len);
  Begin_root (a);
    res = alloc_small(2, 0);
    Field(res, 0) = Val_int(retcode);
    Field(res, 1) = a;
  End_roots();
  return res;
}

#else

CAMLprim value unix_accept(value sock) { invalid_argument("accept not implemented"); }
  
#endif

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
#include <fail.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include <sys/socket.h>

extern int socket_domain_table[], socket_type_table[];

CAMLprim value unix_socketpair(value domain, value type, value proto)
{
  int sv[2];
  value res;
  if (socketpair(socket_domain_table[Int_val(domain)],
                 socket_type_table[Int_val(type)],
                 Int_val(proto), sv) == -1)
    uerror("socketpair", Nothing);
  res = alloc_small(2, 0);
  Field(res,0) = Val_int(sv[0]);
  Field(res,1) = Val_int(sv[1]);
  return res;
}

#else

CAMLprim value unix_socketpair(value domain, value type, value proto)
{ invalid_argument("socketpair not implemented"); }

#endif

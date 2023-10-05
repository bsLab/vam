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
#include <fail.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"

CAMLprim value unix_inet_addr_of_string(value s)
{
#ifdef HAS_INET_ATON
  struct in_addr address;
  if (inet_aton(String_val(s), &address) == 0)
    failwith("inet_addr_of_string");
  return alloc_inet_addr(address.s_addr);
#else
  unsigned int address;
  address = inet_addr(String_val(s));
  if (address == (unsigned int) -1) failwith("inet_addr_of_string");
  return alloc_inet_addr(address);
#endif
}

#else

extern value alloc_inet_addr();

CAMLprim value unix_inet_addr_of_string(value s)
{ 
    /*
    ** dummy function - needed by unix.ml
    */
    return alloc_inet_addr(0);
}
  
#endif

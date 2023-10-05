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


#include <string.h>
#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <errno.h>
#include "unixsupport.h"
#include <fail.h>

#ifdef HAS_SOCKETS

#include "socketaddr.h"

#ifdef _WIN32
  #define EAFNOSUPPORT WSAEAFNOSUPPORT
#endif

value alloc_inet_addr(uint32 a)
{
  value res;
  /*
  ** Use a string rather than an abstract block so that it can be
  ** marshaled safely.  Remember that a is in network byte order,
  ** hence can be marshaled safely. 
  */
  res = alloc_string(sizeof(uint32));
  GET_INET_ADDR(res) = a;
  return res;
}

void get_sockaddr(value mladr,
                  union sock_addr_union * adr /*out*/,
                  socklen_param_type * adr_len /*out*/)
{
  switch(Tag_val(mladr)) {
#ifndef _WIN32
  case 0:                       /* ADDR_UNIX */
    { value path;
      mlsize_t len;
      path = Field(mladr, 0);
      len = string_length(path);
      adr->s_unix.sun_family = AF_UNIX;
      if (len >= sizeof(adr->s_unix.sun_path)) {
        unix_error(ENAMETOOLONG, "", path);
      }
      memmove (adr->s_unix.sun_path, String_val(path), len + 1);
      *adr_len =
        ((char *)&(adr->s_unix.sun_path) - (char *)&(adr->s_unix))
        + len;
      break;
    }
#endif /* _WIN32 */
  case 1:                       /* ADDR_INET */
    {
      char * p;
      int n;
      for (p = (char *) &adr->s_inet, n = sizeof(adr->s_inet);
           n > 0; p++, n--)
        *p = 0;
      adr->s_inet.sin_family = AF_INET;
      adr->s_inet.sin_addr.s_addr = GET_INET_ADDR(Field(mladr, 0));
      adr->s_inet.sin_port = htons(Int_val(Field(mladr, 1)));
      *adr_len = sizeof(struct sockaddr_in);
      break;
    }
  }
}

value alloc_sockaddr(union sock_addr_union * adr /*in*/,
                     socklen_param_type adr_len)
{
  value res;
  switch(adr->s_gen.sa_family) {
#ifndef _WIN32 
  case AF_UNIX:
    { value n = copy_string(adr->s_unix.sun_path);
      Begin_root (n);
        res = alloc_small(1, 0);
        Field(res,0) = n;
      End_roots();
      break;
    }
#endif /* _WIN32 */
  case AF_INET:
    { value a = alloc_inet_addr(adr->s_inet.sin_addr.s_addr);
      Begin_root (a);
        res = alloc_small(2, 1);
        Field(res,0) = a;
        Field(res,1) = Val_int(ntohs(adr->s_inet.sin_port));
      End_roots();
      break;
    }
  default:
    unix_error(EAFNOSUPPORT, "", Nothing);
  }
  return res;
}
#else /* !HAS_SOCKETS */
value alloc_inet_addr(uint32 a)
{
  value res;
  /*
  ** Use a string rather than an abstract block so that it can be
  ** marshaled safely.  Remember that a is in network byte order,
  ** hence can be marshaled safely. 
  */
  res = alloc_string(sizeof(uint32));
  return res;
}

#endif /* !HAS_SOCKETS */

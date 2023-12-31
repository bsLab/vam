/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id: gethost.c,v 1.20 2001/12/07 13:40:29 xleroy Exp $ */

#include <string.h>
#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <fail.h>
#include <signals.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"
#ifndef _WIN32
#include <netdb.h>
#endif

static int entry_h_length;

extern int socket_domain_table[];

static value alloc_one_addr(char const *a)
{
  struct in_addr addr;
  memmove (&addr, a, entry_h_length);
  return alloc_inet_addr(addr.s_addr);
}

static value alloc_host_entry(struct hostent *entry)
{
  value res;
  value name = Val_unit, aliases = Val_unit;
  value addr_list = Val_unit, adr = Val_unit;

  Begin_roots4 (name, aliases, addr_list, adr);
    name = copy_string((char *)(entry->h_name));
    aliases = copy_string_array((const char**)entry->h_aliases);
    entry_h_length = entry->h_length;
#ifdef h_addr
    addr_list = alloc_array(alloc_one_addr, (const char**)entry->h_addr_list);
#else
    adr = alloc_one_addr(entry->h_addr);
    addr_list = alloc_small(1, 0);
    Field(addr_list, 0) = adr;
#endif
    res = alloc_small(4, 0);
    Field(res, 0) = name;
    Field(res, 1) = aliases;
    Field(res, 2) = entry->h_addrtype == PF_UNIX ? Val_int(0) : Val_int(1);
    Field(res, 3) = addr_list;
  End_roots();
  return res;
}

CAMLprim value unix_gethostbyaddr(value a)
{
  uint32 adr;
  struct hostent * entry;
  adr = GET_INET_ADDR(a);
  enter_blocking_section();
  entry = gethostbyaddr((char *) &adr, 4, AF_INET);
  leave_blocking_section();
  if (entry == (struct hostent *) NULL) raise_not_found();
  return alloc_host_entry(entry);
}

CAMLprim value unix_gethostbyname(value name)
{
  char hostname[256];
  struct hostent * entry;
  strncpy(hostname, String_val(name), sizeof(hostname) - 1);
  hostname[sizeof(hostname) - 1] = 0;
  enter_blocking_section();
  entry = gethostbyname(hostname);
  leave_blocking_section();
  if (entry == (struct hostent *) NULL) raise_not_found();
  return alloc_host_entry(entry);
}

#else

CAMLprim value unix_gethostbyaddr(value name)
{ invalid_argument("gethostbyaddr not implemented"); }
  
CAMLprim value unix_gethostbyname(value name)
{ invalid_argument("gethostbyname not implemented"); }
 
#endif

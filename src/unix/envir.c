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

/* $Id: envir.c,v 1.9 2001/12/07 13:40:27 xleroy Exp $ */

#include <mlvalues.h>
#include <alloc.h>

#include "unixsupport.h"

#ifndef _WIN32
extern char ** environ;
#endif

CAMLprim value unix_environment(void)
{
  return copy_string_array((const char**)environ);
}

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

/* $Id: access.c,v 1.9 2001/12/07 13:40:24 xleroy Exp $ */

#include <mlvalues.h>
#include <alloc.h>
#include "unixsupport.h"

#ifdef HAS_UNISTD
# include <unistd.h>
#else
# ifndef _WIN32
#  include <sys/file.h>
#  ifndef R_OK
#   define R_OK    4/* test for read permission */
#   define W_OK    2/* test for write permission */
#   define X_OK    1/* test for execute (search) permission */
#   define F_OK    0/* test for presence of file */
#  endif
# else
#  define R_OK    4/* test for read permission */
#  define W_OK    2/* test for write permission */
#  define X_OK    4/* test for execute (search) permission */
#  define F_OK    0/* test for presence of file */
# endif
#endif

static int access_permission_table[] = {
  R_OK, W_OK, X_OK, F_OK
};

CAMLprim value unix_access(value path, value perms)
{
  int ret;
  ret = access(String_val(path),
               convert_flag_list(perms, access_permission_table));
  if (ret == -1)
    uerror("access", path);
  return Val_unit;
}

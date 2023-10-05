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

/*
** Bytebuf.buffer version.
*/

#include <string.h>
#include <mlvalues.h>
#include <memory.h>
#include <signals.h>
#include <fail.h>
#include "unixsupport.h"
#include "../buffer/buf.h"

CAMLprim value unix_readb(value fd, value buf, value vofs, value len)
{
  long numbytes;
  int ret;
    
  struct caml_buf *vbuf = Buf_val(buf);
  long ofs = Long_val(vofs);
  long size = vbuf->size;

  numbytes = Long_val(len);
  if (ofs+numbytes > size) 
  {
    failwith ("Unix.readb: buffer overflow");
  }
  if (ofs < 0 ||
      numbytes < 0) 
  {
    failwith ("Unix.readb: invalid argument (off,len)");
  }

  Begin_root (buf);
    
    if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;

    enter_blocking_section();
    ret = read(Int_val(fd), &((vbuf->data)[ofs]), (int) numbytes);
    leave_blocking_section();

    if (ret == -1) uerror("readb", Nothing);
  End_roots();
  return Val_int(ret);
}

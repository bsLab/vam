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
** Bytebuf.buffer version
*/


#include <errno.h>
#include <string.h>
#include <mlvalues.h>
#include <fail.h>
#include <memory.h>
#include <signals.h>
#include "unixsupport.h"

#include "../buffer/buf.h"

#ifndef EAGAIN
#define EAGAIN (-1)
#endif
#ifndef EWOULDBLOCK
#define EWOULDBLOCK (-1)
#endif

CAMLprim value unix_writeb(value fd, value buf, value vofs, value vlen)
{
  struct caml_buf *vbuf = Buf_val(buf);
  int size = vbuf->size;
    
  long ofs, len, written;
  int numbytes, ret;

  ofs = Long_val(vofs);
  len = Long_val(vlen);
  
  if (ofs+len > size)
  {
    failwith ("Unix.writeb: buffer overflow");
  }
  if (ofs < 0 ||
      len < 0 )
  {
    failwith ("Unix.writeb: invalid argument (off,len)");
  }
            

  Begin_root (buf);
    written = 0;
    while (len > 0) {
      numbytes = len > UNIX_BUFFER_SIZE ? UNIX_BUFFER_SIZE : len;

      enter_blocking_section();
      ret = write(Int_val(fd), &((vbuf->data)[ofs]), (int)numbytes);
      leave_blocking_section();

      if (ret == -1) {
        if ((errno == EAGAIN || errno == EWOULDBLOCK) && written > 0) break;
        uerror("writeb", Nothing);
      }
      written += ret;
      ofs += ret;
      len -= ret;
    }
  End_roots();
  return Val_long(written);
}

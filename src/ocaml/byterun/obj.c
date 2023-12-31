/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: obj.c,v 1.16 2001/12/07 13:39:33 xleroy Exp $ */

/* Operations on objects */

#include "alloc.h"
#include "fail.h"
#include "gc.h"
#include "major_gc.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "prims.h"

CAMLprim value static_alloc(value size)
{
  return (value) stat_alloc((asize_t) Long_val(size));
}

CAMLprim value static_free(value blk)
{
  stat_free((void *) blk);
  return Val_unit;
}

CAMLprim value static_resize(value blk, value new_size)
{
  return (value) stat_resize((char *) blk, (asize_t) Long_val(new_size));
}

CAMLprim value obj_is_block(value arg)
{
  return Val_bool(Is_block(arg));
}

CAMLprim value obj_tag(value arg)
{
  return Val_int(Tag_val(arg));
}

CAMLprim value obj_block(value tag, value size)
{
  value res;
  mlsize_t sz, i;
  tag_t tg;

  sz = Long_val(size);
  tg = Long_val(tag);
  if (sz == 0) return Atom(tg);
  res = alloc(sz, tg);
  for (i = 0; i < sz; i++)
    Field(res, i) = Val_long(0);

  return res;
}

CAMLprim value obj_dup(value arg)
{
  CAMLparam1 (arg);
  CAMLlocal1 (res);
  mlsize_t sz, i;
  tag_t tg;

  sz = Wosize_val(arg);
  if (sz == 0) return arg;

  tg = Tag_val(arg);
  res = alloc(sz, tg);
  for (i = 0; i < sz; i++)
    Field(res, i) = Field(arg, i);

  CAMLreturn (res);
}

/* Shorten the given block to the given size and return void.
   Raise Invalid_argument if the given size is less than or equal
   to 0 or greater than the current size.

   algorithm:
   Change the length field of the header.  Make up a white object
   with the leftover part of the object: this is needed in the major
   heap and harmless in the minor heap.
*/
CAMLprim value obj_truncate (value v, value newsize)
{
  mlsize_t new_wosize = Long_val (newsize);
  header_t hd = Hd_val (v);
  tag_t tag = Tag_hd (hd);
  color_t color = Color_hd (hd);
  mlsize_t wosize = Wosize_hd (hd);
  mlsize_t i;

  if (tag == Double_array_tag) new_wosize *= Double_wosize;  /* PR#156 */

  if (new_wosize <= 0 || new_wosize > wosize) 
    invalid_argument ("Obj.truncate");
  if (new_wosize == wosize) return Val_unit;
  /* PR#61: since we're about to lose our references to the elements
     beyond new_wosize in v, erase them explicitly so that the GC
     can darken them as appropriate. */
  if (tag < No_scan_tag) {
    for (i = new_wosize; i < wosize; i++){
      modify(&Field(v, i), Val_unit);
#ifdef DEBUG
      Field (v, i) = Debug_free_truncate;
#endif
    }
  }
  Field (v, new_wosize) =
    Make_header (Wosize_whsize (wosize-new_wosize), 0, Caml_white);
  Hd_val (v) = Make_header (new_wosize, tag, color);
  return Val_unit;
}

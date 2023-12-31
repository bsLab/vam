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

/* $Id: compare.c,v 1.22 2001/12/07 13:39:23 xleroy Exp $ */

#include <string.h>
#include <stdlib.h>
#include "custom.h"
#include "fail.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"

/* Structural comparison on trees. */

struct compare_item { value * v1, * v2; mlsize_t count; };

#define COMPARE_STACK_INIT_SIZE 256
#define COMPARE_STACK_MAX_SIZE (1024*1024)

static struct compare_item compare_stack_init[COMPARE_STACK_INIT_SIZE];

static struct compare_item * compare_stack = compare_stack_init;
static struct compare_item * compare_stack_limit = compare_stack_init
                                                   + COMPARE_STACK_INIT_SIZE;

/* Free the compare stack if needed */
static void compare_free_stack(void)
{
  if (compare_stack != compare_stack_init) {
    stat_free(compare_stack);
    /* Reinitialize the globals for next time around */
    compare_stack = compare_stack_init;
    compare_stack_limit = compare_stack + COMPARE_STACK_INIT_SIZE;
  }
}

/* Same, then raise Out_of_memory */
static void compare_stack_overflow(void)
{
  compare_free_stack();
  raise_out_of_memory();
}

/* Grow the compare stack */
static struct compare_item * compare_resize_stack(struct compare_item * sp)
{
  asize_t newsize = 2 * (compare_stack_limit - compare_stack);
  asize_t sp_offset = sp - compare_stack;
  struct compare_item * newstack;

  if (newsize >= COMPARE_STACK_MAX_SIZE) compare_stack_overflow();
  if (compare_stack == compare_stack_init) {
    newstack = malloc(sizeof(struct compare_item) * newsize);
    if (newstack == NULL) compare_stack_overflow();
    memcpy(newstack, compare_stack_init,
           sizeof(struct compare_item) * COMPARE_STACK_INIT_SIZE);
  } else {
    newstack =
      realloc(compare_stack, sizeof(struct compare_item) * newsize);
    if (newstack == NULL) compare_stack_overflow();
  }
  compare_stack = newstack;
  compare_stack_limit = newstack + newsize;
  return newstack + sp_offset;
}

/* Structural comparison */

static long compare_val(value v1, value v2)
{
  struct compare_item * sp;
  tag_t t1, t2;

  sp = compare_stack;
  while (1) {
    if (v1 == v2) goto next_item;
    if (Is_long(v1)) {
      if (Is_long(v2))
        return Long_val(v1) - Long_val(v2);
      else
        return -1;
    }
    if (Is_long(v2)) return 1;
    /* If one of the objects is outside the heap (but is not an atom),
       use address comparison. Since both addresses are 2-aligned,
       shift lsb off to avoid overflow in subtraction. */
    if ((!Is_atom(v1) && !Is_young(v1) && !Is_in_heap(v1)) ||
        (!Is_atom(v2) && !Is_young(v2) && !Is_in_heap(v2)))
      return (v1 >> 1) - (v2 >> 1);
    t1 = Tag_val(v1);
    t2 = Tag_val(v2);
    if (t1 != t2) return (long)t1 - (long)t2;
    switch(t1) {
    case String_tag: {
      mlsize_t len1, len2, len;
      unsigned char * p1, * p2;
      len1 = string_length(v1);
      len2 = string_length(v2);
      for (len = (len1 <= len2 ? len1 : len2),
             p1 = (unsigned char *) String_val(v1),
             p2 = (unsigned char *) String_val(v2);
           len > 0;
           len--, p1++, p2++)
        if (*p1 != *p2) return (long)*p1 - (long)*p2;
      if (len1 != len2) return len1 - len2;
      break;
    }
    case Double_tag: {
      double d1 = Double_val(v1);
      double d2 = Double_val(v2);
      if (d1 < d2) return -1;
      if (d1 > d2) return 1;
      break;
    }
    case Double_array_tag: {
      mlsize_t sz1 = Wosize_val(v1) / Double_wosize;
      mlsize_t sz2 = Wosize_val(v2) / Double_wosize;
      mlsize_t i;
      if (sz1 != sz2) return sz1 - sz2;
      for (i = 0; i < sz1; i++) {
        double d1 = Double_field(v1, i);
        double d2 = Double_field(v2, i);
        if (d1 < d2) return -1;
        if (d1 > d2) return 1;
      }
      break;
    }
    case Abstract_tag:
      compare_free_stack();
      invalid_argument("equal: abstract value");
    case Closure_tag:
    case Infix_tag:
      compare_free_stack();
      invalid_argument("equal: functional value");
    case Object_tag: {
      long oid1 = Oid_val(v1);
      long oid2 = Oid_val(v2);
      if (oid1 != oid2) return oid1 - oid2;
      break;
    }
    case Custom_tag: {
      int res = Custom_ops_val(v1)->compare(v1, v2);
      if (res != 0) return res;
      break;
    }
    default: {
      mlsize_t sz1 = Wosize_val(v1);
      mlsize_t sz2 = Wosize_val(v2);
      /* Compare sizes first for speed */
      if (sz1 != sz2) return sz1 - sz2;
      if (sz1 == 0) break;
      /* Remember that we still have to compare fields 1 ... sz - 1 */
      if (sz1 > 1) {
        sp++;
        if (sp >= compare_stack_limit) sp = compare_resize_stack(sp);
        sp->v1 = &Field(v1, 1);
        sp->v2 = &Field(v2, 1);
        sp->count = sz1 - 1;
      }
      /* Continue comparison with first field */
      v1 = Field(v1, 0);
      v2 = Field(v2, 0);
      continue;
    }
    }
  next_item:
    /* Pop one more item to compare, if any */
    if (sp == compare_stack) return 0; /* we're done */
    v1 = *(sp->v1)++;
    v2 = *(sp->v2)++;
    if (--(sp->count) == 0) sp--;
  }
}

CAMLprim value compare(value v1, value v2)
{
  long res = compare_val(v1, v2);
  /* Free stack if needed */
  if (compare_stack != compare_stack_init) compare_free_stack();
  if (res < 0) 
    return Val_int(-1);
  else if (res > 0)
    return Val_int(1);
  else
    return Val_int(0);
}

CAMLprim value equal(value v1, value v2)
{
  long res = compare_val(v1, v2);
  if (compare_stack != compare_stack_init) compare_free_stack();
  return Val_int(res == 0);
}

CAMLprim value notequal(value v1, value v2)
{
  long res = compare_val(v1, v2);
  if (compare_stack != compare_stack_init) compare_free_stack();
  return Val_int(res != 0);
}

CAMLprim value lessthan(value v1, value v2)
{
  long res = compare_val(v1, v2);
  if (compare_stack != compare_stack_init) compare_free_stack();
  return Val_int(res < 0);
}

CAMLprim value lessequal(value v1, value v2)
{
  long res = compare_val(v1, v2);
  if (compare_stack != compare_stack_init) compare_free_stack();
  return Val_int(res <= 0);
}

CAMLprim value greaterthan(value v1, value v2)
{
  long res = compare_val(v1, v2);
  if (compare_stack != compare_stack_init) compare_free_stack();
  return Val_int(res > 0);
}

CAMLprim value greaterequal(value v1, value v2)
{
  long res = compare_val(v1, v2);
  if (compare_stack != compare_stack_init) compare_free_stack();
  return Val_int(res >= 0);
}

/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: minor_gc.c,v 1.28 2001/12/07 13:39:32 xleroy Exp $ */

#include <string.h>
#include "config.h"
#include "fail.h"
#include "finalise.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "major_gc.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "signals.h"

asize_t minor_heap_size;
char *young_start = NULL, *young_end = NULL;
char *young_ptr = NULL, *young_limit = NULL;
static value **ref_table = NULL, **ref_table_end, **ref_table_threshold;
value **ref_table_ptr = NULL, **ref_table_limit;
static asize_t ref_table_size, ref_table_reserve;
int in_minor_collection = 0;

void set_minor_heap_size (asize_t size)
{
  char *new_heap;
  value **new_table;

  Assert (size >= Minor_heap_min);
  Assert (size <= Minor_heap_max);
  Assert (size % sizeof (value) == 0);
  if (young_ptr != young_end) minor_collection ();
                                           Assert (young_ptr == young_end);
  new_heap = (char *) stat_alloc (size);
  if (young_start != NULL){
    stat_free (young_start);
  }
  young_start = new_heap;
  young_end = new_heap + size;
  young_limit = young_start;
  young_ptr = young_end;
  minor_heap_size = size;

  ref_table_size = minor_heap_size / sizeof (value) / 8;
  ref_table_reserve = 256;
  new_table = (value **) stat_alloc ((ref_table_size + ref_table_reserve)
                                     * sizeof (value *));
  if (ref_table != NULL) stat_free (ref_table);
  ref_table = new_table;
  ref_table_ptr = ref_table;
  ref_table_threshold = ref_table + ref_table_size;
  ref_table_limit = ref_table_threshold;
  ref_table_end = ref_table + ref_table_size + ref_table_reserve;
}

void oldify (value v, value *p)
{
  value result, field0;
  header_t hd;
  mlsize_t sz, i;
  tag_t tag;

 tail_call:
  if (Is_block (v) && Is_young (v)){
    Assert (Hp_val (v) >= young_ptr);
    hd = Hd_val (v);
    if (hd == 0){            /* Already forwarded ? */
      *p = Field (v, 0);     /* Then the forward pointer is the first field. */
    }else if ((tag = Tag_hd (hd)), (tag == Infix_tag)) {
      mlsize_t offset = Infix_offset_hd (hd);
      oldify(v - offset, p);
      *p += offset;
    }else if (tag >= No_scan_tag){
      sz = Wosize_hd (hd);
      result = alloc_shr (sz, tag);
      for (i = 0; i < sz; i++) Field(result, i) = Field(v, i);
      Hd_val (v) = 0;                         /* Put the forward flag. */
      Field (v, 0) = result;                  /* And the forward pointer. */
      *p = result;
    }else{
      /* We can do recursive calls before all the fields are filled, because
         we will not be calling the major GC. */
      sz = Wosize_hd (hd);
      result = alloc_shr (sz, tag);
      *p = result;
      field0 = Field (v, 0);
      Hd_val (v) = 0;                         /* Put the forward flag. */
      Field (v, 0) = result;                  /* And the forward pointer. */
      if (sz == 1) {
        p = &Field (result, 0);
        v = field0;
        goto tail_call;
      } else {
        oldify (field0, &Field (result, 0));
        for (i = 1; i < sz - 1; i++){
          oldify (Field(v, i), &Field (result, i));
        }
        p = &Field (result, i);
        v = Field (v, i);
        goto tail_call;
      }
    }
  }else{
    *p = v;
  }
}

/* Make sure the minor heap is empty by performing a minor collection
   if needed.
*/
void empty_minor_heap (void)
{
  value **r;

  if (young_ptr != young_end){
    in_minor_collection = 1;
    gc_message (0x02, "<", 0);
    oldify_local_roots();
    for (r = ref_table; r < ref_table_ptr; r++) oldify (**r, *r);
    if (young_ptr < young_limit) young_ptr = young_limit;
    stat_minor_words += Wsize_bsize (young_end - young_ptr);
    young_ptr = young_end;
    ref_table_ptr = ref_table;
    ref_table_limit = ref_table_threshold;
    gc_message (0x02, ">", 0);
    in_minor_collection = 0;
  }
  final_empty_young ();
#ifdef DEBUG
  {
    value *p;
    for (p = (value *) young_start; p < (value *) young_end; ++p){
      *p = Debug_free_minor;
    }
  }
#endif
}

/* Do a minor collection and a slice of major collection, call finalisation
   functions, etc.
   Leave the minor heap empty.
*/
void minor_collection (void)
{
  long prev_alloc_words = allocated_words;

  empty_minor_heap ();

  stat_promoted_words += allocated_words - prev_alloc_words;
  ++ stat_minor_collections;
  major_collection_slice ();
  force_major_slice = 0;
  
  final_do_calls ();

  empty_minor_heap ();
}

value check_urgent_gc (value extra_root)
{
  CAMLparam1 (extra_root);
  if (force_major_slice) minor_collection();
  CAMLreturn (extra_root);
}

void realloc_ref_table (void)
{                                 Assert (ref_table_ptr == ref_table_limit);
                                  Assert (ref_table_limit <= ref_table_end);
                            Assert (ref_table_limit >= ref_table_threshold);

  if (ref_table_limit == ref_table_threshold){
    gc_message (0x08, "ref_table threshold crossed\n", 0);
    ref_table_limit = ref_table_end;
    urge_major_slice ();
  }else{ /* This will almost never happen with the bytecode interpreter. */
    asize_t sz;
    asize_t cur_ptr = ref_table_ptr - ref_table;
                                                  Assert (force_major_slice);

    ref_table_size *= 2;
    sz = (ref_table_size + ref_table_reserve) * sizeof (value *);
    gc_message (0x08, "Growing ref_table to %ldk bytes\n", (long) sz / 1024);
    ref_table = (value **) realloc ((char *) ref_table, sz);
    if (ref_table == NULL) fatal_error ("Fatal error: ref_table overflow\n");
    ref_table_end = ref_table + ref_table_size + ref_table_reserve;
    ref_table_threshold = ref_table + ref_table_size;
    ref_table_ptr = ref_table + cur_ptr;
    ref_table_limit = ref_table_end;
  }
}

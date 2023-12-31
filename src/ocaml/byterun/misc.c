/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: misc.c,v 1.21 2001/12/07 13:39:33 xleroy Exp $ */

#include <stdio.h>
#include "config.h"
#include "misc.h"
#include "memory.h"
#ifdef HAS_UI
#include "ui.h"
#endif

#ifdef DEBUG

void caml_failed_assert (char * expr, char * file, int line)
{
  fprintf (stderr, "file %s; line %d ### Assertion failed: %s\n",
           file, line, expr);
  fflush (stderr);
  exit (100);
}

#endif

unsigned long verb_gc = 0;

void gc_message (int level, char *msg, unsigned long arg)
{
  if (level < 0 || (verb_gc & level) != 0){
#ifdef HAS_UI
    ui_print_stderr(msg, (void *) arg);
#else
    fprintf (stderr, msg, arg);
    fflush (stderr);
#endif
  }
}

void fatal_error (char *msg)
{
#ifdef HAS_UI
  ui_print_stderr("%s", msg);
  ui_exit (2);
#else
  fprintf (stderr, "%s", msg);
  exit(2);
#endif
}

void fatal_error_arg (char *fmt, char *arg)
{
#ifdef HAS_UI
  ui_print_stderr(fmt, arg);
  ui_exit (2);
#else
  fprintf (stderr, fmt, arg);
  exit(2);
#endif
}

void fatal_error_arg2 (char *fmt1, char *arg1, char *fmt2, char *arg2)
{
#ifdef HAS_UI
  ui_print_stderr(fmt1, arg1);
  ui_print_stderr(fmt2, arg2);
  ui_exit (2);
#else
  fprintf (stderr, fmt1, arg1);
  fprintf (stderr, fmt2, arg2);
  exit(2);
#endif
}

char *aligned_malloc (asize_t size, int modulo, void **block)
{
  char *raw_mem;
  unsigned long aligned_mem;
                                                  Assert (modulo < Page_size);
  raw_mem = (char *) malloc (size + Page_size);
  if (raw_mem == NULL) return NULL;
  *block = raw_mem;
  raw_mem += modulo;                /* Address to be aligned */
  aligned_mem = (((unsigned long) raw_mem / Page_size + 1) * Page_size);
#ifdef DEBUG
  {
    unsigned long *p;
    unsigned long *p0 = (void *) *block,
                  *p1 = (void *) (aligned_mem - modulo),
                  *p2 = (void *) (aligned_mem - modulo + size),
                  *p3 = (void *) ((char *) *block + size + Page_size);

    for (p = p0; p < p1; p++) *p = Debug_filler_align;
    for (p = p1; p < p2; p++) *p = Debug_uninit_align;
    for (p = p2; p < p3; p++) *p = Debug_filler_align;
  }
#endif
  return (char *) (aligned_mem - modulo);
}

void ext_table_init(struct ext_table * tbl, int init_capa)
{
  tbl->size = 0;
  tbl->capacity = init_capa;
  tbl->contents = stat_alloc(sizeof(void *) * init_capa);
}

int ext_table_add(struct ext_table * tbl, void * data)
{
  int res;
  if (tbl->size >= tbl->capacity) {
    tbl->capacity *= 2;
    tbl->contents =
      stat_resize(tbl->contents, sizeof(void *) * tbl->capacity);
  }
  res = tbl->size;
  tbl->contents[res] = data;
  tbl->size++;
  return res;
}

void ext_table_free(struct ext_table * tbl, int free_entries)
{
  int i;
  if (free_entries)
    for (i = 0; i < tbl->size; i++) stat_free(tbl->contents[i]);
  stat_free(tbl->contents);
}

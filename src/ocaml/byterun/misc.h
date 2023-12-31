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

/* $Id: misc.h,v 1.19 2001/12/07 13:39:33 xleroy Exp $ */

/* Miscellaneous macros and variables. */

#ifndef _misc_
#define _misc_


#include "config.h"

/* Standard definitions */

#include <stddef.h>
#include <stdlib.h>

/* Basic types and constants */

typedef size_t asize_t;

#ifndef NULL
#define NULL 0
#endif

typedef char * addr;

#ifdef __GNUC__
/* Works only in GCC 2.5 and later */
#define Noreturn __attribute ((noreturn))
#else
#define Noreturn
#endif

/* Export control (to mark primitives and to handle Windows DLL) */

#if defined(_WIN32) && defined(_DLL)
# define CAMLexport __declspec(dllexport)
# define CAMLprim __declspec(dllexport)
# if defined(IN_OCAMLRUN)
#  define CAMLextern __declspec(dllexport) extern
# else
#  define CAMLextern __declspec(dllimport) extern
# endif
#else
# define CAMLexport
# define CAMLprim
# define CAMLextern extern
#endif

/* Assertions */

#ifdef DEBUG
#define CAMLassert(x) if (!(x)) caml_failed_assert ( #x , __FILE__, __LINE__)
void caml_failed_assert (char *, char *, int) Noreturn;
#else
#define CAMLassert(x)
#endif

void fatal_error (char *msg) Noreturn;
void fatal_error_arg (char *fmt, char *arg) Noreturn;
void fatal_error_arg2 (char *fmt1, char *arg1, 
                       char *fmt2, char *arg2) Noreturn;

/* Data structures */

struct ext_table {
  int size;
  int capacity;
  void ** contents;
};

extern void ext_table_init(struct ext_table * tbl, int init_capa);
extern int ext_table_add(struct ext_table * tbl, void * data);
extern void ext_table_free(struct ext_table * tbl, int free_entries);

/* GC flags and messages */

extern unsigned long verb_gc;
void gc_message (int, char *, unsigned long);

/* Memory routines */

char *aligned_malloc (asize_t, int, void **);

#ifdef DEBUG
#ifdef ARCH_SIXTYFOUR
#define Debug_tag(x) (0xD700D7D7D700D6D7ul \
                      | ((unsigned long) (x) << 16) \
                      | ((unsigned long) (x) << 48))
#else
#define Debug_tag(x) (0xD700D6D7ul | ((unsigned long) (x) << 16))
#endif /* ARCH_SIXTYFOUR */

/*
  00 -> free words in minor heap
  01 -> fields of free list blocks in major heap
  03 -> heap chunks deallocated by heap shrinking
  04 -> fields deallocated by obj_truncate
  10 -> uninitialised fields of minor objects
  11 -> uninitialised fields of major objects
  15 -> uninitialised words of aligned_malloc blocks
  85 -> filler bytes of aligned_malloc

  special case (byte by byte):
  D7 -> uninitialised words of stat_alloc blocks
*/
#define Debug_free_minor     Debug_tag (0x00)
#define Debug_free_major     Debug_tag (0x01)
#define Debug_free_shrink    Debug_tag (0x03)
#define Debug_free_truncate  Debug_tag (0x04)
#define Debug_uninit_minor   Debug_tag (0x10)
#define Debug_uninit_major   Debug_tag (0x11)
#define Debug_uninit_align   Debug_tag (0x15)
#define Debug_filler_align   Debug_tag (0x85)

#define Debug_uninit_stat    0xD7
#endif /* DEBUG */


#ifndef CAML_AVOID_CONFLICTS
#define Assert CAMLassert
#endif


#endif /* _misc_ */

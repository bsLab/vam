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

/* $Id: intern.c,v 1.44 2001/12/07 13:39:29 xleroy Exp $ */

/* Structured input, compact format */

#include <string.h>
#include "alloc.h"
#include "custom.h"
#include "fail.h"
#include "gc.h"
#include "intext.h"
#include "io.h"
#include "memory.h"
#include "mlvalues.h"
#include "misc.h"
#include "reverse.h"

static unsigned char * intern_src;
/* Reading pointer in block holding input data. */

static unsigned char * intern_input;
/* Pointer to beginning of block holding input data.
   Meaningful only if intern_input_malloced = 1. */

static int intern_input_malloced;
/* 1 if intern_input was allocated by stat_alloc()
   and needs stat_free() on error, 0 otherwise. */

static header_t * intern_dest;
/* Writing pointer in destination block */

static header_t * intern_extra_block;
/* If non-NULL, point to new heap chunk allocated with alloc_for_heap. */

static asize_t obj_counter;
/* Count how many objects seen so far */

static value * intern_obj_table;
/* The pointers to objects already seen */

static unsigned int intern_color;
/* Color to assign to newly created headers */

static header_t intern_header;
/* Original header of the destination block.
   Meaningful only if intern_extra_block is NULL. */

static value intern_block;
/* Point to the heap block allocated as destination block.
   Meaningful only if intern_extra_block is NULL. */

#define Sign_extend_shift ((sizeof(long) - 1) * 8)
#define Sign_extend(x) (((long)(x) << Sign_extend_shift) >> Sign_extend_shift)

#define read8u() (*intern_src++)
#define read8s() Sign_extend(*intern_src++)
#define read16u() \
  (intern_src += 2, \
   (intern_src[-2] << 8) + intern_src[-1])
#define read16s() \
  (intern_src += 2, \
   (Sign_extend(intern_src[-2]) << 8) + intern_src[-1])
#define read32u() \
  (intern_src += 4, \
   (intern_src[-4] << 24) + (intern_src[-3] << 16) + \
   (intern_src[-2] << 8) + intern_src[-1])
#define read32s() \
  (intern_src += 4, \
   (Sign_extend(intern_src[-4]) << 24) + (intern_src[-3] << 16) + \
   (intern_src[-2] << 8) + intern_src[-1])

#ifdef ARCH_SIXTYFOUR
static long read64s(void)
{
  long res;
  int i;
  res = 0;
  for (i = 0; i < 8; i++) res = (res << 8) + intern_src[i];
  intern_src += 8;
  return res;
}
#endif

#define readblock(dest,len) \
  (memmove((dest), intern_src, (len)), intern_src += (len))

static void intern_cleanup(void)
{
  if (intern_input_malloced) stat_free(intern_input);
  if (intern_obj_table != NULL) stat_free(intern_obj_table);
  if (intern_extra_block != NULL) {
    /* free newly allocated heap chunk */
    free_for_heap(intern_extra_block); 
  } else if (intern_block != 0) {
    /* restore original header for heap block, otherwise GC is confused */
    Hd_val(intern_block) = intern_header;
  }
}

static void intern_rec(value *dest)
{
  unsigned int code;
  tag_t tag;
  mlsize_t size, len, ofs_ind;
  value v, clos;
  asize_t ofs;
  header_t header;
  char cksum[16];
  struct custom_operations * ops;

 tailcall:
  code = read8u();
  if (code >= PREFIX_SMALL_INT) {
    if (code >= PREFIX_SMALL_BLOCK) {
      /* Small block */
      tag = code & 0xF;
      size = (code >> 4) & 0x7;
    read_block:
      if (size == 0) {
        v = Atom(tag);
      } else {
        v = Val_hp(intern_dest);
        *dest = v;
        if (intern_obj_table != NULL) intern_obj_table[obj_counter++] = v;
        dest = (value *) (intern_dest + 1);
        *intern_dest = Make_header(size, tag, intern_color);
        intern_dest += 1 + size;
        for(/*nothing*/; size > 1; size--, dest++)
          intern_rec(dest);
        goto tailcall;
      }
    } else {
      /* Small integer */
      v = Val_int(code & 0x3F);
    }
  } else {
    if (code >= PREFIX_SMALL_STRING) {
      /* Small string */
      len = (code & 0x1F);
    read_string:
      size = (len + sizeof(value)) / sizeof(value);
      v = Val_hp(intern_dest);
      if (intern_obj_table != NULL) intern_obj_table[obj_counter++] = v;
      *intern_dest = Make_header(size, String_tag, intern_color);
      intern_dest += 1 + size;
      Field(v, size - 1) = 0;
      ofs_ind = Bsize_wsize(size) - 1;
      Byte(v, ofs_ind) = ofs_ind - len;
      readblock(String_val(v), len);
    } else {
      switch(code) {
      case CODE_INT8:
        v = Val_long(read8s());
        break;
      case CODE_INT16:
        v = Val_long(read16s());
        break;
      case CODE_INT32:
        v = Val_long(read32s());
        break;
      case CODE_INT64:
#ifdef ARCH_SIXTYFOUR
        v = Val_long(read64s());
        break;
#else
        intern_cleanup();
        failwith("input_value: integer too large");
        break;
#endif
      case CODE_SHARED8:
        ofs = read8u();
      read_shared:
        Assert (ofs > 0);
        Assert (ofs <= obj_counter);
        Assert (intern_obj_table != NULL); 
        v = intern_obj_table[obj_counter - ofs];
        break;
      case CODE_SHARED16:
        ofs = read16u();
        goto read_shared;
      case CODE_SHARED32:
        ofs = read32u();
        goto read_shared;
      case CODE_BLOCK32:
        header = (header_t) read32u();
        tag = Tag_hd(header);
        size = Wosize_hd(header);
        goto read_block;
      case CODE_STRING8:
        len = read8u();
        goto read_string;
      case CODE_STRING32:
        len = read32u();
        goto read_string;
      case CODE_DOUBLE_LITTLE:
      case CODE_DOUBLE_BIG:
        if (sizeof(double) != 8) {
          intern_cleanup();
          invalid_argument("input_value: non-standard floats");
        }
        v = Val_hp(intern_dest);
        if (intern_obj_table != NULL) intern_obj_table[obj_counter++] = v;
        *intern_dest = Make_header(Double_wosize, Double_tag, intern_color);
        intern_dest += 1 + Double_wosize;
        readblock((char *) v, 8);
        if (code != CODE_DOUBLE_NATIVE) Reverse_64(v, v);
        break;
      case CODE_DOUBLE_ARRAY8_LITTLE:
      case CODE_DOUBLE_ARRAY8_BIG:
        len = read8u();
      read_double_array:
        if (sizeof(double) != 8) {
          intern_cleanup();
          invalid_argument("input_value: non-standard floats");
        }
        size = len * Double_wosize;
        v = Val_hp(intern_dest);
        if (intern_obj_table != NULL) intern_obj_table[obj_counter++] = v;
        *intern_dest = Make_header(size, Double_array_tag, intern_color);
        intern_dest += 1 + size;
        readblock((char *) v, len * 8);
        if (code != CODE_DOUBLE_ARRAY8_NATIVE && 
            code != CODE_DOUBLE_ARRAY32_NATIVE) {
          mlsize_t i;
          for (i = 0; i < len; i++) Reverse_64((value)((double *)v + i),
                                               (value)((double *)v + i));
        }
        break;
      case CODE_DOUBLE_ARRAY32_LITTLE:
      case CODE_DOUBLE_ARRAY32_BIG:
        len = read32u();
        goto read_double_array;
      case CODE_CODEPOINTER:
        ofs = read32u();
        readblock(cksum, 16);
        if (memcmp(cksum, code_checksum(), 16) != 0) {
          intern_cleanup();
          failwith("input_value: code mismatch");
        }
        v = (value) (code_area_start + ofs);
        break;
      case CODE_INFIXPOINTER:
        ofs = read32u();
        intern_rec(&clos);
        v = clos + ofs;
        break;
      case CODE_CUSTOM:
        ops = find_custom_operations((char *) intern_src);
        if (ops == NULL) {
          intern_cleanup();
          failwith("input_value: unknown custom block identifier");
        }
        while (*intern_src++ != 0) /*nothing*/;  /*skip identifier*/
        size = ops->deserialize((void *) (intern_dest + 2));
        size = 1 + (size + sizeof(value) - 1) / sizeof(value);
        v = Val_hp(intern_dest);
        if (intern_obj_table != NULL) intern_obj_table[obj_counter++] = v;
        *intern_dest = Make_header(size, Custom_tag, intern_color);
        Custom_ops_val(v) = ops;
        intern_dest += 1 + size;
        break;
      default:
        intern_cleanup();
        failwith("input_value: ill-formed message");
      }
    }
  }
  *dest = v;
}

static void intern_alloc(mlsize_t whsize, mlsize_t num_objects)
{
  mlsize_t wosize;

  if (whsize == 0) {
    intern_obj_table = NULL;
    intern_extra_block = NULL;
    intern_block = 0;
    return;
  }
  wosize = Wosize_whsize(whsize);
  if (wosize > Max_wosize) {
    /* Round desired size up to next page */
    asize_t request =
      ((Bsize_wsize(whsize) + Page_size - 1) >> Page_log) << Page_log;
    intern_extra_block = alloc_for_heap(request);
    if (intern_extra_block == NULL) raise_out_of_memory();
    intern_color = allocation_color(intern_extra_block);
    intern_dest = intern_extra_block;
  } else {
    /* this is a specialised version of alloc from alloc.c */
    if (wosize == 0){
      intern_block = Atom (String_tag);
    }else if (wosize <= Max_young_wosize){
      intern_block = alloc_small (wosize, String_tag);
    }else{
      intern_block = alloc_shr (wosize, String_tag);
      /* do not do the urgent_gc check here because it might darken
         intern_block into gray and break the Assert 3 lines down */
    }
    intern_header = Hd_val(intern_block);
    intern_color = Color_hd(intern_header);
    Assert (intern_color == Caml_white || intern_color == Caml_black);
    intern_dest = (header_t *) Hp_val(intern_block);
    intern_extra_block = NULL;
  }
  obj_counter = 0;
  if (num_objects > 0)
    intern_obj_table = (value *) stat_alloc(num_objects * sizeof(value));
  else
    intern_obj_table = NULL;
}

static void intern_add_to_heap(mlsize_t whsize)
{
  /* Add new heap chunk to heap if needed */
  if (intern_extra_block != NULL) {
    /* If heap chunk not filled totally, build free block at end */
    asize_t request =
      ((Bsize_wsize(whsize) + Page_size - 1) >> Page_log) << Page_log;
    header_t * end_extra_block = intern_extra_block + Wsize_bsize(request);
    Assert(intern_dest <= end_extra_block);
    if (intern_dest < end_extra_block)
      *intern_dest =
        Make_header(Wosize_whsize(end_extra_block-intern_dest), 0, Caml_white);
    add_to_heap(intern_extra_block);
  }
}

value input_val(struct channel *chan)
{
  uint32 magic;
  mlsize_t block_len, num_objects, size_32, size_64, whsize;
  char * block;
  value res;

  if (! channel_binary_mode(chan))
    failwith("input_value: not a binary channel");
  magic = getword(chan);
  if (magic != Intext_magic_number) failwith("input_value: bad object");
  block_len = getword(chan);
  num_objects = getword(chan);
  size_32 = getword(chan);
  size_64 = getword(chan);
  /* Read block from channel */
  block = stat_alloc(block_len);
  /* During really_getblock, concurrent input_val operations can take
     place (via signal handlers or context switching in systhreads),
     and intern_input may change.  So, wait until really_getblock
     is over before using intern_input and the other global vars. */
  if (really_getblock(chan, block, block_len) == 0) {
    stat_free(block);
    failwith("input_value: truncated object");
  }
  intern_input = (unsigned char *) block;
  intern_input_malloced = 1;
  intern_src = intern_input;
  /* Allocate result */
#ifdef ARCH_SIXTYFOUR
  whsize = size_64;
#else
  whsize = size_32;
#endif
  intern_alloc(whsize, num_objects);
  /* Fill it in */
  intern_rec(&res);
  intern_add_to_heap(whsize);
  /* Free everything */
  stat_free(intern_input);
  if (intern_obj_table != NULL) stat_free(intern_obj_table);
  return res;
}

CAMLprim value input_value(value vchan)
{
  CAMLparam1 (vchan);
  struct channel * chan = Channel(vchan);
  CAMLlocal1 (res);

  Lock(chan);
  res = input_val(chan);
  Unlock(chan);
  CAMLreturn (res);
}

CAMLexport value input_val_from_string(value str, long int ofs)
{
  CAMLparam1 (str);
  mlsize_t num_objects, size_32, size_64, whsize;
  CAMLlocal1 (obj);

  intern_src = &Byte_u(str, ofs + 2*4);
  intern_input_malloced = 0;
  num_objects = read32u();
  size_32 = read32u();
  size_64 = read32u();
  /* Allocate result */
#ifdef ARCH_SIXTYFOUR
  whsize = size_64;
#else
  whsize = size_32;
#endif
  intern_alloc(whsize, num_objects);
  intern_src = &Byte_u(str, ofs + 5*4); /* If a GC occurred */
  /* Fill it in */
  intern_rec(&obj);
  intern_add_to_heap(whsize);
  /* Free everything */
  if (intern_obj_table != NULL) stat_free(intern_obj_table);
  CAMLreturn (obj);
}

CAMLprim value input_value_from_string(value str, value ofs)
{
  return input_val_from_string(str, Long_val(ofs));
}

static value input_val_from_block(void)
{
  mlsize_t num_objects, size_32, size_64, whsize;
  value obj;

  num_objects = read32u();
  size_32 = read32u();
  size_64 = read32u();
  /* Allocate result */
#ifdef ARCH_SIXTYFOUR
  whsize = size_64;
#else
  whsize = size_32;
#endif
  intern_alloc(whsize, num_objects);
  /* Fill it in */
  intern_rec(&obj);
  intern_add_to_heap(whsize);
  /* Free everything */
  if (intern_obj_table != NULL) stat_free(intern_obj_table);
  return obj;
}

CAMLexport value input_value_from_malloc(char * data, long ofs)
{
  mlsize_t magic, block_len;
  value obj;

  intern_input = (unsigned char *) data;
  intern_src = intern_input + ofs;
  intern_input_malloced = 1;
  magic = read32u();
  if (magic != Intext_magic_number) 
    failwith("input_value_from_malloc: bad object");
  block_len = read32u();
  obj = input_val_from_block();
  /* Free the input */
  stat_free(intern_input);
  if (intern_obj_table != NULL) stat_free(intern_obj_table);
  return obj;
}

CAMLexport value input_value_from_block(char * data, long len)
{
  mlsize_t magic, block_len;
  value obj;

  intern_input = (unsigned char *) data;
  intern_src = intern_input;
  intern_input_malloced = 0;
  magic = read32u();
  if (magic != Intext_magic_number) 
    failwith("input_value_from_block: bad object");
  block_len = read32u();
  if (5*4 + block_len > len)
    failwith("input_value_from_block: bad block length");
  obj = input_val_from_block();
  if (intern_obj_table != NULL) stat_free(intern_obj_table);
  return obj;
}

CAMLprim value marshal_data_size(value buff, value ofs)
{
  uint32 magic;
  mlsize_t block_len;

  intern_src = &Byte_u(buff, Long_val(ofs));
  intern_input_malloced = 0;
  magic = read32u();
  if (magic != Intext_magic_number) failwith("Marshal.data_size: bad object");
  block_len = read32u();
  return Val_long(block_len);
}

/* Return an MD5 checksum of the code area */

#ifdef NATIVE_CODE

#include "md5.h"

unsigned char * code_checksum()
{
  static unsigned char checksum[16];
  static int checksum_computed = 0;

  if (! checksum_computed) {
    struct MD5Context ctx;
    MD5Init(&ctx);
    MD5Update(&ctx,
              (unsigned char *) code_area_start,
              code_area_end - code_area_start);
    MD5Final(checksum, &ctx);
    checksum_computed = 1;
  }
  return checksum;
}

#else

#include "fix_code.h"

unsigned char * code_checksum(void)
{
  return code_md5;
}

#endif

/* Functions for writing user-defined marshallers */

CAMLexport int deserialize_uint_1(void)
{
  return read8u();
}

CAMLexport int deserialize_sint_1(void)
{
  return read8s();
}

CAMLexport int deserialize_uint_2(void)
{
  return read16u();
}

CAMLexport int deserialize_sint_2(void)
{
  return read16s();
}

CAMLexport uint32 deserialize_uint_4(void)
{
  return read32u();
}

CAMLexport int32 deserialize_sint_4(void)
{
  return read32s();
}

CAMLexport uint64 deserialize_uint_8(void)
{
  uint64 i;
  deserialize_block_8(&i, 1);
  return i;
}

CAMLexport int64 deserialize_sint_8(void)
{
  int64 i;
  deserialize_block_8(&i, 1);
  return i;
}

CAMLexport float deserialize_float_4(void)
{
  float f;
  deserialize_block_4(&f, 1);
  return f;
}

CAMLexport double deserialize_float_8(void)
{
  double f;
  deserialize_block_8(&f, 1);
  return f;
}

CAMLexport void deserialize_block_1(void * data, long len)
{
  memmove(data, intern_src, len);
  intern_src += len;
}

CAMLexport void deserialize_block_2(void * data, long len)
{
  unsigned char * p, * q;
#ifndef ARCH_BIG_ENDIAN
  for (p = intern_src, q = data; len > 0; len--, p += 2, q += 2)
    Reverse_16(q, p);
  intern_src = p;
#else
  memmove(data, intern_src, len * 2);
  intern_src += len * 2;
#endif
}

CAMLexport void deserialize_block_4(void * data, long len)
{
  unsigned char * p, * q;
#ifndef ARCH_BIG_ENDIAN
  for (p = intern_src, q = data; len > 0; len--, p += 4, q += 4)
    Reverse_32(q, p);
  intern_src = p;
#else
  memmove(data, intern_src, len * 4);
  intern_src += len * 4;
#endif
}

CAMLexport void deserialize_block_8(void * data, long len)
{
  unsigned char * p, * q;
#ifndef ARCH_BIG_ENDIAN
  for (p = intern_src, q = data; len > 0; len--, p += 8, q += 8)
    Reverse_64(q, p);
  intern_src = p;
#else
  memmove(data, intern_src, len * 8);
  intern_src += len * 8;
#endif
}

CAMLexport void deserialize_error(char * msg)
{
  intern_cleanup();
  failwith(msg);
}

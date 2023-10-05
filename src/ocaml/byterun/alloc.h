/*
**      ==================================
**      OOOO   OOOO OOOO  O      O   OOOO
**      O   O  O    O     O     O O  O   O
**      O   O  O    O     O     O O  O   O
**      OOOO   OOOO OOOO  O     OOO  OOOO
**      O   O     O    O  O    O   O O   O
**      O   O     O    O  O    O   O O   O
**      OOOO   OOOO OOOO  OOOO O   O OOOO
**      ==================================
**      BSSLAB, Dr. Stefan Bosse sci@bsslab.de
**
**    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF:
**    Free Software Foundation-Europe, GNU GPL License, Version 2
**
**    $MODIFIEDBY:  BSSLAB
**    $AUTHORS:     Xavier Leroy, Damien Doligez
**    $INITIAL:     Copyright 1996 INRIA
**    $CREATED:     2001.12.07
**    $VERSION:     1.01
**    $INFO:
**
**      1. Allocation functions doing the same work as the macros in the
**         case where [Setup_for_gc] and [Restore_after_gc] are no-ops.
**      2. Convenience functions related to allocation.
**
**    $ENDOFINFO
*/

#ifndef _alloc_
#define _alloc_


#include "misc.h"
#include "mlvalues.h"

CAMLextern value alloc (mlsize_t, tag_t);
CAMLextern value alloc_small (mlsize_t, tag_t);
CAMLextern value alloc_tuple (mlsize_t);
CAMLextern value alloc_string (mlsize_t);
CAMLextern value copy_string (char const *);
CAMLextern value copy_string_array (char const **);
CAMLextern value copy_double (double);
CAMLextern value copy_int32 (int32);       /* defined in [ints.c] */
CAMLextern value copy_int64 (int64);       /* defined in [ints.c] */
CAMLextern value copy_nativeint (long);    /* defined in [ints.c] */
CAMLextern value alloc_array (value (*funct) (char const *),
                              char const ** array);

typedef void (*final_fun)(value);
CAMLextern value alloc_final (mlsize_t /*size in words*/,
                              final_fun, /*finalization function*/
                              mlsize_t, /*resources consumed*/
                              mlsize_t  /*max resources*/);

CAMLextern int convert_flag_list (value, int *);

#endif /* _alloc_ */

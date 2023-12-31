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
**    $AUTHORS:     Manuel Serrano and Xavier Leroy
**    $INITIAL:     (C) 2000 INRIA
**    $CREATED:     2001.12.07
**    $MODIFIED:    2003.11.08
**    $VERSION:     1.01
**
**    $INFO:
**
**  Support for customized external C-ML code.
**  Support for abstract data types.
**
**    $ENDOFINFO
**
*/

#ifndef _custom_
#define _custom_


#include "mlvalues.h"

struct custom_operations {
    char *identifier;
    void (*finalize)(value v);
    int (*compare)(value v1, value v2);
    long (*hash)(value v);
    void (*serialize)(value v, 
                    /*out*/ unsigned long * wsize_32 /*size in bytes*/,
                    /*out*/ unsigned long * wsize_64 /*size in bytes*/);
    unsigned long (*deserialize)(void * dst);
};

#define custom_finalize_default NULL
CAMLextern int custom_compare_default(value v1, value v2);
#define custom_hash_default NULL
CAMLextern void custom_serialize_default(value v, unsigned long * wsize_32,
                                         unsigned long * wsize_64);
#define custom_deserialize_default NULL

#define Custom_ops_val(v) (*((struct custom_operations **) (v)))

CAMLextern value alloc_custom(struct custom_operations * ops,
                              unsigned long size, /*size in bytes*/
                              mlsize_t mem, /*resources consumed*/
                              mlsize_t max  /*max resources*/);

CAMLextern void register_custom_operations(struct custom_operations * ops);
extern struct custom_operations * find_custom_operations(char * ident);
extern struct custom_operations * final_custom_operations(void (*fn)(value));

extern void init_custom_operations(void);

#endif

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
**    $AUTHORS:     Xavier Leroy
**    $INITIAL:     (C) 1996 INRIA
**    $CREATED:     2001.12.07
**    $MODIFIED:    2003.11.08
**    $VERSION:     1.01
**
**    $INFO:
**
**  Callbacks from C to Caml 
**
**    $ENDOFINFO
**
*/

#ifndef _callback_
#define _callback_

#include "mlvalues.h"

CAMLextern value callback (value closure, value arg);
CAMLextern value callback2 (value closure, value arg1, value arg2);
CAMLextern value callback3 (value closure, value arg1, value arg2, value arg3);
CAMLextern value callbackN (value closure, int narg, value args[]);

CAMLextern value callback_exn (value closure, value arg);
CAMLextern value callback2_exn (value closure, value arg1, value arg2);
CAMLextern value callback3_exn (value closure,
                                value arg1, value arg2, value arg3);
CAMLextern value callbackN_exn (value closure, int narg, value args[]);

#define Make_exception_result(v) ((v) | 2)
#define Is_exception_result(v) (((v) & 3) == 2)
#define Extract_exception(v) ((v) & ~3)

CAMLextern char * format_caml_exception(value exn); /* in [printexc.c] */

CAMLextern value * caml_named_value (char * name);

CAMLextern void caml_main (char ** argv);
CAMLextern void caml_startup (char ** argv);

CAMLextern int callback_depth;

#endif

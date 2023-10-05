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
**    $INITIAL:     (C) 2000 INRIA
**    $CREATED:     2001.12.07
**    $MODIFIED:    2003.11.08
**    $VERSION:     1.13
**
**    $INFO:
**
**  Stack backtrace for uncaught exceptions 
**
**    $ENDOFINFO
**
*/


#ifndef _backtrace_
#define _backtrace_

#include "mlvalues.h"

CAMLextern int backtrace_active;
CAMLextern int backtrace_self;
CAMLextern int backtrace_verbose;
CAMLextern int backtrace_pos;
CAMLextern code_t * backtrace_buffer;
CAMLextern value backtrace_last_exn;

/*
** Debug event handler structure.
*/

struct event_info {
    code_t  pc_ev;
    int     line_pos;
    int     char_pos;
    int     stack_size;
};

struct event_module {
    char                *module_name;
    int                 evl_length;
    struct event_info   *evl;
};

/*
** The debug event table. One entry for one module.
*/

#define MAX_MODULES     1000
extern struct event_module event_table[];

/*
** Keep track of all code segments
*/

struct code_seg {
    char      *code_name;
    code_t    *pc_start;
    code_t    *pc_end;
    int       seg_num;
};

/*
** Code segment table. One entry for one module.
*/

extern struct code_seg code_table[];


/*
** Refresh backtrace table
*/
extern void init_backtrace(void);

/*
** Add an event to the backtrace table
*/
extern void stash_backtrace(value exn, code_t pc, value * sp);

/*
** Print current exception backtrace
*/
CAMLextern void print_exception_backtrace(void);

/*
** Search the event for the given PC.  Return Val_false if not found.
*/

struct event_module *event_for_location(code_t pc);

/*
** Read debug info for the current bytecode - independent of
** backtrace settings - needed for debugging
*/
void init_debuginfo();

#endif

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
**      BSSLAB, Dr. Stefan Bosse www.bsslab.de
**
**    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF: 
**    Free Software Foundation-Europe, GNU GPL License, Version 2
**
**    $MODIFIEDBY:  BSSLAB
**    $AUTHORS:     Xavier Leroy, Stefan Bosse
**    $INITIAL:     (C) 1996 INRIA
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.01
**
**    $INFO:
**
**
**
**    $ENDOFINFO
**
*/


#include <mlvalues.h>
#include <memory.h>
#include <fail.h>

#include "unixsupport.h"

#ifdef HAS_EXECVP

extern char ** cstringvect();
#ifndef _WIN32
extern char ** environ;
#endif

CAMLprim value unix_execvp(value path, value args)
{
  char ** argv;
  argv = cstringvect(args);
  (void) execvp(String_val(path), argv);
  stat_free((char *) argv);
  uerror("execvp", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                /* from smart compilers */
}

CAMLprim value unix_execvpe(value path, value args, value env)
{
  char ** argv;
  char ** saved_environ;
  argv = cstringvect(args);
  saved_environ = environ;
  environ = cstringvect(env);
  (void) execvp(String_val(path), argv);
  stat_free((char *) argv);
  stat_free((char *) environ);
  environ = saved_environ;
  uerror("execvp", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                /* from smart compilers */
}

#else 
CAMLprim value unix_execvp(value path, value args)
{
    invalid_argument("execvp not implemented");
}
CAMLprim value unix_execvpe(value path, value args)
{
    invalid_argument("execvpe not implemented");
}

#endif

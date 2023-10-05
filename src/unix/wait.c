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
#include <alloc.h>
#include <memory.h>
#include <signals.h>
#include <fail.h>

#include "unixsupport.h"


#ifdef HAS_WAIT


#include <sys/types.h>
#include <sys/wait.h>

#if !(defined(WIFEXITED) && defined(WEXITSTATUS) && defined(WIFSTOPPED) && \
      defined(WSTOPSIG) && defined(WTERMSIG))
/* Assume old-style V7 status word */
#define WIFEXITED(status) (((status) & 0xFF) == 0)
#define WEXITSTATUS(status) (((status) >> 8) & 0xFF)
#define WIFSTOPPED(status) (((status) & 0xFF) == 0xFF)
#define WSTOPSIG(status) (((status) >> 8) & 0xFF)
#define WTERMSIG(status) ((status) & 0x3F)
#endif

#define TAG_WEXITED 0
#define TAG_WSIGNALED 1
#define TAG_WSTOPPED 2

static value alloc_process_status(int pid, int status)
{
  value st, res;

  if (WIFEXITED(status)) {
    st = alloc_small(1, TAG_WEXITED);
    Field(st, 0) = Val_int(WEXITSTATUS(status));
  }
  else if (WIFSTOPPED(status)) {
    st = alloc_small(1, TAG_WSTOPPED);
    Field(st, 0) = Val_int(WSTOPSIG(status));
  }
  else {
    st = alloc_small(1, TAG_WSIGNALED);
    Field(st, 0) = Val_int(WTERMSIG(status));
  }
  Begin_root (st);
    res = alloc_small(2, 0);
    Field(res, 0) = Val_int(pid);
    Field(res, 1) = st;
  End_roots();
  return res;
}

CAMLprim value unix_wait(void)
{
  int pid, status;

  enter_blocking_section();
  pid = wait(&status);
  leave_blocking_section();
  if (pid == -1) uerror("wait", Nothing);
  return alloc_process_status(pid, status);
}

#else   /* !HAS_WAIT */
CAMLprim value unix_wait(void)
{
    invalid_argument("wait not implemented");
};
#endif


#if defined(HAS_WAITPID) || defined(HAS_WAIT4)

#ifndef HAS_WAITPID
#define waitpid(pid,status,opts) wait4(pid,status,opts,NULL)
#endif

static int wait_flag_table[] = {
  WNOHANG, WUNTRACED
};

CAMLprim value unix_waitpid(value flags, value pid_req)
{
  int pid, status;
  
  enter_blocking_section();
  pid = waitpid(Int_val(pid_req), &status, 
                convert_flag_list(flags, wait_flag_table));
  leave_blocking_section();
  if (pid == -1) uerror("waitpid", Nothing);
  return alloc_process_status(pid, status);
}

#else

CAMLprim value unix_waitpid(value flags, value pid_req)
{ invalid_argument("waitpid not implemented"); }

#endif

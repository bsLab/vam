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


#include <errno.h>
#include <signal.h>
#include <fail.h>

#include <alloc.h>
#include <memory.h>
#include <mlvalues.h>
#include <signals.h>
#include "unixsupport.h"

#ifndef NSIG
#define NSIG 32
#endif

#ifdef POSIX_SIGNALS

static void decode_sigset(value vset, sigset_t * set)
{
  sigemptyset(set);
  while (vset != Val_int(0)) {
    int sig = convert_signal_number(Int_val(Field(vset, 0)));
    sigaddset(set, sig);
    vset = Field(vset, 1);
  }
}

static value encode_sigset(sigset_t * set)
{
  value res = Val_int(0);
  int i;

  Begin_root(res)
    for (i = 1; i < NSIG; i++)
      if (sigismember(set, i)) {
        value newcons = alloc_small(2, 0);
        Field(newcons, 0) = Val_int(i);
        Field(newcons, 1) = res;
        res = newcons;
      }
  End_roots();
  return res;
}

static int sigprocmask_cmd[3] = { SIG_SETMASK, SIG_BLOCK, SIG_UNBLOCK };

CAMLprim value unix_sigprocmask(value vaction, value vset)
{
  int how;
  sigset_t set, oldset;
  int retcode;

  how = sigprocmask_cmd[Int_val(vaction)];
  decode_sigset(vset, &set);
  enter_blocking_section();
  retcode = sigprocmask(how, &set, &oldset);
  leave_blocking_section();
  if (retcode == -1) uerror("sigprocmask", Nothing);
  return encode_sigset(&oldset);
}

CAMLprim value unix_sigpending(value unit)
{
  sigset_t pending;
  if (sigpending(&pending) == -1) uerror("sigpending", Nothing);
  return encode_sigset(&pending);
}

CAMLprim value unix_sigsuspend(value vset)
{
  sigset_t set;
  int retcode;
  decode_sigset(vset, &set);
  enter_blocking_section();
  retcode = sigsuspend(&set);
  leave_blocking_section();
  if (retcode == -1 && errno != EINTR) uerror("sigsuspend", Nothing);
  return Val_unit;
}

#else

CAMLprim value unix_sigprocmask(value vaction, value vset)
{ invalid_argument("Unix.sigprocmask not available"); }

CAMLprim value unix_sigpending(value unit)
{ invalid_argument("Unix.sigpending not available"); }

CAMLprim value unix_sigsuspend(value vset)
{ invalid_argument("Unix.sigsuspend not available"); }

#endif

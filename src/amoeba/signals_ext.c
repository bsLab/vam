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
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2004 BSSLAB
**    $CREATED:     9.5.2004
**    $MODIFIED:    
**    $VERSION:     1.05
**
**    $INFO:
**
**  Amoeba signals wrapped to VAM. This is really a hack. We can't
**  call ML directly from a signal handler because the thread context
**  is not registered in ML. Therefore, a awiat/wakeup mechanism
**  is used. Each thread-signal pair has it's own signal handler
**  routine.
**
**    $ENDOFINFO
**
*/

/* #define DPRINTF_LEVEL 1 */

#include <amoeba.h>
#include <module/buffers.h>
#include <stderr.h>

#include <mlvalues.h>
#include <alloc.h>   
#include <callback.h>
#include <memory.h>  
#include <signals.h> 
#include <fail.h>    
#include <stdio.h>
#include <fcntl.h> 
#include <string.h>
#include <module/ar.h>
#include <sys/debug.h>

#include <thread.h>

extern errstat _sig_catch(
        int _sig,
        void (*catcher)(int _sig));

#define MAX_SIG_HANDLER 10

static value handler;
static int   sigfree=0;
static int   sigraise[MAX_SIG_HANDLER];

#define SIG_HANDLER(stnum)                                  \
    static void signal_catcher_##stnum  (int sig)           \
    {                                                       \
        value res;                                          \
        thread_wakeup((event)&sigraise[stnum]);             \
        return;                                             \
    };                                                          

SIG_HANDLER(0);
SIG_HANDLER(1);
SIG_HANDLER(2);
SIG_HANDLER(3);
SIG_HANDLER(4);
SIG_HANDLER(5);
SIG_HANDLER(6);
SIG_HANDLER(7);
SIG_HANDLER(8);
SIG_HANDLER(9);


CAMLexport value am_sig_catch(value vsig)
{
    CAMLparam0();
    
    int sig=Int_val(vsig);

    if (sigfree == MAX_SIG_HANDLER)
        failwith("am_sig_catch: too much signal handlers registered!");

    sigfree++;
    DPRINTF(1,("signal_catch %d (sigfree=%d)\n",sig,sigfree));

    switch (sigfree) {
        case 0: _sig_catch(sig,signal_catcher_0); break;
        case 1: _sig_catch(sig,signal_catcher_1); break;
        case 2: _sig_catch(sig,signal_catcher_2); break;
        case 3: _sig_catch(sig,signal_catcher_3); break;
        case 4: _sig_catch(sig,signal_catcher_4); break;
        case 5: _sig_catch(sig,signal_catcher_5); break;
        case 6: _sig_catch(sig,signal_catcher_6); break;
        case 7: _sig_catch(sig,signal_catcher_7); break;
        case 8: _sig_catch(sig,signal_catcher_8); break;
        case 9: _sig_catch(sig,signal_catcher_9); break;
        default: break;
        };
   
    CAMLreturn(Val_int(sigfree));
};

CAMLexport value am_sig_await(value u)
{
    CAMLparam0();
    int stnum=Int_val(u);

    enter_blocking_section();
      DPRINTF(1,("am_sig_await %d (tid %d)...\n",stnum,thread_id()));
      thread_await((event)&sigraise[stnum],0);
      DPRINTF(1,("... am_sig_await %d (tid %d)\n",stnum,thread_id()));
    leave_blocking_section();

    CAMLreturn(Val_int(0));
};

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
**    $INITIAL:     (C) 2003-2005
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.10
**
**    $INFO:
**
** Thread interface for native Amoeba threads 
**                                                                     
**        Native Amoeba threads for Ocaml                            
**                                                                     
** Uses the native Amoeba C thread interface:
**
**      thread_init ()
** int  thread_newthread(func,stsize,param,size)
** int  thread_exit()
** int  thread_id ()
** int  thread_await(ev,to)
** int  thread_await_lock(ev,to,mulock)
** void thread_wakeup(ev)
** void thread_switch()
**
**
**  Derived from posix.c
**
**
**
**    $ENDOFINFO
**
*/

/* #define DPRINTF_LEVEL   100 */
                                                                     
#include <mlvalues.h>



#include <amoeba.h>
#include <module/strmisc.h>
#include <errno.h>
#include <string.h>
#include <thread.h>
#include <module/mutex.h>
#include <sys/debug.h>

#include <stdlib.h>
#include <stdio.h>

#include <signal.h>
#include <sys/time.h>
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/io.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/signals.h"
#include "caml/backtrace.h"

#include "caml/stacks.h"
#include "caml/interp.h"

#include "caml/sys.h"

#ifdef HAS_STOPPROCESS
extern void stopprocess();
#endif

#define CAML_THREADS_MAX    200       /* should be enough for the first */
#define CAML_THREAD_STKSIZE    64000

/*
** Initial size of stack when a thread is created (32 Ko) 
*/
#define Thread_stack_size (Stack_size*2)


/*
** The ML value describing a thread (heap-allocated) 
*/

struct caml_thread_descr {
  value ident;                  /* Unique integer ID */
  value start_closure;          /* The closure to start this thread */
  value terminated;             /* Mutex held while the thread is running */
};

#define Ident(v) (((struct caml_thread_descr *)(v))->ident)
#define Start_closure(v) (((struct caml_thread_descr *)(v))->start_closure)
#define Terminated(v) (((struct caml_thread_descr *)(v))->terminated)

/*
** The infos on threads (allocated via malloc()) 
*/

struct caml_thread_struct {
  value descr;                  /* The heap-allocated descriptor */
  struct caml_thread_struct * next;  /* Double linking of running threads */
  struct caml_thread_struct * prev;

  value * stack_low;            /* The execution stack for this thread */
  value * stack_high;
  value * stack_threshold;
  value * sp;                   /* Saved value of extern_sp for this thread */
  code_t  pc;                   /* Saved extern_pc value */
  value * trapsp;               /* Saved value of trapsp for this thread */
  struct caml__roots_block * local_roots; /* Saved value of local_roots */
  struct longjmp_buffer * external_raise; /* Saved external_raise */
  struct channel *chann;        /* thread io channels */
};

typedef struct caml_thread_struct * caml_thread_t;

/*
** The descriptor for the currently executing thread 
*/

static caml_thread_t curr_thread = NULL;

/*
** Table of all caml thread structures. Index = thread_id()
*/


static caml_thread_t caml_thread_table[CAML_THREADS_MAX];

/*
** The global mutex used to ensure that at most one thread is running
** Caml code 
*/
static mutex caml_mutex;


/*
** Identifier for next thread creation 
*/
static long thread_next_ident = 0;


/*
** Forward declarations 
*/

value caml_threadstatus_new (void);
void caml_threadstatus_terminate (value);
int caml_threadstatus_wait (value);
static void caml_thread_check (int, char *);

/*
** Hook for scanning the stacks of the other threads 
*/

static void (*prev_scan_roots_hook) (scanning_action);

static void caml_thread_scan_roots(scanning_action action)
{
  caml_thread_t th;

  th = curr_thread;
  do {
    (*action)(th->descr, &th->descr);

    /*
    ** Don't rescan the stack of the current thread, it was done already 
    */
    if (th != curr_thread) 
    {
      do_local_roots(action, th->sp, th->stack_high, th->local_roots);
    }
    th = th->next;
  } while (th != curr_thread);
  /* Hook */
  if (prev_scan_roots_hook != NULL) (*prev_scan_roots_hook)(action);
}

/* Hooks for enter_blocking_section and leave_blocking_section */

static void (*prev_enter_blocking_section_hook) () = NULL;
static void (*prev_leave_blocking_section_hook) () = NULL;

static void caml_thread_enter_blocking_section(void)
{
  if (prev_enter_blocking_section_hook != NULL)
    (*prev_enter_blocking_section_hook)();

  /*
  ** Save the stack-related global variables in the thread descriptor
  ** of the current thread 
  */

  curr_thread->stack_low = stack_low;
  curr_thread->stack_high = stack_high;
  curr_thread->stack_threshold = stack_threshold;
  curr_thread->sp = extern_sp;
  curr_thread->pc = extern_pc;
  curr_thread->trapsp = trapsp;
  curr_thread->local_roots = local_roots;
  curr_thread->external_raise = external_raise;

  /* Release the global mutex */
  mu_unlock(&caml_mutex);
}

static void caml_thread_leave_blocking_section(void)
{

  /*
  ** Re-acquire the global mutex 
  */
  mu_lock(&caml_mutex);


  /*
  ** Update curr_thread to point to the thread descriptor corresponding
  ** to the thread currently executing 
  */

  curr_thread = caml_thread_table[thread_id()];

  /*
  ** Restore the stack-related global variables 
  */

  stack_low = curr_thread->stack_low;
  stack_high = curr_thread->stack_high;
  stack_threshold = curr_thread->stack_threshold;
  extern_sp = curr_thread->sp;
  curr_thread->pc=NULL;
  trapsp = curr_thread->trapsp;
  local_roots = curr_thread->local_roots;
  external_raise = curr_thread->external_raise;

  if (prev_leave_blocking_section_hook != NULL)
    (*prev_leave_blocking_section_hook)();
}

/*
** Hooks for I/O locking 
*/

static void caml_io_mutex_free(struct channel *chan)
{
  mutex *mutex_c = chan->mutex;

  if (mutex_c != NULL) 
  {
    stat_free((char *) mutex_c);
  }
}

static void caml_io_mutex_lock(struct channel *chann)
{
#ifndef AMOEBA_RAW
  int myid=thread_id();
#else
  int myid=thread_kid();
#endif

  if (chann->mutex == NULL) {
    mutex *mutex_c =
      (mutex *) stat_alloc(sizeof(mutex));
    mu_init(mutex_c);
    chann->mutex = (void *) mutex_c;
  }

  DPRINTF(1,("io_mu_lock: myid=%d mu=%x *mu=%x\n",myid,
                    chann->mutex,
                    *((mutex *)chann->mutex)));

  if (  (MU_OTID((*((mutex *)chann->mutex)))==myid && 
        (MU_ISSEMA(*(mutex *)chann->mutex))==0) )
    failwith("caml_io_mutex_lock: programming error");
  
  enter_blocking_section();

  mu_lock((mutex *)chann->mutex);
  /* 
  ** Problem: if a signal occurs at this point,
  ** and the signal handler raises an exception, we will not
  ** unlock the mutex.  The alternative (doing the setspecific
  ** before locking the mutex is also incorrect, since we could
  ** then unlock a mutex that is unlocked or locked by someone else. 
  */
  
  {
    int tid=thread_id();
    caml_thread_table[tid]->chann = chann;
  }

  leave_blocking_section();

}

static void caml_io_mutex_unlock(struct channel *chan)
{

    DPRINTF(1,("io_mu_unlock: myid=%d mu=%x *mu=%x\n",thread_kid(),
                    chan->mutex,
                    *((mutex *)chan->mutex)));

  if (*((mutex *)(chan->mutex)) == 0)
    failwith("caml_io_mutex_unlock: programming error");

  mu_unlock(chan->mutex);

  {
    int tid=thread_id();
    caml_thread_table[tid]->chann = NULL;
  }
}

static void caml_io_mutex_unlock_exn(void)
{

  
  int tid=thread_id();
  struct channel * chan = caml_thread_table[tid]->chann;
  

  if (chan != NULL) caml_io_mutex_unlock(chan);

}




static void stopthreads()
{
#ifdef HAS_STOPPROCESS
    stopprocess(0);
#endif
};

/*
** Initialize the thread machinery 
*/

value caml_thread_initialize(value unit)   /* ML */
{
  int tid;
  

  value mu = Val_unit;
  value descr;


  Begin_root (mu);
    /*
    ** Initialize the main mutex 
    */
    mu_init(&caml_mutex);
    mu_lock(&caml_mutex);


    /*
    ** Create and initialize the termination semaphore 
    */
    mu = caml_threadstatus_new();

    /*
    ** Create a descriptor for the current thread 
    */

    descr = alloc_small(3, 0);

    /* the first thread id */
    tid = thread_id();
    Ident(descr) = Val_long(tid);

    Start_closure(descr) = Val_unit;
    Terminated(descr) = mu;
    thread_next_ident++;
    /*
    ** Create an info block for the current thread 
    */
    curr_thread =
      (caml_thread_t) stat_alloc(sizeof(struct caml_thread_struct));
    curr_thread->descr = descr;
    curr_thread->next = curr_thread;
    curr_thread->prev = curr_thread;

    /*
    ** The stack-related fields will be filled in at the next
    ** enter_blocking_section 
    */
    /*
    ** Associate the thread descriptor with the thread 
    */

      
    if(tid < CAML_THREADS_MAX && tid >= 0) 
    {


        caml_thread_table[tid]=curr_thread;
        caml_thread_table[tid]->chann=NULL;
    }
    else
        caml_thread_check(0,"caml_thread_initialize");
        
    /*
    ** Set up the hooks 
    */

    prev_scan_roots_hook = scan_roots_hook;
    scan_roots_hook = caml_thread_scan_roots;
    prev_enter_blocking_section_hook = enter_blocking_section_hook;
    enter_blocking_section_hook = caml_thread_enter_blocking_section;
    prev_leave_blocking_section_hook = leave_blocking_section_hook;
    leave_blocking_section_hook = caml_thread_leave_blocking_section;
    channel_mutex_free = caml_io_mutex_free;
    channel_mutex_lock = caml_io_mutex_lock;
    channel_mutex_unlock = caml_io_mutex_unlock;
    channel_mutex_unlock_exn = caml_io_mutex_unlock_exn;


  End_roots();

  atexit(stopthreads);

  return Val_unit;
}

/*
** Create a thread 
*/

static void  caml_thread_start(long arg)
{
  
  int tid=thread_id();

  caml_thread_t th = (caml_thread_t)arg;  
  value clos;


  if(tid < CAML_THREADS_MAX && tid >= 0)
  { 
      caml_thread_table[tid] = th;
      caml_thread_table[tid]->chann = NULL;
  }
  else
      caml_thread_check(0,"caml_thread_start");

  /*
  ** Acquire the global mutex and set up the stack variables 
  */
  leave_blocking_section();


  Begin_roots1(th->descr);

    /*
    ** Callback the closure 
    */
    clos = Start_closure(th->descr);
    modify(&(Start_closure(th->descr)), Val_unit);
    callback_exn(clos, Val_unit);

  End_roots();

  /*
  ** Signal that the thread has terminated 
  */

  caml_threadstatus_terminate(Terminated(th->descr));

  /*
  ** Remove th from the doubly-linked list of threads 
  */
  th->next->prev = th->prev;
  th->prev->next = th->next;

  /*
  ** Release the main mutex (forever) 
  */
  mu_unlock(&caml_mutex);

  /*
  ** Free the memory resources 
  */
  stat_free(th->stack_low);

  /*
  ** Free the thread descriptor 
  */
  stat_free(th);

  /*
  ** The thread now stops running 
  */


  thread_exit();
}  

value caml_thread_new(value clos)          /* ML */
{
  CAMLparam0();
  CAMLlocal1(descr);

  caml_thread_t th;
  value mu = Val_unit;
/*  value descr; */
  int err;
  int tid;
  long arg;
  
  Begin_roots2 (clos, mu)
    /*
    ** Create and initialize the termination semaphore 
    */
    mu = caml_threadstatus_new();

    /*
    ** Create a descriptor for the new thread 
    */
    descr = alloc_small(3, 0);
    Start_closure(descr) = clos;
    Terminated(descr) = mu;
    thread_next_ident++;

    /*
    ** Create an info block for the current thread 
    */
    th = (caml_thread_t) stat_alloc(sizeof(struct caml_thread_struct));
    th->descr = descr;

    /*
    ** Allocate the stacks 
    */
    th->stack_low = (value *) stat_alloc(Thread_stack_size);
    th->stack_high = th->stack_low + Thread_stack_size / sizeof(value);
    th->stack_threshold = th->stack_low + Stack_threshold / sizeof(value);
    th->sp = th->stack_high;
    th->pc = NULL;
    th->trapsp = th->stack_high;
    th->local_roots = NULL;
    th->external_raise = NULL;

    /*
    ** Add thread info block to the list of threads 
    */
    th->next = curr_thread->next;
    th->prev = curr_thread;
    curr_thread->next->prev = th;
    curr_thread->next = th;

    /*
    ** Fork the new thread 
    */

    DPRINTF(1,("thread_create\n"));    
    tid = thread_create(caml_thread_start,
                        (long)th,
                        CAML_THREAD_STKSIZE,NORM);


    if (tid < 0) 
    {
      /*
      ** Fork failed, remove thread info block from list of threads 
      */

      th->next->prev = curr_thread;
      curr_thread->next = th->next;

      stat_free(th->stack_low);

      stat_free(th);
      caml_thread_check(err, "Thread.create");
    };


    Ident(descr) = Val_long(tid);

  End_roots();
/*  return descr; */
  CAMLreturn(descr);

}

/*
** Return the current thread 
*/

value caml_thread_self(value unit)         /* ML */
{
  if (curr_thread == NULL) invalid_argument("Thread.self: not initialized");
  return curr_thread->descr;
}

/*
** Return the identifier of a thread 
*/

value caml_thread_id(value th)          /* ML */
{
  return Ident(th);
}

/*
** Allow re-scheduling 
*/

value caml_thread_yield(value unit)        /* ML */
{
  enter_blocking_section();
  thread_switch();
  leave_blocking_section();
  return Val_unit;
}

/*
** Suspend the current thread until another thread terminates 
*/

value caml_thread_join(value th)          /* ML */
{
#if TODO
  int retcode = caml_threadstatus_wait(Terminated(th));
  caml_pthread_check(retcode, "Thread.join");
#endif

  return Val_unit;
}

/*
** Mutex operations 
*/

#define Mutex_val(v) (* ((mutex **) Data_custom_val(v)))
#define Event_val(v) (* ((event **) Data_custom_val(v)))
#define Max_mutex_number 1000
#define Max_event_number 1000

static void caml_mutex_finalize(value wrapper)
{
  mutex * mut = Mutex_val(wrapper);
  stat_free(mut);
}

static int caml_mutex_condition_compare(value wrapper1, value wrapper2)
{
  mutex * mut1 = Mutex_val(wrapper1);
  mutex * mut2 = Mutex_val(wrapper2);
  return mut1 == mut2 ? 0 : mut1 < mut2 ? -1 : 1;
}

static struct custom_operations caml_mutex_ops = {
  "_mutex",
  caml_mutex_finalize,
  caml_mutex_condition_compare,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

value caml_mutex_new(value unit)        /* ML */
{
  mutex * mut;
  value wrapper;
  mut = stat_alloc(sizeof(mutex));

  mu_init(mut);

  wrapper = alloc_custom(&caml_mutex_ops, sizeof(mutex *),
                         1, Max_mutex_number);
  Mutex_val(wrapper) = mut;
  return wrapper;
}

value caml_mutex_lock(value wrapper)     /* ML */
{
#ifndef AMOEBA_RAW
  int myid=thread_id();
#else
  int myid=thread_kid();
#endif

  int retcode;
  mutex * mut = Mutex_val(wrapper);

  if (MU_OTID(*mut)==myid && MU_ISSEMA(*mut)==0)
    failwith("caml_mutex_lock: programming error");

  Begin_root(wrapper)           /* prevent the deallocation of mutex */
    enter_blocking_section();
    mu_lock(mut);
    leave_blocking_section();
  End_roots();

  return Val_unit;
}

value caml_mutex_unlock(value wrapper)           /* ML */
{
  int retcode;
  mutex * mut = Mutex_val(wrapper);

  if (*mut == 0)
    failwith("caml_mutex_unlock: programming error");

  Begin_root(wrapper)           /* prevent the deallocation of mutex */
    enter_blocking_section();
    mu_unlock(mut);
    leave_blocking_section();
  End_roots();

  return Val_unit;
}

value caml_mutex_try_lock(value wrapper)           /* ML */
{
  int retcode;
  mutex * mut = Mutex_val(wrapper);

  enter_blocking_section();
  retcode = mu_trylock(mut,(interval)0);
  leave_blocking_section();

  if (retcode < 0) 
    return Val_false;
  else
    return Val_true;
}


/*
** Thread synchronisation
*/

value caml_event_new(value unit)        /* ML */
{
  event * ev;
  value wrapper;
  ev = stat_alloc(sizeof(event));

  wrapper = alloc_custom(&caml_mutex_ops, sizeof(event *),
                         1, Max_event_number);
  Event_val(wrapper) = ev;
  return wrapper;
}

value caml_thread_await(value wrapper,value vtmo)     /* ML */
{
  int retcode;
  event *ev = Event_val(wrapper);
  interval tmo = Int_val(vtmo);
  int ret;

  Begin_root(wrapper)           /* prevent the deallocation of the event */
    enter_blocking_section();
    ret=thread_await((event)ev,tmo);
    leave_blocking_section();
  End_roots();

    if (ret<0)
        return Val_false;
    else
        return Val_true;
}

value caml_thread_await_lock(value vev,value vtmo, value vmut)     /* ML */
{
  int retcode;
  event *ev = Event_val(vev);
  mutex * mut = Mutex_val(vmut);
  interval tmo = Int_val(vtmo);
  int ret;

  Begin_roots2(vev,vmut)           /* prevent the deallocation of the event */
    enter_blocking_section();
    ret=thread_await_lock((event)ev,tmo,mut);
    leave_blocking_section();
  End_roots();

  if (ret < 0)
    return Val_false;
  else
    return Val_true;
}


value caml_event_wakeup(value wrapper)           /* ML */
{
  event *ev = Event_val(wrapper);
  Begin_root(wrapper)           /* prevent the deallocation of the event */
    enter_blocking_section();
    thread_wakeup((event)ev);
    leave_blocking_section();
  End_roots();

  return Val_unit;
}



/*
** Thread status blocks 
*/

struct caml_threadstatus {
  mutex lock;                           /* mutex for mutual exclusion */
  enum { ALIVE, TERMINATED } status;    /* status of thread */
  event terminated;                     /* signaled when thread terminates */
};

#define Threadstatus_val(v) \
  (* ((struct caml_threadstatus **) Data_custom_val(v)))
#define Max_threadstatus_number 500

static void caml_threadstatus_finalize(value wrapper)
{
  struct caml_threadstatus * ts = Threadstatus_val(wrapper);
  stat_free(ts);
}

static struct custom_operations caml_threadstatus_ops = {
  "_threadstatus",
  caml_threadstatus_finalize,
  caml_mutex_condition_compare,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

value caml_threadstatus_new (void)
{
  struct caml_threadstatus * ts;
  value wrapper;
  ts = stat_alloc(sizeof(struct caml_threadstatus));

  mu_init(&ts->lock);

  ts->status = ALIVE;
  wrapper = alloc_custom(&caml_threadstatus_ops,
                         sizeof(struct caml_threadstatus *),
                         1, Max_threadstatus_number);
  Threadstatus_val(wrapper) = ts;
  return wrapper;
}

void caml_threadstatus_terminate (value wrapper)
{
  struct caml_threadstatus * ts = Threadstatus_val(wrapper);
  mu_lock(&ts->lock);
  ts->status = TERMINATED;
  mu_unlock(&ts->lock);

  enter_blocking_section();
  thread_wakeup((event)&ts->terminated);
  leave_blocking_section();

}

int caml_threadstatus_wait (value wrapper)
{
  struct caml_threadstatus * ts = Threadstatus_val(wrapper);
  int retcode=0;

  Begin_roots1(wrapper)         /* prevent deallocation of ts */
    enter_blocking_section();
    mu_lock(&ts->lock);
    while (ts->status != TERMINATED) 
    {
      mu_unlock(&ts->lock);
      thread_await((event)&ts->terminated,(interval)-1);
      mu_lock(&ts->lock);
    };
    mu_unlock(&ts->lock);
    leave_blocking_section();
  End_roots();

  return retcode;
}

/*
** Synchronous signal wait 
*/

value caml_wait_signal(value sigs) /* ML */
{
  sigset_t set;
  int retcode, signo;

#if TODO
  sigemptyset(&set);
  while (sigs != Val_int(0)) {
    int sig = convert_signal_number(Int_val(Field(sigs, 0)));
    sigaddset(&set, sig);
    sigs = Field(sigs, 1);
  }
  enter_blocking_section();
  retcode = sigwait(&set, &signo);
  leave_blocking_section();
  caml_thread_check(retcode, "Thread.wait_signal");
#endif

  return Val_int(signo);
}

/*
** Suspend current thread for ##vdel*vunit seconds.
*/

value
caml_thread_unit_delay (value vdel, value vunit)
{
    int del = Int_val(vdel);
    int uni = Int_val(vunit);
    int ret;

    enter_blocking_section();
    ret=thread_delay(del,uni);
    leave_blocking_section();

    if (ret<0)
        return Val_false;
    else
        return Val_true;
}




/* Error report */

static void caml_thread_check(int retcode, char *msg)
{
  char * err;
  int errlen, msglen;
  value str;

  if (retcode == 0) return;
  err = strerror(retcode);
  msglen = strlen(msg);
  errlen = strlen(err);
  str = alloc_string(msglen + 2 + errlen);
  memmove (&Byte(str, 0), msg, msglen);
  memmove (&Byte(str, msglen), ": ", 2);
  memmove (&Byte(str, msglen + 2), err, errlen);
  raise_sys_error(str);
}


/*
** Thread frame infos
*/

static char * print_thread_frame(caml_thread_t tp,
                                 char *buf,char *end,
                                 int debug_debug)
{
    int i;
    struct event_module *ev=NULL;
    code_t sp=tp->sp;
    code_t pc=tp->pc;
    value env=*sp;
    char *p=buf;

    p=bprintf(p,end,"  STACK: HIGH=0x%x LOW=0x%x\n",
                (int)tp->stack_high,
                (int)tp->stack_low);
    p=bprintf(p,end,"  REG: SP=0x%x PC=0x%x\n",
                (int)tp->sp,
                (int)tp->pc);


    if (debug_debug && env)
        p=bprintf(p,end,"env: %x %x %x %x\n",
            (int)Field(env,1),
            (int)Field(env,2),
            (int)Field(env,3),
            (int)Field(env,4));


    if (pc)
    {

        ev=event_for_location(pc);
        if (ev)
        {
            p=bprintf(p,end,
                    "    Entry Module %20s   Line=%5d   Pos=%3d   Stacksize=%3d\n",
                    ev->module_name,ev->evl->line_pos,
                    ev->evl->char_pos,
                    ev->evl->stack_size);
        };
    };

    if (ev==NULL)
    {
        ev=NULL;
        while(sp <= tp->stack_high && ev==NULL)
        {
            ev=event_for_location((code_t)*sp);
            if (debug_debug)
                p=bprintf (p,end,"sp=%x *sp=%x\n",(int)sp,(int)*sp);

            if (ev)
            {
                p=bprintf(p,end,
                    "    Find Module  %20s   Line=%5d   Pos=%3d   Stacksize=%3d\n",
                        ev->module_name,
                        ev->evl->line_pos,
                        ev->evl->char_pos,
                        ev->evl->stack_size);
                sp=sp+3+ev->evl->stack_size;
            }
            else
                sp++;
        };
    }
    else
        sp = sp + ev->evl->stack_size + 1;

    while(sp <= tp->stack_high && ev!=NULL)
    {
        ev=event_for_location((code_t)*sp);

        if (debug_debug)
            p=bprintf (p,end,"sp=%x *sp=%x\n",(int)sp,(int)*sp);

        if (ev)
        {
            p=bprintf(p,end,
                    "    Module       %20s   Line=%5d   Pos=%3d   Stacksize=%3d\n",
                    ev->module_name,ev->evl->line_pos,
                    ev->evl->char_pos,
                    ev->evl->stack_size);
            sp=sp+3+ev->evl->stack_size;
        };
    };
    return p;
};

value caml_thread_debuginfo(value v)         /* ML */
{
    CAMLparam0();
    CAMLlocal1 (vstr);
    char *buf,*end;
    char *p;
    int len=0;
    int debug_debug=Int_val(v);

    caml_thread_t tp=curr_thread;

    init_debuginfo();

    /*
    ** Allocate temp. char buffer
    */
    buf=malloc(30001);
    if (buf==NULL)
        failwith("thread_debuginfo: can't allocate temp buffer\n");

    end=&buf[30000];
    p=buf;
    
    p=bprintf(p,end,
            "Current Thread %d >>>>\n",(int)Long_val(Ident(tp->descr)));

    if (tp) 
    {
        tp->pc=extern_pc;
        p=print_thread_frame(tp,p,end,debug_debug);
        tp->pc=NULL;
    };
    if (tp)
    for(tp=tp->next;
        tp != curr_thread &&
         tp != NULL;
        tp = tp->next)
    {
        p=bprintf(p,end,
            "Other Thread %d >>>>\n",(int)Long_val(Ident(tp->descr)));
        p=print_thread_frame(tp,p,end,debug_debug);
    };

    if (p)
    {
        len=(int)(p-buf);
        
    }
    else
    {
        len=strlen(buf);
    };

    vstr=alloc_string(len);
    strncpy(String_val(vstr),buf,len);
    free(buf);

    CAMLreturn(vstr);
};


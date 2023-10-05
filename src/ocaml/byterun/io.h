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
**    $VERSION:     1.03
**
**    $INFO:
**
**  Buffered input/output
**
**    $ENDOFINFO
**
*/



#ifndef _io_
#define _io_

#include "misc.h"
#include "mlvalues.h"

#ifndef IO_BUFFER_SIZE
#define IO_BUFFER_SIZE 4096
#endif

struct channel {
  int fd;                       /* Unix file descriptor */
  long offset;                  /* Absolute position of fd in the file */
  char * end;                   /* Physical end of the buffer */
  char * curr;                  /* Current position in the buffer */
  char * max;                   /* Logical end of the buffer (for input) */
  void * mutex;                 /* Placeholder for mutex (for systhreads) */
  struct channel * next;        /* Linear chaining of channels (flush_all) */
  int revealed;                 /* For Cash only */
  int old_revealed;             /* For Cash only */
  int refcount;                 /* For flush_all and for Cash */
  char buff[IO_BUFFER_SIZE];    /* The buffer itself */
};

/*
** For an output channel:
**   [offset] is the absolute position of the beginning of the buffer [buff].
** For an input channel:
**   [offset] is the absolute position of the logical end of the buffer, [max].
*/

/*
** Functions and macros that can be called from C.  Take arguments of
** type struct channel *.  No locking is performed. 
*/

#define putch(channel, ch) do{                                            \
  if ((channel)->curr >= (channel)->end) flush_partial(channel);          \
  *((channel)->curr)++ = (ch);                                            \
}while(0)

#define getch(channel)                                                      \
  ((channel)->curr >= (channel)->max                                        \
   ? refill(channel)                                                        \
   : (unsigned char) *((channel))->curr++)

CAMLextern struct channel * open_descriptor_in (int);
CAMLextern struct channel * open_descriptor_out (int);
CAMLextern void close_channel (struct channel *);
CAMLextern int channel_binary_mode (struct channel *);

CAMLextern int flush_partial (struct channel *);
CAMLextern void flush (struct channel *);
CAMLextern void putword (struct channel *, uint32);
CAMLextern int putblock (struct channel *, char *, long);
CAMLextern void really_putblock (struct channel *, char *, long);

CAMLextern unsigned char refill (struct channel *);
CAMLextern uint32 getword (struct channel *);
CAMLextern int getblock (struct channel *, char *, long);
CAMLextern int really_getblock (struct channel *, char *, long);

/*
** Extract a struct channel * from the heap object representing it 
*/

#define Channel(v) (*((struct channel **) (Data_custom_val(v))))

/*
** The locking machinery 
*/

CAMLextern void (*channel_mutex_free) (struct channel *);
CAMLextern void (*channel_mutex_lock) (struct channel *);
CAMLextern void (*channel_mutex_unlock) (struct channel *);
CAMLextern void (*channel_mutex_unlock_exn) (void);

#define Lock(channel) \
  if (channel_mutex_lock!=NULL)(*channel_mutex_lock)(channel);
#define Unlock(channel) \
  if (channel_mutex_unlock != NULL) (*channel_mutex_unlock)(channel)
#define Unlock_exn() \
  if (channel_mutex_unlock_exn != NULL) (*channel_mutex_unlock_exn)()

/*
** Standard I/O (terminal)
** The following methods are supported:
** buffer only, buffer + callback function, Amoeba or Unix.
*/

#ifdef __STDC__
#include "stdarg.h"
#define VA_LIST         va_list
#define VA_ALIST
#define VA_DCL
#define VA_START( ap, last )    va_start( ap, last )
#define VA_END          va_end
#define VA_ARG          va_arg
#else
#include "varargs.h"
#define VA_LIST         va_list
#define VA_ALIST        , va_alist
#define VA_DCL          va_dcl
#define VA_START( ap, last )    va_start( ap )
#define VA_END          va_end
#define VA_ARG          va_arg
#endif  /* __STDC__ */

char    *buf_printf  (char *begin, char *end, char *fmt, ...);
char    *print   (char *begin, char *end, char *fmt, VA_LIST args);


/*
** Internal Standard I/O (terminal) -> stdout,stderr channels
**
** The following methods are supported:
**  1. send directly to the terminal, 
**  2. buffer + callback function, 
**  Amoeba or Unix IO.
**
** Callback functions with the interface
**
**  val f : string -> unit = <fun> 
**
**  can be installed with the following ML code:
**
**      external set_stdout_callback: string -> unit =
**             "caml_set_stdout_callback" ;;
**      external set_stderr_callback: string -> unit =
**             "caml_set_stderr_callback" ;;
**      let f str = (do something here with the print string) ;;
**      Callback.register "f" f;;
**      set_stdout_callback "f";;
**
**  Each time, the VM or ML code emits output to the STDOUT or STDERR
**  output channels, these callback functions will be called with the
**  output string. 
**
*/

/*CAMLextern int chan_std_in();*/
CAMLextern int print_stdout   (char *_format, ...);
CAMLextern int print_stderr   (char *_format, ...);


#endif /* _io_ */

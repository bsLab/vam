
/*
** get prototypes or declarations for string and stdlib functions and deal
** with missing functions like strchr. 
*/

/*
** we will assume the ANSI/Posix/whatever situation as default.
** the header file is called string.h and the index functions are called
** strchr, strrchr. Exceptions have to be listed explicitly 
*/

#ifndef STDFN_H
#define STDFN_H


#include "config.h"

#include <ctype.h>
#include <signal.h>


#include <stdio.h>
#include <setjmp.h>

#ifdef HAVE_PROTO
  #define __PROTO(a)    a
#endif

#ifdef HAS_UNISTD
  #define HAVE_UNISTD_H
#endif
#ifdef HAS_POPEN
  #define HAVE_POPEN
#endif
#ifdef HAS_STRNCASECMP
  #define HAVE_STRNCASECMP
#endif


#define alloc(a,b)      malloc(a)
#define ralloc(a,b,c)  realloc(a,b)

#define int_error(a,b)  {printf("%s\n",a);return;}



#ifdef sequent
#define NO_STRCHR
#endif

#ifndef NO_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

#ifdef NO_STRCHR
#ifdef strchr
#undef strchr
#endif
#define strchr index
#ifdef strrchr
#undef strrchr
#endif
#define strrchr rindex
#endif

#ifdef NO_STDLIB_H
char *malloc();
char *realloc();
char *getenv();
int system();
double atof();
int atoi();
long atol();
double strtod();
#else
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#else
#ifdef HAVE_LIBC_H /* NeXT uses libc instead of unistd */
#include <libc.h>
#endif
#endif

#ifndef NO_ERRNO_H
#include <errno.h>
#endif
#ifdef EXTERN_ERRNO
extern int errno;
#endif

#ifndef NO_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifndef NO_LIMITS_H
#include <limits.h>
#else
#ifdef HAVE_VALUES_H
#include <values.h>
#endif
#endif

#ifdef NEED_TIME_T
#define time_t unsigned long
#endif

#include <time.h> /* ctime etc, should also define time_t and struct tm */

#ifndef HAVE_POPEN
  FILE *popen(char *cmd, char *mode);
  int pclose(FILE *pipe);
#endif

#ifndef NO_LOCALE_H
  #include <locale.h>
#endif

#ifndef HAVE_STRNICMP
#  ifdef HAVE_STRNCASECMP
#    define strnicmp strncasecmp
#  else
#    define NEED_STRNICMP
int strnicmp __PROTO((char *s1, char *s2, int n));
#  endif
#endif


#ifndef GP_GETCWD
#  if defined(HAVE_GETCWD)
#   define GP_GETCWD(path,len) getcwd (path, len)
#  else
#   define GP_GETCWD(path,len) getwd (path)
#  endif
#endif

#define NO_CARET (-1)


#ifdef LINUX /* HBB: to get prototype for ioctl() */
  #include <sys/ioctl.h>
#endif

#ifdef HAVE_TERMIOS_H
# ifdef HAVE_TCGETATTR
#  define TERMIOS
# endif
#else
# ifdef HAVE_SGTTY_H
#  define SGTTY
# endif
#endif


/*
** Set up structures using the proper include file
*/
#if defined(_IBMR2) || defined(alliant)
  #define SGTTY
#endif

/*
**  submitted by Francois.Dagorn@cicb.fr 
*/
#ifdef SGTTY
  #include <sgtty.h>
  static struct sgttyb orig_termio, rl_termio;
  /*
  ** define terminal control characters 
  */
  static struct tchars s_tchars;
  #ifndef VERASE
    #define VERASE    0
  #endif
  #ifndef VEOF
    #define VEOF      1
  #endif
  #ifndef VKILL
    #define VKILL     2
  #endif
  #ifdef TIOCGLTC		 
  /*
  ** available only with the 'new' line discipline 
  */
    static struct ltchars s_ltchars;
    #ifndef VWERASE
      #define VWERASE   3
    #endif
    #ifndef VREPRINT
      #define VREPRINT  4
    #endif
    #ifndef VSUSP
      #define VSUSP     5
    #endif
  #endif /* TIOCGLTC */

  #ifndef NCCS
    #define NCCS      6
  #endif

#else /* SGTTY */

/*
** SIGTSTP defines job control 
** if there is job control then we need termios.h instead of termio.h 
** (Are there any systems with job control that use termio.h?  I hope not.) 
*/

  #if defined(SIGTSTP) || defined(TERMIOS)
    #ifndef TERMIOS
      #define TERMIOS
    #endif
    #include <termios.h>
    /*
    ** Added by Robert Eckardt, RobertE@beta.TP2.Ruhr-Uni-Bochum.de 
    */
    #ifdef ISC22
      #ifndef ONOCR			/* taken from sys/termio.h */
        #define ONOCR 0000020	/* true at least for ISC 2.2 */
      #endif 
      #ifndef IUCLC
        #define IUCLC 0001000
      #endif
    #endif /* ISC22 */
    #if !defined(IUCLC)
      #define IUCLC 0 /* translate upper to lower case not supported */
    #endif

    static struct termios orig_termio, rl_termio;
  #else
    #include <termio.h>
    static struct termio orig_termio, rl_termio;
    /*
    ** termio defines NCC instead of NCCS 
    */
    #define NCCS    NCC
  #endif /* SIGTSTP */
#endif /* SGTTY */

/*
** ULTRIX defines VRPRNT instead of VREPRINT 
*/
#if defined(VRPRNT) && !defined(VREPRINT)
  #define VREPRINT VRPRNT
#endif


#endif /* STDFN_H */

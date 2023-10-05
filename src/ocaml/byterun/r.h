#include <ctype.h>
#include <signal.h>

#include "readline.h"

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

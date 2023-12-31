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
**    $AUTHORS:     ?, SB
**    $INITIAL:     (C) 1994 Vrije Universiteit, The Netherlands.
**    $CREATED:     ?
**    $VERSION:     1.03
**
**    $INFO:
**
**  Buffer print(f) compatbile functions.
**
**    $ENDOFINFO
**
*/


#include "alloc.h"
#include "custom.h"
#include "fail.h"  
#include "io.h"    
#include <string.h>

#ifdef AMOEBA
#include <amoeba.h>
#include <module/ar.h>
#endif


#define MAXWIDTH        32

#define between(c,l,u)  ((unsigned) ((c) - (l)) <= (unsigned) ((u) - (l)))
#define isdigit(c)      between(c, '0', '9')
#define islower(c)      between(c, 'a', 'z')
#define isupper(c)      between(c, 'A', 'Z')
#define toupper(c)      ((c) + 'A' - 'a')


static char hexdec_upper[] = "0123456789ABCDEF";
static char hexdec_lower[] = "0123456789abcdef";

/*
** the following is a table of the characters which should be printd
** when a particular value for a character is requested to be printed
** by printchar.
*/
#define BEL             0x07    /* bell */
#define BS              0x08    /* back space */
#define NL              0x0a    /* new line */
#define CR              0x0d    /* carriage return */
#define ESC             0x1b    /* escape */

static char     displayable[256] =
{
'?', '?', '?', '?', '?', '?', '?', BEL, BS , '?', NL , '?', '?', CR , '?', '?',
'?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', ESC, '?', '?', '?', '?',
' ', '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/',
'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?',
'@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '[', '\\', ']', '^', '_',
'`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '{', '|', '}', '~', '?',
'?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?',
'?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?',
'?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?',
'?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?',
'?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?',
'?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?',
'?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?',
'?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?'
};



/*
** BPRINTF
**      This is rather like sprintf except that it takes pointers to the
**      beginning and end of the buffer where it must put the formatted
**      string.  It returns when either the buffer is full or when the
**      format string is copied.  The string in the buffer is not null
**      terminated!
**      It returns a pointer to the end of the formatted string in the buffer
**      If either of the buffer pointers are null it writes on standard output.
**      So if your output unexpectedly appears on the console look for a null
**      pointer.
*/

/*VARARGS3*/
char *buf_printf(char *begin, char *end, char *fmt, ...)
{
    char *      print();
    char *rc;
    VA_LIST args;

    VA_START( args, fmt );

    rc = print(begin, end, fmt, args);
    /* If safe, null terminate */
    if (rc && rc < end)
        *rc = '\0';

    VA_END( args );
    return( rc );
}


/*
** PUT_CHAR
**      Put a character in the specified buffer and return a pointer to the
**      next free place in the buffer.
**      However:
**              If the buffer is full, change nothing.
**              If there, is no buffer, write it on the console.
*/

static char *
put_char(p, end, c)
char *  p;
char *  end;
char    c;
{
    if (p == 0)
    {
        return 0;
    }
    if (p >= end)
        return p;
    *p++ = c;
    return p;
}


/*
** PRINTNUM
**      Format a number in a the chosen base
*/

static char *
printnum(begin, end, n, base, sign, width, pad, hexdec)
char *  begin;
char *  end;
long    n;
int     base;
int     sign;
int     width;
int     pad;
char *  hexdec;
{
    register short      i;
    register short      mod;
    char                a[MAXWIDTH];
    register char *     p = a;

    if (sign)
    {
        if (n < 0)
        {
            n = -n;
            width--;
        }
        else 
        sign = 0;
    }

    do
    {           /* mod = n % base; n /= base */
        mod = 0;
        for (i = 0; i < 32; i++) {
                mod <<= 1;
                if (n < 0) mod++;
                n <<= 1;
                if (mod >= base) {
                        mod -= base;
                        n++;
                }
        }
        *p++ = hexdec[mod];
        width--;
    } while (n);

    while (width-- > 0)
        begin = put_char(begin, end, pad);

    if (sign)
        *p++ = '-';

    while (p > a)
        begin = put_char(begin, end, *--p);
    return begin;
}


/*
** DOPRNT
**      This routine converts variables to printable ascii strings for printf
**      and bprintf.  The first two parameters point to the beginning and end
**      (respectively) of a buffer to hold the result of the formatting.
**      If either of the buffer pointers is null the output is directed to the
**      system console.  If the buffer is there it will stop formatting at the
**      end of the buffer and return
*/

/*VARARGS3*/
char *
print(begin, end, fmt, argp)
char *                  begin;  /* pointer to buffer where to put output */
char *                  end;    /* end of buffer to receive output */
register char *         fmt;    /* format of output */
VA_LIST                 argp;   /* arguments to format string */
{
    register char *     s;
    register char       c;
    register short      width;
    register short      pad;
    register short      n;

/*
** Make sure that we don't have a defective buffer pointer!  We write on the
** console if begin is the null-pointer.
*/
    if (end == 0)
        begin = 0;

/*
** Process the format string, doing conversions where required
** note: we ignore the left justify flag.  Numbers are always right justified
** and everything else is left justified.
*/
    for ( ; *fmt != 0; fmt++)
        if (*fmt == '%')
        {
            if (*++fmt == '-')
                fmt++;
            pad = *fmt == '0' ? '0' : ' ';
            width = 0;
            while (isdigit(*fmt))
            {
                width *= 10;
                width += *fmt++ - '0';
            }
            if (*fmt == 'l' && islower(*++fmt) && *fmt != 'x')
                c = toupper(*fmt);
            else
                c = *fmt;
            switch (c)
            {
            case 'c':
            case 'C':
                begin = put_char(begin, end,
                                     displayable[ VA_ARG( argp, int )]);
                width--;
                break;
            case 'b':
                begin = printnum(begin, end, (long) VA_ARG( argp, unsigned ),
                                 2, 0, width, pad, hexdec_lower);
                width = 0;
                break;
            case 'B':
                begin = printnum(begin, end, VA_ARG( argp, long ),
                                 2, 0, width, pad, hexdec_lower);
                width = 0;
                break;
            case 'o':
                begin = printnum(begin, end, (long) VA_ARG( argp, unsigned ),
                                 8, 0, width, pad, hexdec_lower);
                width = 0;
                break;
            case 'O':
                begin = printnum(begin, end, VA_ARG( argp, long ),
                                 8, 0, width, pad, hexdec_lower);
                width = 0;
                break;
            case 'd':
                begin = printnum(begin, end, (long) VA_ARG( argp, int ),
                                 10, 1, width, pad, hexdec_lower);
                width = 0;
                break;
            case 'D':
                begin = printnum(begin, end, VA_ARG( argp, long ),
                                 10, 1, width, pad, hexdec_lower);
                width = 0;
                break;
            case 'u':
                begin = printnum(begin, end, (long) VA_ARG( argp, unsigned ),
                                 10, 0, width, pad, hexdec_lower);
                width = 0;
                break;
            case 'U':
                begin = printnum(begin, end, VA_ARG( argp, long ),
                                 10, 0, width, pad, hexdec_lower);
                width = 0;
                break;
            case 'x':
                begin = printnum(begin, end, (long) VA_ARG( argp, unsigned ),
                                 16, 0, width, pad, hexdec_lower);
                width = 0;
                break;
            case 'X':
                begin = printnum(begin, end, (long) VA_ARG( argp, unsigned ),
                                 16, 0, width, pad, hexdec_upper);
                width = 0;
                break;
#ifdef AMOEBA
            case 'p':
            case 'P':           /* print a port in ascii format */
                s = (char *) VA_ARG( argp, port **);
                n = PORTSIZE;
                if (s == NULL)  /* bad boy */
                {
#if 0
                        s= "(null port)";
                        n= strlen(s);
#endif
                        int i;
                        s= "(null port)";
                        for(i=0;i<n;i++)
                                *s='\0';
                }
                do {
#if 0
                    if (*s > ' ')
                        begin = put_char(begin, end, displayable[(int)*s]);
                    else
                        begin = put_char(begin, end, '?');
#endif
                    begin = printnum(begin, end, (unsigned int)(unsigned char)*s,
                                 16, 0, width, pad, hexdec_lower);
                                                
                    width--;
                    if(n>1)
                    {
                        begin = put_char(begin, end, ':');
                        width--;
                    }
                    s++;
                } while (--n);
                break;
#endif  /* AMOEBA */     
            case 's':
            case 'S': 
                s = VA_ARG( argp, char *);
                while (*s)
                {
                    begin = put_char(begin, end, displayable[(int)*s++]);
                    width--;
                }
                break;
            }
            while (width-- > 0)
                begin = put_char(begin, end, pad);
        }
        else
            begin = put_char(begin, end, *fmt);

    return begin;
}

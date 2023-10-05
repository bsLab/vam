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
**    $AUTHORS:     
**    $INITIAL:     (C) 1986 - 1993, 1996   Thomas Williams, Colin Kelley
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.02
**
**    $INFO:
**
** NANO-EMACS line editing facility 
** printable characters print as themselves (insert not overwrite) 
** ^A moves to the beginning of the line 
** ^B moves back a single character 
** ^E moves to the end of the line 
** ^F moves forward a single character 
** ^K kills from current position to the end of line 
** ^P moves back through history 
** ^N moves forward through history 
** ^H and DEL delete the previous character 
** ^D deletes the current character, or EOF if line is empty 
** ^L/^R redraw line in case it gets trashed 
** ^U kills the entire line 
** ^W kills last word 
** LF and CR return the entire line regardless of the cursor postition 
** EOF with an empty line returns (char *)NULL 
**
** all other characters are ignored 
**
**
**    $ENDOFINFO
**
*/


/*
** Copyright (C) 1986 - 1993, 1996   Thomas Williams, Colin Kelley
**
** Permission to use, copy, and distribute this software and its
** documentation for any purpose with or without fee is hereby granted, 
** provided that the above copyright notice appear in all copies and 
** that both that copyright notice and this permission notice appear 
** in supporting documentation.
**
** Permission to modify the software is granted, but not the right to
** distribute the modified code.  Modifications are to be distributed 
** as patches to released version.
**  
** This software is provided "as is" without express or implied warranty.
** 
**
** AUTHORS
**
**   Original Software:
**     Tom Tkacik
**
**   Msdos port and some enhancements:
**     Gershon Elber and many others.
**
**   Some more changes:
**     Stefan Bosse     
** 
*/



/* 
** A small portable version of GNU's readline 
** This is not the BASH or GNU EMACS version of READLINE.
** Do not need any terminal capabilities except backspace, 
** and space overwrites a character.
*/


#include "readline.h"
#include "signals.h"

/*
** define characters to use with our input character handler 
*/
static char term_chars[NCCS];

static int term_set = 0;	/* =1 if rl_termio set */

#define special_getc() ansi_getc()
static int ansi_getc __PROTO((void));



#define MAXBUF      1024	/* initial size and increment of input line length */
#define BACKSPACE   0x08	/* ^H */
#define SPACE       ' '
#define NEWLINE     '\n'

struct hist {
	char *line;
	struct hist *prev;
	struct hist *next;
};

static struct hist *history = NULL;  /* no history yet */
static struct hist *cur_entry = NULL;

static char *cur_line;  /* current contents of the line */
static int line_len=0;
static int cur_pos = 0;	/* current position of the cursor */
static int max_pos = 0;	/* maximum character position */


static void fix_line __PROTO((void));
static void redraw_line __PROTO((char *prompt));
static void clear_line __PROTO((char *prompt));
static void clear_eoline __PROTO((void));
static void copy_line __PROTO((char *line));
static void set_termio __PROTO((void));
static void reset_termio __PROTO((void));
static int ansi_getc __PROTO((void));
static char user_getc __PROTO((void));
static int user_putc __PROTO((int ch));
static int user_puts __PROTO((char *str));
static void backspace __PROTO((void));
static void extend_cur_line __PROTO((void));

/*
** user_putc and user_puts should be used in the place of
** fputc(ch,stderr) and fputs(str,stderr) for all output
** of user typed characters.  
*/

static char
user_getc()
{
	int rv;
    char my_c;
    enter_blocking_section();
      rv = read(STDOUT_FILENO,&my_c,1);
    leave_blocking_section ();
	return my_c;
}

static int
user_putc(ch)
int ch;
{
	int rv;
#if 0
	rv = fputc(ch, stderr);
#endif
    enter_blocking_section();
      rv = write(STDOUT_FILENO,&ch,1);
    leave_blocking_section ();
	return rv;
}

static int
user_puts(str)
char *str;
{
	int rv;
#if 0
	rv = fputs(str, stderr);
#endif
    enter_blocking_section();
      rv = write(STDOUT_FILENO,str,strlen(str));
    leave_blocking_section ();
	return rv;
}

/*
** This function provides a centralized non-destructive backspace capability 
**
** M. Castro 
*/

static void
backspace()
{
	user_putc(BACKSPACE);
}

static void extend_cur_line()
{
  char *new_line;

  /*
  ** extent input line length 
  */
  new_line=ralloc(cur_line, line_len+MAXBUF, NULL);
  if(!new_line) {
    reset_termio();
    int_error("Can't extend readline length", NO_CARET);
  }
  cur_line=new_line;
  line_len+=MAXBUF;


#ifdef DEBUG_STR
  fprintf(stderr, "\nextending readline length to %d chars\n", line_len);
#endif
}

char *
caml_readline(prompt)
char *prompt;
{

	int cur_char;
	char *new_line;

	/* start with a string of MAXBUF chars */
	
	if(line_len!=0) {
		free(cur_line);
		line_len=0;
	}

	cur_line=alloc((unsigned long)MAXBUF, "readline");
	line_len=MAXBUF;

	/*
	** set the termio so we can do our own input processing 
	*/
	set_termio();

	/* print the prompt */

	user_puts(prompt);

	cur_line[0] = '\0';
	cur_pos = 0;
	max_pos = 0;
	cur_entry = NULL;

	/* get characters */
	for(;;) {
		cur_char = special_getc();
    /*
    ** The #define CHARSET7BIT should be used when one encounters problems with
    ** 8bit characters that should not be entered on the commandline. I cannot
    ** think on any reasonable example where this could happen, but what do I know?
    ** After all, the unix world still ignores 8bit chars in most applications.
    **
    ** Note that latin1 defines the chars 0x80-0x9f as control chars. For the
    ** benefit of Atari, MSDOS, Windows and NeXT I have decided to ignore this,
    ** since it would require more #ifs.
    **
    */

#ifdef CHARSET7BIT
		if(isprint(cur_char)) {
#else  /* CHARSET7BIT */
		if(isprint(cur_char) || (((unsigned char)cur_char > 0x7f) &&
					 cur_char != EOF)) {
#endif /* CHARSET7BIT */
			int i;

			if(max_pos+1>=line_len) {
				extend_cur_line();
			}

			for(i=max_pos; i>cur_pos; i--) {
				cur_line[i] = cur_line[i-1];
			}
			user_putc(cur_char);
			cur_line[cur_pos] = cur_char;
			cur_pos += 1;
			max_pos += 1;
			if (cur_pos < max_pos)
			    fix_line();
			cur_line[max_pos] = '\0';

		/* else interpret unix terminal driver characters */
#ifdef VERASE
		} else if(cur_char == term_chars[VERASE] ){  /* DEL? */
			if(cur_pos > 0) {
				int i;
				cur_pos -= 1;
				backspace();
				for(i=cur_pos; i<max_pos; i++)
					cur_line[i] = cur_line[i+1];
				max_pos -= 1;
				fix_line();
			}
#endif /* VERASE */
#if defined(VEOF) 
		} else if(cur_char == term_chars[VEOF] ){   /* ^D? */
			if(max_pos == 0) {
				reset_termio();
				return((char *)NULL);
			}
			if((cur_pos < max_pos)&&(cur_char == 004)) { /* ^D */
				int i;
				for(i=cur_pos; i<max_pos; i++)
					cur_line[i] = cur_line[i+1];
				max_pos -= 1;
				fix_line();
			}
#endif /* VEOF */
#ifdef VKILL
		} else if(cur_char == term_chars[VKILL] ){  /* ^U? */
			clear_line(prompt);
#endif /* VKILL */
#ifdef VWERASE
		} else if(cur_char == term_chars[VWERASE] ){  /* ^W? */
			while((cur_pos > 0) &&
			      (cur_line[cur_pos-1] == SPACE)) {
				cur_pos -= 1;
				backspace();
			}
			while((cur_pos > 0) &&
			      (cur_line[cur_pos-1] != SPACE)) {
				cur_pos -= 1;
				backspace();
			}
			clear_eoline();
			max_pos = cur_pos;
#endif /* VWERASE */
#ifdef VREPRINT
		} else if(cur_char == term_chars[VREPRINT] ){  /* ^R? */
			user_putc(NEWLINE); /* go to a fresh line */
			redraw_line(prompt);
#endif /* VREPRINT */
#ifdef VSUSP
		} else if(cur_char == term_chars[VSUSP]) {
			reset_termio();

#ifndef AMOEBA_RAW
			kill(0, SIGTSTP);
#endif
			/* process stops here */

			set_termio();
			/* print the prompt */
			redraw_line(prompt);
#endif /* VSUSP */
		} else {
			/* do normal editing commands */
			/* some of these are also done above */
			int i;
			switch(cur_char) {
			    case EOF:
				reset_termio();
				return((char *)NULL);
			    case 001: /* ^A */
				while(cur_pos > 0) {
					cur_pos -= 1;
					backspace();
				}
				break;
			    case 002: /* ^B */
				if(cur_pos > 0) {
					cur_pos -= 1;
					backspace();
				}
				break;
			    case 005: /* ^E */
				while(cur_pos < max_pos) {
					user_putc(cur_line[cur_pos]);
					cur_pos += 1;
				}
				break;
			    case 006: /* ^F */
				if(cur_pos < max_pos) {
					user_putc(cur_line[cur_pos]);
					cur_pos += 1;
				}
				break;
			    case 013: /* ^K */
				clear_eoline();
				max_pos = cur_pos;
				break;
			    case 020: /* ^P */
				if(history != NULL) {
					if(cur_entry == NULL) {
						cur_entry = history;
						clear_line(prompt);
						copy_line(cur_entry->line);
					} else if(cur_entry->prev != NULL) {
						cur_entry = cur_entry->prev;
						clear_line(prompt);
						copy_line(cur_entry->line);
					}
				}
				break;
			    case 016: /* ^N */
				if(cur_entry != NULL) {
					cur_entry = cur_entry->next;
					clear_line(prompt);
					if(cur_entry != NULL) 
						copy_line(cur_entry->line);
					else
						cur_pos = max_pos = 0;
				}
				break;
			    case 014: /* ^L */
			    case 022: /* ^R */
				user_putc(NEWLINE); /* go to a fresh line */
				redraw_line(prompt);
				break;
			    case 0177: /* DEL */
			    case 010: /* ^H */
				if(cur_pos > 0) {
					cur_pos -= 1;
					backspace();
					for(i=cur_pos; i<max_pos; i++)
						cur_line[i] = cur_line[i+1];
					max_pos -= 1;
					fix_line();
				}
				break;
			    case 004: /* ^D */
				if(max_pos == 0) {
					reset_termio();
					return((char *)NULL);
				}
				if(cur_pos < max_pos) {
					for(i=cur_pos; i<max_pos; i++)
						cur_line[i] = cur_line[i+1];
					max_pos -= 1;
					fix_line();
				}
				break;
			    case 025:  /* ^U */
				clear_line(prompt);
				break;
			    case 027:  /* ^W */
				while((cur_pos > 0) &&
				      (cur_line[cur_pos-1] == SPACE)) {
					cur_pos -= 1;
					backspace();
				}
				while((cur_pos > 0) &&
				      (cur_line[cur_pos-1] != SPACE)) {
					cur_pos -= 1;
					backspace();
				}
				clear_eoline();
				max_pos = cur_pos;
				break;
			    case '\n': /* ^J */
				cur_line[max_pos+1] = '\0';

				user_putc(NEWLINE);
				new_line = (char *)ralloc(cur_line, (unsigned long)(strlen(cur_line)+1), "line resize");
				line_len=0;
#ifdef DEBUG_STR
				fprintf(stderr, "Resizing input line to %d chars\n", strlen(new_line));
#endif
				reset_termio();
				return(new_line);
			    default:
				break;
			}
		}
	}
}

/*
** fix up the line from cur_pos to max_pos 
** do not need any terminal capabilities except backspace, 
** and space overwrites a character 
*/
static void
fix_line()
{
	int i;

	/* write tail of string */
	for(i=cur_pos; i<max_pos; i++)
		user_putc(cur_line[i]);

	/* write a space at the end of the line in case we deleted one */
	user_putc(SPACE);

	/* backup to original position */
	for(i=max_pos+1; i>cur_pos; i--)
		backspace();

}

/*
** redraw the entire line, putting the cursor where it belongs 
*/
static void
redraw_line(prompt)
char *prompt;
{
	int i;

	user_puts(prompt);
	user_puts(cur_line);

	/* put the cursor where it belongs */
	for(i=max_pos; i>cur_pos; i--)
		backspace();
}

/*
** clear cur_line and the screen line 
*/
static void
clear_line(prompt)
char *prompt;
{
	int i;
	for(i=0; i<max_pos; i++)
		cur_line[i] = '\0';

	for(i=cur_pos; i>0; i--)
		backspace();

	for(i=0; i<max_pos; i++)
		user_putc(SPACE);

	user_putc('\r');
	user_puts(prompt);

	cur_pos = 0;
	max_pos = 0;
}

/*
** clear to end of line and the screen end of line 
*/
static void
clear_eoline()
{
	int i;
	for(i=cur_pos; i<max_pos; i++)
		cur_line[i] = '\0';

	for(i=cur_pos; i<max_pos; i++)
		user_putc(SPACE);
	for(i=cur_pos; i<max_pos; i++)
		backspace();
}

/*
** copy line to cur_line, draw it and set cur_pos and max_pos 
*/
static void
copy_line(line)
char *line;
{
	while(strlen(line)+1>line_len) {
		extend_cur_line();
	}
	strcpy(cur_line, line);
	user_puts(cur_line);
	cur_pos = max_pos = strlen(cur_line);
}

/*
** add line to the history 
*/
void
caml_add_history(line)
char *line;
{
	struct hist *entry;
	entry = (struct hist *)alloc((unsigned long)sizeof(struct hist),"history");
	entry->line = alloc((unsigned long)(strlen(line)+1),"history");
	strcpy(entry->line, line);

	entry->prev = history;
	entry->next = NULL;
	if(history != NULL) {
		history->next = entry;
	}
	history = entry;
}


/*
** Convert ANSI arrow keys to control characters 
*/

static int
ansi_getc()
{
 
  int c;   
  
  fflush(stdout);
  fflush(stderr);

  c = user_getc();

#ifdef AMOEBA
	if(c==-1)
		return 4 /* ^D */;	
#endif

  if (c == 033) {

            
    c = user_getc(); /* check for CSI */
    if (c == '[') {


      c = user_getc(); /* get command character */

      switch (c) {
      case 'D': /* left arrow key */
	    c = 002;
    	break;
      case 'C': /* right arrow key */
	    c = 006;
    	break;
      case 'A': /* up arrow key */
	    c = 020;
    	break;
      case 'B': /* down arrow key */
	    c = 016;
    	break;
      }
    }
  }
  return c;
}


/*
** set termio so we can do our own input processing 
*/
static int restore_on_exit=0;

static void
set_termio()
{
#if !defined(AMOEBA) 
/*
** set termio so we can do our own input processing 
** and save the old terminal modes so we can reset them later 
*/
	if(term_set == 0) 
    {
        if (restore_on_exit==0)
        {
            /*
            ** On exit make sure that the original terminal 
            ** state is restored
            */
            atexit(reset_termio);
            restore_on_exit=1;
        };

		/*
		 * Get terminal modes.
		 */
#ifdef SGTTY
		ioctl(0, TIOCGETP, &orig_termio);
#else  /* SGTTY */
#ifdef TERMIOS
#ifdef TCGETS
		ioctl(0, TCGETS, &orig_termio);
#else
		tcgetattr(0, &orig_termio);
#endif /* TCGETS */
#else
		ioctl(0, TCGETA, &orig_termio);
#endif /* TERMIOS */
#endif /* SGTTY */

		/*
		 * Save terminal modes
		 */
		rl_termio = orig_termio;

		/*
		 * Set the modes to the way we want them
		 *  and save our input special characters
		 */
#ifdef SGTTY
		rl_termio.sg_flags |= CBREAK;
		rl_termio.sg_flags &= ~(ECHO|XTABS);
		ioctl(0, TIOCSETN, &rl_termio);

		ioctl(0, TIOCGETC, &s_tchars);
		term_chars[VERASE]   = orig_termio.sg_erase;
		term_chars[VEOF]     = s_tchars.t_eofc;
		term_chars[VKILL]    = orig_termio.sg_kill;
#ifdef TIOCGLTC
		ioctl(0, TIOCGLTC, &s_ltchars);
		term_chars[VWERASE]  = s_ltchars.t_werasc;
		term_chars[VREPRINT] = s_ltchars.t_rprntc;
		term_chars[VSUSP]    = s_ltchars.t_suspc;

		/* disable suspending process on ^Z */
		s_ltchars.t_suspc = 0;
		ioctl(0, TIOCSLTC, &s_ltchars);
#endif /* TIOCGLTC */
#else  /* SGTTY */
		rl_termio.c_iflag &= ~(BRKINT|PARMRK|INPCK|IUCLC|IXON|IXOFF);
		rl_termio.c_iflag |=  (IGNBRK|IGNPAR);
		/* rl_termio.c_oflag &= ~(ONOCR); Costas Sphocleous Irvine,CA */

		rl_termio.c_lflag &= ~(ICANON|ECHO|ECHOE|ECHOK|ECHONL|NOFLSH);

		rl_termio.c_lflag |=  (ISIG);
		rl_termio.c_cc[VMIN] = 1;
		rl_termio.c_cc[VTIME] = 0;

#ifndef VWERASE
#define VWERASE 3
#endif
		term_chars[VERASE]   = orig_termio.c_cc[VERASE];
		term_chars[VEOF]     = orig_termio.c_cc[VEOF];
		term_chars[VKILL]    = orig_termio.c_cc[VKILL];
#ifdef TERMIOS
		term_chars[VWERASE]  = orig_termio.c_cc[VWERASE];
#ifdef VREPRINT
		term_chars[VREPRINT] = orig_termio.c_cc[VREPRINT];
#else
#ifdef VRPRNT
		term_chars[VRPRNT] = orig_termio.c_cc[VRPRNT];
#endif
#endif
		term_chars[VSUSP]    = orig_termio.c_cc[VSUSP];

		/* disable suspending process on ^Z */
		rl_termio.c_cc[VSUSP] = 0;
#endif /* TERMIOS */
#endif /* SGTTY */

		/*
		 * Set the new terminal modes.
		 */
#ifdef SGTTY
		ioctl(0, TIOCSLTC, &s_ltchars);
#else
#ifdef TERMIOS
#ifdef TCSETSW
		ioctl(0, TCSETSW, &rl_termio);
#else
		tcsetattr(0, TCSADRAIN, &rl_termio);
#endif /* TCSETSW */
#else
		ioctl(0, TCSETAW, &rl_termio);
#endif /* TERMIOS */
#endif /* SGTTY */
		term_set = 1;
	}

#endif /* !AMOEBA */

#ifdef AMOEBA
	tcgetattr(0,&orig_termio);
	rl_termio=orig_termio;
	rl_termio.c_lflag &= ~(ICANON|ECHO|ECHOE|ECHOK|ECHONL|NOFLSH);
	tcsetattr(0,TCSANOW,&rl_termio);
	term_set=1;
#endif

}
  
static void
reset_termio()
{

    /*
    ** reset saved terminal modes 
    */
	if(term_set == 1) {
#ifdef SGTTY
		ioctl(0, TIOCSETN, &orig_termio);
#ifdef TIOCGLTC
		/* enable suspending process on ^Z */
		s_ltchars.t_suspc = term_chars[VSUSP];
		ioctl(0, TIOCSLTC, &s_ltchars);
#endif /* TIOCGLTC */
#else  /* SGTTY */
#ifdef TERMIOS
#ifdef TCSETSW
		ioctl(0, TCSETSW, &orig_termio);
#else
#ifndef AMOEBA
		tcsetattr(0, TCSADRAIN, &orig_termio);
#else
		tcsetattr(0, TCSANOW, &orig_termio);

#endif
#endif /* TCSETSW */
#else
		ioctl(0, TCSETAW, &orig_termio);
#endif /* TERMIOS */
#endif /* SGTTY */

		term_set = 0;
	}
}

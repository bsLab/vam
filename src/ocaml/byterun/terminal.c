/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/*
** Changes: (sbosse@physik.uni-bremen.de)
**
**  - changed standout mode to bold mode if available
**
** Last modified: 20/08/2002
*/


/*
** Read and output terminal commands, termianl control. 
*/

#include "config.h"
#include "alloc.h"
#include "fail.h"
#include "io.h"
#include "mlvalues.h"
#include "memory.h"

extern char    *caml_readline();
extern void     caml_add_history();

#define Uninitialised (Val_int(0))
#define Bad_term (Val_int(1))
#define Good_term_tag 0

#if defined (HAS_TERMCAP) && !defined (NATIVE_CODE)

extern int tgetent (char * buffer, char * name);
extern char * tgetstr (char * id, char ** area);
extern int tgetnum (char * id);
extern int tputs (char * str, int count, int (*outchar)(int c));

static struct channel *chan;
static char area [1024];
static char *area_p = area;
static int num_lines;
static char *up = NULL;
static char *down = NULL;
static char *standout = NULL;
static char *standend = NULL;

CAMLprim value terminfo_setup (value vchan)
{
  value result;
  static char buffer[1024];

  chan = Channel (vchan);

  if (tgetent(buffer, getenv("TERM")) != 1) return Bad_term;

  num_lines = tgetnum ("li");
  up = tgetstr ("up", &area_p);
  down = tgetstr ("do", &area_p);
  standout = tgetstr ("md", &area_p);   /* original us */
  standend = tgetstr ("me", &area_p);   /* original ue */
  if (standout == NULL || standend == NULL){
    standout = tgetstr ("so", &area_p);    
    standend = tgetstr ("se", &area_p);     
  }
  Assert (area_p <= area + 1024);
  if (num_lines == -1 || up == NULL || down == NULL
      || standout == NULL || standend == NULL){
    return Bad_term;
  }
  result = alloc_small (1, Good_term_tag);
  Field (result, 0) = Val_int (num_lines);
  return result;
}

static int terminfo_putc (int c)
{
  putch (chan, c);
  return c;
}

CAMLprim value terminfo_backup (value lines)
{
  int i;

  for (i = 0; i < Int_val (lines); i++){
    tputs (up, 1, terminfo_putc);
  }
  return Val_unit;
}

CAMLprim value terminfo_standout (value start)
{
  tputs (Bool_val (start) ? standout : standend, 1, terminfo_putc);
  return Val_unit;
}

CAMLprim value terminfo_resume (value lines)
{
  int i;

  for (i = 0; i < Int_val (lines); i++){
    tputs (down, 1, terminfo_putc);
  }
  return Val_unit;
}

#else /* defined (HAS_TERMCAP) && !defined (NATIVE_CODE) */

CAMLexport value terminfo_setup (value vchan)
{
  return Bad_term;
}

CAMLexport value terminfo_backup (value lines)
{
  invalid_argument("Terminfo.backup");
  return Val_unit;
}

CAMLexport value terminfo_standout (value start)
{
  invalid_argument("Terminfo.standout");
  return Val_unit;
}

CAMLexport value terminfo_resume (value lines)
{
  invalid_argument("Terminfo.resume");
  return Val_unit;
}

#endif /* defined (HAS_TERMCAP) && !defined (NATIVE_CODE) */


static char *terminal_line;

#define EOFACTION   "exit 0;;"
#define NEWLINE     "\n"

/*
** Readline support. See readline.c for details.
*/

CAMLexport value terminal_readline(value prompt)
{
    
    terminal_line=caml_readline(String_val(prompt));

    if(terminal_line==NULL)
        return copy_string(EOFACTION);

    if(strlen(terminal_line)==0)
        return copy_string(NEWLINE);    

    caml_add_history(terminal_line);
    
    return(copy_string(terminal_line));
}
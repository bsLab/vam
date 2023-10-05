/* 
** This file is part of the FIREBALL AMOEBA System.
**
**
** Last modified:
**      14/12/01
**
** Changes: (Stefan Bosse)
**      -- ERROR number changed
**
**
**
** FIREBALL AMOEBA is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public License as 
** published by the Free Software Foundation; version 2.
**
** The FIREBALL AMOEBA is distributed in the hope that it will be usefull,
** but WITHOUT ANY WARRANTY; without even implied warranty of 
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
** General Public License for more details.
**
** Original Copyright: Vrije Universiteit, The Netherlands.
*/ 


/*
** Errors are errors: only 1 allowed
*/

#define MAX_ERRORS	1
#define MAX_LEX_ERRORS	1

#if __STDC__
#define PROTO(x) x
#else
#define PROTO(x) ()
#endif

EXTERN int ErrorsOccurred;

PUBLIC void SetBadExitStatus	PROTO(( void ));
PUBLIC int GetExitStatus	PROTO(( void ));
PUBLIC void Exit		PROTO(( void ));

/*
 * Number of times an error-reporting function has been called
 */

EXTERN void LexError		PROTO(( char *format, ... ));
EXTERN void FatalError		PROTO(( char *format, ... ));
EXTERN void ParseError		PROTO(( char *format, ... ));
EXTERN void RunTimeError	PROTO(( char *format, ... ));
EXTERN void SemanticError	PROTO(( char *format, ... ));
EXTERN void Verbose		PROTO(( int level, char *format, ... ));

/*
 * When more than MAX_LEX_ERRORS lexical errors are reported a
 * FatalError terminates the program, because presumably a non-Amake
 * source file is being read.
 * The same happens when more than MAX_ERRORS errors are reported.
 */

EXTERN void SemanticIdError	PROTO(( struct idf *idp, char *fmt, ... ));
/* Write a message iff idp is not a generated error-identifier */
EXTERN void Message		PROTO(( char *format, ... ));
EXTERN void SafeMessage		PROTO(( char *long_string ));
EXTERN void PrintError		PROTO(( char *format, ... ));
EXTERN void Warning		PROTO(( char *format, ... ));
EXTERN void FuncError		PROTO(( struct expr *call, char *fmt, ... ));

/* Write a newline on stderr */
EXTERN void NewLine		PROTO(( void ));

EXTERN struct idf *ErrorId	PROTO(( void ));
/* Create new identifier and deliver a pointer to it. The name is of
 * the form _err<n>, so this shouldn't conflict with user-supplied names.
 */

EXTERN void CaseError		PROTO(( char *format, int num ));
/* Call when default case in switch shouldn't be reached */

#undef PROTO

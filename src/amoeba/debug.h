/* 
** This file is part of the FIREBALL AMOEBA System.
**
**
** Last modified:
**              03/11/02
**
** Changes:
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
**
*/ 



#ifndef __DEBUG_H__
#define __DEBUG_H__

/*
** This is used mainly in the kernel for different levels of debug which
** can be set at compile time.  Add a flag -DDEBUG_LEVEL=n (n >= 0) to the
** compiler call to get the desired level of debugging.
** Example call to DPRINTF is
**	DPRINTF(2, ("this is the %d error message\n", mesg_num));
*/



#ifdef DEBUG_LEVEL

/*
** Generic debug macro
*/

#define	DPRINTF(x, y)           \
	if ((x) <= DEBUG_LEVEL)     \
	    printf y;               \
	else                        \
	    /* do nothing */;




#else   /* !DEBUG_LEVEL */

#define DPRINTF(x,y)

#endif  /* !DEBUG_LEVEL */

#endif /* !__DEBUG_H__ */

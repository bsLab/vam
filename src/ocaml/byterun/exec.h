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
**    $VERSION:     1.01
**
**    $INFO:
**
**  exec.h : format of executable bytecode files
**
**    $ENDOFINFO
**
*/


#ifndef _exec_
#define _exec_

/*
** Executable bytecode files are composed of a number of sections,
** identified by 4-character names.  A table of contents at the
** end of the file lists the section names along with their sizes,
** in the order in which they appear in the file:
**
** offset 0 --->  initial junk
**                data for section 1
**                data for section 2
**                ...
**                data for section N
**                table of contents:
**                  descriptor for section 1
**                  ...
**                  descriptor for section N
**                trailer
** end of file --->
*/

/*
** Structure of t.o.c. entries
** Numerical quantities are 32-bit unsigned integers, big endian 
*/

struct section_descriptor {
  char name[4];                 /* Section name */
  uint32 len;                   /* Length of data in bytes */
};

/*
** Structure of the trailer. 
*/

struct exec_trailer {
  uint32 num_sections;          /* Number of sections */
  char magic[12];               /* The magic number */
  struct section_descriptor * section; /* Not part of file */
};

#define TRAILER_SIZE (4+12)

/*
** Magic number for this release 
*/

#define EXEC_MAGIC "Caml1999X007"


#endif

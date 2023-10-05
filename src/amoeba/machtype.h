
#ifndef __MACHTYPE_H_
#define __MACHTYPE_H_

#include "config.h"
/*
** Must be consistent with machtype.ml(type t)
*/

#define INT8        0
#define INT16       1
#define INT32       2
#define INT64       3
#define UINT8       4
#define UINT16      5
#define UINT32      6
#define UINT64      7
/*
** == UINT
*/
#define WORD8       8
#define WORD16      9
#define WORD32      10
#define WORD64      11

/*
** Macros to read and write specified value from and to buffer
*/

#define INT8_R(d)    (*((ARCH_INT8_TYPE *)d))
#define INT16_R(d)   (*((ARCH_INT16_TYPE *)d))
#define INT32_R(d)   (*((ARCH_INT32_TYPE *)d))
#define INT64_R(d)   (*((ARCH_INT64_TYPE *)d))

#define UINT8_R(d)    (*((ARCH_UINT8_TYPE *)d))
#define UINT16_R(d)   (*((ARCH_UINT16_TYPE *)d))
#define UINT32_R(d)   (*((ARCH_UINT32_TYPE *)d))
#define UINT64_R(d)   (*((ARCH_UINT64_TYPE *)d))


#define INT8_W(d,v)     (*((ARCH_INT8_TYPE *)d)=(ARCH_INT8_TYPE)v)
#define INT16_W(d,v)    (*((ARCH_INT16_TYPE *)d)=(ARCH_INT16_TYPE)v)
#define INT32_W(d,v)    (*((ARCH_INT32_TYPE *)d)=(ARCH_INT32_TYPE)v)
#define INT64_W(d,v)    (*((ARCH_INT64_TYPE *)d)=(ARCH_INT64_TYPE)v)

#define UINT8_W(d,v)     (*((ARCH_UINT8_TYPE *)d)=(ARCH_UINT8_TYPE)v)
#define UINT16_W(d,v)    (*((ARCH_UINT16_TYPE *)d)=(ARCH_UINT16_TYPE)v)
#define UINT32_W(d,v)    (*((ARCH_UINT32_TYPE *)d)=(ARCH_UINT32_TYPE)v)
#define UINT64_W(d,v)    (*((ARCH_UINT64_TYPE *)d)=(ARCH_UINT64_TYPE)v)



typedef unsigned int UINT;
typedef signed int SINT;
typedef unsigned char UCHAR;
typedef signed char SCHAR;

#ifdef HAVE_LONGLONG
  typedef long long unsigned int LLUINT;
  typedef long long signed int LLSINT;
#endif


#define MACHTYPE_R(d)       ((int)((UCHAR)d[0]))
#define MACHTYPE_W(d,v)     (d[0]=(UCHAR)v)
#define MACHDATA(d)         ((UCHAR*)(&d[1]))

#endif /* !__MACHTYPE_H_ */

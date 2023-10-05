/* config.h header created from Amakefile.sys */
#ifndef _config_
#define _config_

#define OCAML_OS_TYPE "Unix"
#define OCAML_OS_ID "bsd_elf"
#define OCAML_STDLIB_DIR "/usr/local/lib/ocaml"
#define HAS_STACK_OVERFLOW_DETECTION
#define HAS_ALARM
#define HAS_ASYNC_IO
#define HAS_CHROOT
#define HAS_DIRENT
#define HAS_DUP2
#define HAS_EXECV
#define HAS_EXECVE
#define HAS_EXECVP
#define HAS_FCHMOD
#define HAS_FORK
#define HAS_GETCWD
#define HAS_GETHOSTNAME
#define HAS_GETGROUPS
#define HAS_GETPID
#define HAS_GETPPID
#define HAS_GETPRIORITY
#define HAS_GETTIMEOFDAY
#define HAS_GETWD
#define HAS_INET_ATON
#define HAS_KILL
#define HAS_LOCALE
#define HAS_LOCKF
#define HAS_MKFIFO
#define HAS_MKTIME
#define HAS_MMAP
#define HAS_NICE
#define HAS_PIPE
#define HAS_POPEN
#define HAS_PUTENV
#define HAS_REWINDDIR
#define HAS_SELECT
#define HAS_SETITIMER
#define HAS_SETSID
#define HAS_SOCKETS
#define HAS_STAT
#define HAS_STOPPROCESS
#define HAS_STRERROR
#define HAS_STRNCASECMP
#define HAS_SYMLINK
#define HAS_SYS_SELECT_H
#define HAS_TERMCAP
#define HAS_TERMIOS
#define HAS_TIMES
#define HAS_TRUNCATE
#define HAS_UNISTD
#define HAS_UNAME
#define HAS_UTIME
#define HAS_UTIMES
#define HAS_WAIT
#define HAS_WAITPID
#define HAS_WAIT4
#define HAVE_PROTO
#define OCAML_ARCH_TYPE "i386"
#define OCAML_MODEL_TYPE "default"
#undef ARCH_SIXTYFOUR
#define SIZEOF_INT 4
#define SIZEOF_LONG 4
#define SIZEOF_SHORT 2
#define ARCH_INT8_TYPE char
#define ARCH_UINT8_TYPE unsigned char
#define ARCH_INT16_TYPE short
#define ARCH_UINT16_TYPE unsigned short
#define ARCH_INT32_TYPE int
#define ARCH_UINT32_TYPE unsigned int
#define ARCH_INT64_TYPE long long
#define ARCH_UINT64_TYPE unsigned long long
#define ARCH_INT64_PRINTF_FORMAT "ll"
#undef ARCH_BIG_ENDIAN
#undef ARCH_ALIGN_DOUBLE
#undef ARCH_ALIGN_INT64
#undef NONSTANDARD_DIV_MOD


/*
** Types for signed chars, 16-bit integers, 32-bit integers, 64-bit integers 
*/

#ifdef AMOEBA_OLD

  typedef short int16;            /* FIXME -- not true on the Cray T3E */
  typedef unsigned short uint16;  /* FIXME -- not true on the Cray T3E */

  #if SIZEOF_INT == 4
    typedef int int32;
    typedef unsigned int uint32;
  #elif SIZEOF_LONG == 4
    typedef long int32;
    typedef unsigned long uint32;
  #elif SIZEOF_SHORT == 4
    typedef short int32;
    typedef unsigned short uint32;
  #endif

#else   /* AMOEBA */
  #include <am_types.h>
#endif  /* AMOEBA */

#if defined(ARCH_INT64_TYPE) && defined(ARCH_UINT64_TYPE)
typedef ARCH_INT64_TYPE int64;
typedef ARCH_UINT64_TYPE uint64;
#else
/*
** Int64.t will not be supported, and operations over it are not defined,
** but we must define the types int64 and uint64 as 64-bit placeholders. 
*/

typedef struct { uint32 a, b; } uint64;
typedef uint64 int64;
#endif


/*
** Library dependencies 
*/

/* We use threaded code interpretation if the compiler provides labels
   as first-class values (GCC 2.x).
   Macintosh 68k also uses threaded code, with the assembly-language
   bytecode interpreter (THREADED_CODE defined in config/sm-Mac.h).
*/

#if defined(__GNUC__) && __GNUC__ >= 2 && !defined(DEBUG) && !defined (SHRINKED_GNUC)
#define THREADED_CODE
#endif


/* Do not change this definition. */
#define Page_size (1 << Page_log)

/* Memory model parameters */

/* The size of a page for memory management (in bytes) is [1 << Page_log].
   It must be a multiple of [sizeof (long)]. */
#define Page_log 12             /* A page is 4 kilobytes. */

/* Initial size of stack (bytes). */
#define Stack_size (4096 * sizeof(value))

/* Minimum free size of stack (bytes); below that, it is reallocated. */
#define Stack_threshold (256 * sizeof(value))

/* Default maximum size of the stack (words). */
#define Max_stack_def (256 * 1024)


/* Maximum size of a block allocated in the young generation (words). */
/* Must be > 4 */
#define Max_young_wosize 256


/* Minimum size of the minor zone (words).
   This must be at least [Max_young_wosize + 1]. */
#define Minor_heap_min 4096

/* Maximum size of the minor zone (words).
   Must be greater than or equal to [Minor_heap_min].
*/
#define Minor_heap_max (1 << 28)

/* Default size of the minor zone. (words)  */
#define Minor_heap_def 32768


/* Minimum size increment when growing the heap (words).
   Must be a multiple of [Page_size / sizeof (value)]. */
#define Heap_chunk_min (2 * Page_size / sizeof (value))

/* Maximum size of a contiguous piece of the heap (words).
   Must be greater than or equal to [Heap_chunk_min].
   Must be greater than or equal to [Bhsize_wosize (Max_wosize)]. */
#define Heap_chunk_max (Bhsize_wosize (Max_wosize))

/* Default size increment when growing the heap. (words)
   Must be a multiple of [Page_size / sizeof (value)]. */
#define Heap_chunk_def (62 * 1024)

/* Default initial size of the major heap (words);
   same constraints as for Heap_chunk_def. */
#define Init_heap_def (62 * 1024)


/* Default speed setting for the major GC.  The heap will grow until
   the dead objects and the free list represent this percentage of the
   heap size.  The rest of the heap is live objects. */
#define Percent_free_def 42

/* Default setting for the compacter: off */
#define Max_percent_free_def 1000000


#endif /* _config_ */

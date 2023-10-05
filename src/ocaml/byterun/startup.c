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
**    $AUTHORS:     Xavier Leroy and Damien Dolige, Stefan Bosse
**    $INITIAL:     (C) 1996 Xavier Leroy and Damien Doligez, INRIA Rocquencourt
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.05
**
**    $INFO:
**
**  Startup Code
**
**    $ENDOFINFO
**
*/




#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include "config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef _WIN32
#include <process.h>
#endif
#include "alloc.h"
#include "backtrace.h"
#include "callback.h"
#include "custom.h"
#include "debugger.h"
#ifdef DYNLINK
#include "dynlink.h"
#endif
#include "exec.h"
#include "fail.h"
#include "fix_code.h"
#include "gc_ctrl.h"
#include "instrtrace.h"
#include "interp.h"
#include "intext.h"
#include "io.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "osdeps.h"
#include "prims.h"
#include "printexc.h"
#include "reverse.h"
#include "signals.h"
#include "stacks.h"
#include "sys.h"
#include "startup.h"

#ifndef O_BINARY
#define O_BINARY 0
#endif

#ifndef SEEK_END
#define SEEK_END 2
#endif

#ifndef DYNLINK

/*
** The table of primitives 
*/
struct ext_table prim_table;

/*
** Unknown primitive. Be gentle:
** Raise exception only when called from the bytecode program!
*/

static value unknown_prim(value v)
{
    failwith("Unknown primitive called!\n");
    /*
    ** never reached
    */
    return 0;
};

/*
** Look up the given primitive name in the built-in primitive table,
*/

static c_primitive lookup_primitive(char * name)
{
    int i;
    void * res;

    for (i = 0; names_of_builtin_cprim[i] != NULL; i++) 
    {
        if (strcmp(name, names_of_builtin_cprim[i]) == 0)
            return builtin_cprim[i];
    }
#if 0
    return NULL;
#endif
    print_stderr("Warning: unknown primitive %s.\n",name);
    return unknown_prim;
}


/*
** Build the table of primitives.
**   Abort the runtime system on error. 
*/
void build_primitive_table(char * req_prims)
{
    char * tofree1, * tofree2;
    char * p;

    /*
    ** Build the primitive table 
    */
    ext_table_init(&prim_table, 0x180);
    for (p = req_prims; *p != 0; p += strlen(p) + 1) 
    {
        c_primitive prim = lookup_primitive(p);
        if (prim == NULL)
            fatal_error_arg("Fatal error: unknown C primitive `%s'\n", p);
        ext_table_add(&prim_table, (void *) prim);
    };
}

#endif

extern int parser_trace;

CAMLexport header_t atom_table[256];

/*
** Initialize the atom table 
*/

static void init_atoms(void)
{
  int i;
  for(i = 0; i < 256; i++) atom_table[i] = Make_header(0, i, Caml_white);
}

/*
** Read the trailer of a bytecode file 
*/

static void fixup_endianness_trailer(uint32 * p)
{
#ifndef ARCH_BIG_ENDIAN
  Reverse_32(p, p);
#endif
}

static long padoff=0;

/*
** Try to read the bytecode trailer
*/
static int read_trailer(int fd, struct exec_trailer *trail)
{
  padoff=0;
  lseek(fd, (long) -TRAILER_SIZE, SEEK_END);
  if (read(fd, (char *) trail, TRAILER_SIZE) < TRAILER_SIZE)
    return BAD_BYTECODE;
  fixup_endianness_trailer(&trail->num_sections);

  if (strncmp(trail->magic, EXEC_MAGIC, 12) == 0)
    return 0;
  else
  {
    char buf[512]; 
    /*
    ** Maybe file is padded with '0' or ' ' at the end ?
    ** Give the file a last chance...
    */
    lseek(fd, (long) -512, SEEK_END);
    if (read(fd,buf,512)==512)
    {
        int i=511;
        while(i>=0 && 
              (buf[i]==(char)0 || buf[i]==' '))
        {
            i--;
            padoff++;
        };

        /*
        ** read again the expected trailer structure...
        */
        lseek(fd, (long) -(TRAILER_SIZE+padoff), SEEK_END);
        if (read(fd, (char *) trail, TRAILER_SIZE) < TRAILER_SIZE)
            return BAD_BYTECODE;

        fixup_endianness_trailer(&trail->num_sections);
        if (strncmp(trail->magic, EXEC_MAGIC, 12) == 0)
            return 0;       /* Found bytecode */
        else
            return BAD_BYTECODE;

        /*
        ** Okay, really not a bytecode file!
        */
    }
    else
        return BAD_BYTECODE;
  };
}

/*
** Try to determine file type: bytecode, script or none of these...
*/

int attempt_open(char **name, struct exec_trailer *trail,
                 int do_open_script)
{
  char * truename;
  int fd;
  int err;
  char buf [2];

  truename = search_exe_in_path(*name);
  *name = truename;
  gc_message(0x100, "Opening bytecode executable %s\n",
             (unsigned long) truename);
  fd = open(truename, O_RDONLY | O_BINARY);
  if (fd == -1) {
    gc_message(0x100, "Cannot open file\n", 0);
    return FILE_NOT_FOUND;
  }
  if (!do_open_script) {
    err = read (fd, buf, 2);
    if (err < 2 || (buf [0] == '#' && buf [1] == '!')) {
      close(fd);
      gc_message(0x100, "Rejected #! script\n", 0);
      return BAD_BYTECODE;
    }
  }
  err = read_trailer(fd, trail);
  if (err != 0) {
    close(fd);
    gc_message(0x100, "Not a bytecode executable\n", 0);
    return err;
  }
  return fd;
}

/*
** Read the section descriptors 
*/

void read_section_descriptors(int fd, struct exec_trailer *trail)
{
  int toc_size, i;

  toc_size = trail->num_sections * 8;
  trail->section = stat_alloc(toc_size);
  lseek(fd, - (long) (TRAILER_SIZE + padoff + toc_size), SEEK_END);
  if (read(fd, (char *) trail->section, toc_size) != toc_size)
    fatal_error("Fatal error: cannot read section table\n");

  /*
  ** Fixup endianness of lengths 
  */
  for (i = 0; i < trail->num_sections; i++)
    fixup_endianness_trailer(&(trail->section[i].len));
}

/*
** Position fd at the beginning of the section having the given name.
** Return the length of the section data in bytes, or -1 if no section
** found with that name. 
*/

int32 seek_optional_section(int fd, struct exec_trailer *trail, char *name)
{
  long ofs;
  int i;

  ofs = TRAILER_SIZE + padoff + trail->num_sections * 8;
  for (i = trail->num_sections - 1; i >= 0; i--) {
    ofs += trail->section[i].len;
    if (strncmp(trail->section[i].name, name, 4) == 0) {
      lseek(fd, -ofs, SEEK_END);
      return trail->section[i].len;
    }
  }
  return -1;
}

/* Position fd at the beginning of the section having the given name.
   Return the length of the section data in bytes. */

int32 seek_section(int fd, struct exec_trailer *trail, char *name)
{
  int32 len = seek_optional_section(fd, trail, name);
  if (len == -1) 
    fatal_error_arg("Fatal_error: section `%s' is missing\n", name);
  return len;
}

/* Read and return the contents of the section having the given name.
   Add a terminating 0.  Return NULL if no such section. */

static char * read_section(int fd, struct exec_trailer *trail, char *name)
{
  int32 len;
  char * data;

  len = seek_optional_section(fd, trail, name);
  if (len == -1) return NULL;
  data = stat_alloc(len + 1);
  if (read(fd, data, len) != len)
    fatal_error_arg("Fatal error: error reading section %s\n", name);
  data[len] = 0;
  return data;
}

static char help_info[]="\
 Invocation of ocamlrun: 4 cases.                                   \n\
                                                                    \n\
   1.  runtime + bytecode                                           \n\
       user types:  ocamlrun [options] bytecode args...             \n\
       arguments:  ocamlrun [options] bytecode args...              \n\
                                                                    \n\
   2.  bytecode script                                              \n\
       user types:  bytecode args...                                \n\
   2a  (kernel 1) arguments:  ocamlrun ./bytecode args...           \n\
   2b  (kernel 2) arguments:  bytecode bytecode args...             \n\
                                                                    \n\
   3.  concatenated runtime and bytecode                            \n\
       user types:  composite args...                               \n\
       arguments:  composite args...                                \n\
                                                                    \n\
Algorithm:                                                          \n\
  1-  If argument 0 is a valid byte-code file that does not start with #!,\n\
      then we are in case 3 and we pass the same command line to the    \n\
      Objective Caml program.                                           \n\
  2-  In all other cases, we parse the command line as:                 \n\
        (whatever) [options] bytecode args...                           \n\
      and we strip \"(whatever) [options]\" from the command line.      \n\
";  

static char options_info[]="\n\
Virtual machine option flags:                                           \n\
                                                                        \n\
  Flags can also be set with Environment Variable OCAMLRUNPARAM/CAMLRUNPARAM\n\
  export CAMLRUNPARAM=\"-b-d-v\"                                        \n\
                                                                        \n\
  usage: <thisvm> [VM options] <bytecode> [Bytecode argument]           \n\
                                                                        \n\
    -h:         print this help                                         \n\
    -v:         verbose debug level (GC)                                \n\
    -b:         on an uncaught exception, load current bytecode debug infos\n\
    -d:         backtrace debugging                                     \n\
    -p:         print builtin primitive table                           \n\
";

/*
** Configuration parameters and flags 
*/

static unsigned long percent_free_init = Percent_free_def;
static unsigned long max_percent_free_init = Max_percent_free_def;
static unsigned long minor_heap_init = Minor_heap_def;
static unsigned long heap_chunk_init = Heap_chunk_def;
static unsigned long heap_size_init = Init_heap_def;
static unsigned long max_stack_init = Max_stack_def;

/*
** Usage help
*/
void usage(void)
{
    printf("%s\n",help_info);
    printf("%s\n",options_info);
    return;
};
/*
** Parse options on the command line 
*/

static int parse_command_line(char **argv)
{
  int i, j;

  for(i = 1; argv[i] != NULL && argv[i][0] == '-'; i++) {
    switch(argv[i][1]) {
#ifdef DEBUG
    case 't':
      trace_flag = 1;
      break;
#endif
    case 'v':
      verb_gc = 1+4+8+16+32;
      break;
    case 'p':
      for (j = 0; names_of_builtin_cprim[j] != NULL; j++)
        printf("%s\n", names_of_builtin_cprim[j]);
      exit(0);
      break;
    case 'b':
      backtrace_active=1;
      backtrace_self=1;
      break;
    case 'd':
      backtrace_verbose=1;
      break;
#ifdef DYNLINK
    case 'I':
      if (argv[i + 1] != NULL) {
        ext_table_add(&shared_libs_path, argv[i + 1]);
        i++;
      }
      break;
#endif
    case 'h':
      usage();
      exit(0);
      break;

    default:
      usage();
      fatal_error_arg("Unknown option %s.\n", argv[i]);
    }
  }
  return i;
}

/*
** Parse the CAMLRUNPARAM variable 
*/
/*
** The option letter for each runtime option is the first letter of the
** last word of the ML name of the option (see [stdlib/gc.mli]).
** Except for l (maximum stack size) and h (initial heap size).
*/

static void scanmult (char *opt, long unsigned int *var)
{
  char mult = ' ';
  sscanf (opt, "=%lu%c", var, &mult);
  if (mult == 'k') *var = *var * 1024;
  if (mult == 'M') *var = *var * (1024 * 1024);
  if (mult == 'G') *var = *var * (1024 * 1024 * 1024);
}

static void parse_camlrunparam(void)
{
  char *opt = getenv ("OCAMLRUNPARAM");

  if (opt == NULL) opt = getenv ("CAMLRUNPARAM");

  if (opt != NULL){
    while (*opt != '\0'){
      switch (*opt++){
      case 's': scanmult (opt, &minor_heap_init); break;
      case 'i': scanmult (opt, &heap_chunk_init); break;
      case 'h': scanmult (opt, &heap_size_init); break;
      case 'l': scanmult (opt, &max_stack_init); break;
      case 'o': scanmult (opt, &percent_free_init); break;
      case 'O': scanmult (opt, &max_percent_free_init); break;
      case 'v': scanmult (opt, &verb_gc); break;
      case 'b': backtrace_active=1;
                backtrace_self=1;
                break;
      case 'd': backtrace_verbose=1;
                break;
      case 'p': parser_trace = 1; break;
      }
    }
  }
}

extern void init_ieee_floats (void);

#ifdef _WIN32
extern void caml_signal_thread(void * lpParam);
#endif

/*
** Main entry point when loading code from a file 
*/

CAMLexport void caml_main(char **argv)
{
  int fd, pos;
  struct exec_trailer trail;
  struct channel * chan;
  value res;
  char * shared_lib_path, * shared_libs, * req_prims;

  /* Machine-dependent initialization of the floating-point hardware
     so that it behaves as much as possible as specified in IEEE */
  init_ieee_floats();
  init_custom_operations();

#ifdef DYNLINK
  ext_table_init(&shared_libs_path, 8);
#endif

  external_raise = NULL;
  /* Determine options and position of bytecode file */
#ifdef DEBUG
  verb_gc = 63;
#endif
  parse_camlrunparam();
  pos = 0;
  fd = attempt_open(&argv[0], &trail, 0);
  if (fd < 0) {
    pos = parse_command_line(argv);
    if (argv[pos] == 0)
      fatal_error("No bytecode file specified.\n");
    fd = attempt_open(&argv[pos], &trail, 1);
    switch(fd) {
    case FILE_NOT_FOUND:
      fatal_error_arg("Fatal error: cannot find file %s\n", argv[pos]);
      break;
    case BAD_BYTECODE:
      fatal_error_arg(
        "Fatal error: the file %s is not a bytecode executable file\n",
        argv[pos]);
      break;
    }
  }
  /* Read the table of contents (section descriptors) */
  read_section_descriptors(fd, &trail);
  /* Initialize the abstract machine */
  init_gc (minor_heap_init, heap_size_init, heap_chunk_init,
           percent_free_init, max_percent_free_init);
  init_stack (max_stack_init);
  init_atoms();
  /* Initialize the interpreter */
  interprete(NULL, 0);
  /* Initialize the debugger, if needed */
  debugger_init();
  /* Load the code */
  code_size = seek_section(fd, &trail, "CODE");
  load_code(fd, code_size);
  /* Build the table of primitives */
  shared_lib_path = read_section(fd, &trail, "DLPT");
  shared_libs = read_section(fd, &trail, "DLLS");
  req_prims = read_section(fd, &trail, "PRIM");

    if (req_prims == NULL) 
        fatal_error("Fatal error: no PRIM section\n");

#ifdef DYNLINK
    build_primitive_table(shared_lib_path, shared_libs, req_prims);
#else
    build_primitive_table(req_prims);
#endif

  stat_free(shared_lib_path);
  stat_free(shared_libs);
  stat_free(req_prims);
  /* Load the globals */
  seek_section(fd, &trail, "DATA");
  chan = open_descriptor_in(fd);
  global_data = input_val(chan);
  close_channel(chan); /* this also closes fd */
  stat_free(trail.section);
  /* Ensure that the globals are in the major heap. */
  oldify(global_data, &global_data);
  /* Initialize system libraries */
  init_exceptions();
  /* Initialize backtracing */
  init_backtrace();     
  sys_init(argv + pos);
#ifdef _WIN32
  /* Start a thread to handle signals */
  if (getenv("CAMLSIGPIPE"))
    _beginthread(caml_signal_thread, 4096, NULL);
#endif
  /* Execute the program */
  debugger(PROGRAM_START);
  res = interprete(start_code, code_size);
  if (Is_exception_result(res)) {
    exn_bucket = Extract_exception(res);
    if (debugger_in_use) {
      extern_sp = &exn_bucket; /* The debugger needs the exception value. */
      debugger(UNCAUGHT_EXC);
    }
    fatal_uncaught_exception(exn_bucket);
  }
}

/*
** Main entry point when code is linked in as initialized data 
*/

CAMLexport void caml_startup_code(code_t code, asize_t code_size,
                                  char *data, char **argv)
{
  value res;

  init_ieee_floats();
#ifdef DEBUG
  verb_gc = 63;
#endif
  parse_camlrunparam();
  external_raise = NULL;
  /* Initialize the abstract machine */
  init_gc (minor_heap_init, heap_size_init, heap_chunk_init,
           percent_free_init, max_percent_free_init);
  init_stack (max_stack_init);
  init_atoms();
  /* Initialize the interpreter */
  interprete(NULL, 0);
  /* Load the code */
  start_code = code;
#ifdef THREADED_CODE
  thread_code(start_code, code_size);
#endif
  /* Use the builtin table of primitives */
  prim_table.size = prim_table.capacity = -1;
  prim_table.contents = (void **) builtin_cprim;
  /* Load the globals */
  global_data = input_val_from_string((value)data, 0);
  /* Ensure that the globals are in the major heap. */
  oldify(global_data, &global_data);
  /* Run the code */
  init_exceptions();
  sys_init(argv);
  res = interprete(start_code, code_size);
  if (Is_exception_result(res))
    fatal_uncaught_exception(Extract_exception(res));
}


/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Dynamic loading of C primitives. */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>
#include "config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include "alloc.h"
#include "dynlink.h"
#include "fail.h"
#include "mlvalues.h"
#include "memory.h"
#include "misc.h"
#include "osdeps.h"
#include "prims.h"

#ifndef NATIVE_CODE

/* The table of primitives */
struct ext_table prim_table;

/* The table of shared libraries currently opened */
static struct ext_table shared_libs;

/* The search path for shared libraries */
struct ext_table shared_libs_path;

/* Look up the given primitive name in the built-in primitive table,
   then in the opened shared libraries (shared_libs) */
static c_primitive lookup_primitive(char * name)
{
  int i;
  void * res;

  for (i = 0; names_of_builtin_cprim[i] != NULL; i++) {
    if (strcmp(name, names_of_builtin_cprim[i]) == 0)
      return builtin_cprim[i];
  }
  for (i = 0; i < shared_libs.size; i++) {
    res = caml_dlsym(shared_libs.contents[i], name);
    if (res != NULL) return (c_primitive) res;
  }
  return NULL;
}

/* Parse the OCAML_STDLIB_DIR/ld.conf file and add the directories
   listed there to the search path */

#define LD_CONF_NAME "ld.conf"

static char * parse_ld_conf(void)
{
  char * stdlib, * ldconfname, * config, * p, * q;
  struct stat st;
  int ldconf, nread;

  stdlib = getenv("OCAMLLIB");
  if (stdlib == NULL) stdlib = getenv("CAMLLIB");
  if (stdlib == NULL) stdlib = OCAML_STDLIB_DIR;
  ldconfname = stat_alloc(strlen(stdlib) + 2 + sizeof(LD_CONF_NAME));
  strcpy(ldconfname, stdlib);
  strcat(ldconfname, "/" LD_CONF_NAME);
  if (stat(ldconfname, &st) == -1) {
    stat_free(ldconfname);
    return NULL;
  }
  ldconf = open(ldconfname, O_RDONLY, 0);
  if (ldconf == -1)
    fatal_error_arg("Fatal error: cannot read loader config file %s\n",
                    ldconfname);
  config = stat_alloc(st.st_size + 1);
  nread = read(ldconf, config, st.st_size);
  if (nread == -1) 
    fatal_error_arg("Fatal error: error while reading loader config file %s\n",
                    ldconfname);
  config[nread] = 0;
  q = config;
  for (p = config; *p != 0; p++) {
    if (*p == '\n') {
      *p = 0;
      ext_table_add(&shared_libs_path, q);
      q = p + 1;
    }
  }
  if (q < p) ext_table_add(&shared_libs_path, q);
  close(ldconf);
  stat_free(ldconfname);
  return config;
}

/* Open the given shared library and add it to shared_libs.
   Abort on error. */
static void open_shared_lib(char * name)
{
  char * realname;
  void * handle;

  realname = search_dll_in_path(&shared_libs_path, name);
  gc_message(0x100, "Loading shared library %s\n", (unsigned long) realname);
  handle = caml_dlopen(realname);
  if (handle == NULL)
    fatal_error_arg2("Fatal error: cannot load shared library %s\n", name,
                     "Reason: %s\n", caml_dlerror());
  ext_table_add(&shared_libs, handle);
  stat_free(realname);
}

#ifdef DYNLINK
/* Build the table of primitives, given a search path and a list
   of shared libraries (both 0-separated in a char array).
   Abort the runtime system on error. */
void build_primitive_table(char * lib_path,
                           char * libs,
                           char * req_prims)
{
  char * tofree1, * tofree2;
  char * p;

  /* Initialize the search path for dynamic libraries:
     - directories specified on the command line with the -I option
     - directories specified in the CAML_LD_LIBRARY_PATH
     - directories specified in the executable
     - directories specified in the file <stdlib>/ld.conf */
  tofree1 = decompose_path(&shared_libs_path, getenv("CAML_LD_LIBRARY_PATH"));
  if (lib_path != NULL)
    for (p = lib_path; *p != 0; p += strlen(p) + 1)
      ext_table_add(&shared_libs_path, p);
  tofree2 = parse_ld_conf();
  /* Open the shared libraries */
  ext_table_init(&shared_libs, 8);
  if (libs != NULL)
    for (p = libs; *p != 0; p += strlen(p) + 1)
      open_shared_lib(p);
  /* Build the primitive table */
  ext_table_init(&prim_table, 0x180);
  for (p = req_prims; *p != 0; p += strlen(p) + 1) {
    c_primitive prim = lookup_primitive(p);
    if (prim == NULL)
      fatal_error_arg("Fatal error: unknown C primitive `%s'\n", p);
    ext_table_add(&prim_table, (void *) prim);
  }
  /* Clean up */
  stat_free(tofree1);
  stat_free(tofree2);
  ext_table_free(&shared_libs_path, 0);
}
#endif

#endif

/** dlopen interface for the bytecode linker **/

#define Handle_val(v) (*((void **) (v)))

CAMLprim value dynlink_open_lib(value filename)
{
  void * handle;
  value result;

  handle = caml_dlopen(String_val(filename));
  if (handle == NULL) failwith(caml_dlerror());
  result = alloc_small(1, Abstract_tag);
  Handle_val(result) = handle;
  return result;
}

CAMLprim value dynlink_close_lib(value handle)
{
  caml_dlclose(Handle_val(handle));
  return Val_unit;
}

#include <stdio.h>
CAMLprim value dynlink_lookup_symbol(value handle, value symbolname)
{
  void * symb;
  value result;
  symb = caml_dlsym(Handle_val(handle), String_val(symbolname));
  /* printf("%s = 0x%lx\n", String_val(symbolname), symb);
     fflush(stdout); */
  if (symb == NULL) return Val_unit /*failwith(caml_dlerror())*/;
  result = alloc_small(1, Abstract_tag);
  Handle_val(result) = symb;
  return result;
}

#ifndef NATIVE_CODE

CAMLprim value dynlink_add_primitive(value handle)
{
  return Val_int(ext_table_add(&prim_table, Handle_val(handle)));
}

CAMLprim value dynlink_get_current_libs(value unit)
{
  CAMLparam0();
  CAMLlocal1(res);
  int i;

  res = alloc_tuple(shared_libs.size);
  for (i = 0; i < shared_libs.size; i++) {
    value v = alloc_small(1, Abstract_tag);
    Handle_val(v) = shared_libs.contents[i];
    Store_field(res, i, v);
  }
  CAMLreturn(res);
}

#else

value dynlink_add_primitive(value handle)
{
  invalid_argument("dynlink_add_primitive");
  return Val_unit; /* not reached */
}

value dynlink_get_current_libs(value unit)
{
  invalid_argument("dynlink_get_current_libs");
  return Val_unit; /* not reached */
}

#endif

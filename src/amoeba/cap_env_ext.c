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
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2003 BSSLAB
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.02
**
**    $INFO:
**
** Amoeba Capability environment handling.
**
**    $ENDOFINFO
**
*/



#include <amoeba.h>
#include <module/buffers.h>
#include <stderr.h>

#include <mlvalues.h>
#include <alloc.h>
#include <callback.h>
#include <memory.h>
#include <signals.h>
#include <fail.h>

#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <module/ar.h>


extern char *ocaml_os_type;

static capability nilcap;


CAMLexport value am_getcap(value name)
{
    CAMLparam0();
    CAMLlocal1(buf_str);

    
    capability  envcap=nilcap;
    
    char *names = String_val(name);
    int os_len = strlen(ocaml_os_type);

    if (os_len == 6 &&
        strcmp(ocaml_os_type,"Amoeba") == 0)
    {
        /*
        ** Amoeba native environment. Get the caps
        ** from the process enviornment.
        */
        capability *tmp = getcap (names);
        if (tmp != NULL)    
            envcap = *tmp;
    } else
        failwith ("am_getcap: only for native Amoeba OS.\n");
    

    
    buf_str = alloc_string(CAPSIZE);

    memcpy((char *)String_val(buf_str),(char *)&envcap,CAPSIZE);

    CAMLreturn(buf_str);    
}

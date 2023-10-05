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
**      BSSLAB, Dr. Stefan Bosse www.bsslab.de
**
**    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF: 
**    Free Software Foundation-Europe, GNU GPL License, Version 2
**
**    $MODIFIEDBY:  BSSLAB
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2003
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.01
**
**    $INFO:
**
**
**
**    $ENDOFINFO
**
*/




#include <amoeba.h>
#include <module/rpc.h>
#include <string.h>
#include <module/ar.h>
#include <stderr.h>

#include <mlvalues.h>
#include <alloc.h>
#include <callback.h>
#include <memory.h>
#include <signals.h>
#include <fail.h>

#include <caplist.h>
#include <stdio.h>
#include "buf.h"
#include <ar.h>
#include <module/buffers.h>

#include "machtype.h"

extern unsigned long
_buildstack(
    char          *buf,     /* In: stack buffer */
    long           buflen,  /* In: stack buffer length */
    unsigned long  start,   /* In: VM start of buffer */
    char          *argv[],  /* In: NULL-terminated argument list */
    char          *envp[],  /* In: NULL-terminated environment list */
    struct caplist caps[]   /* In: capability list */
);


/*
** argument:
**               (value vbuf,
**               value vstart,
**               value vargs,
**               value vstrenv,
**               value vcapenv)
**
*/
CAMLexport value
buildstack_ext(value v)
{
    CAMLparam0();
    CAMLlocal1(res);
    UCHAR *datar;
    
    struct caml_buf *vbuf=Buf_val(Field(v,0));
    char *buf=vbuf->data;
    UCHAR *us=(UCHAR*)String_val(Field(v,1));
    int type=(int)us[0];
    UCHAR *data=&us[1];   
    unsigned long start;
    unsigned long sp;
    long buflen=vbuf->size;
    value vargs=Field(v,2);
    value vstrenv=Field(v,3);
    value vcapenv=Field(v,4);
    int args=Wosize_val(vargs);
    int strenvs=Wosize_val(vstrenv);
    int capenvs=Wosize_val(vcapenv);
    int i;
    
    char    *argv[args+1];
    char    *envp[strenvs+1];
    struct caplist caps[capenvs+1];

    if(type == WORD32)    
    {
            UINT d1=(UINT)(data[0]);
            UINT d2=(UINT)(data[1]);
            UINT d3=(UINT)(data[2]);
            UINT d4=(UINT)(data[3]);
            start=(long)(d1 | (d2 << 8) | (d3 << 16) | (d4 << 24));
    }
    else
        failwith ("buildstack");

    for(i=0;i<args;i++)
        argv[i] = String_val(Field(vargs,i));
    for(i=0;i<strenvs;i++)
        envp[i] = String_val(Field(vstrenv,i));
    for(i=0;i<capenvs;i++)
    {
        value vcs = Field(vcapenv,i);
        
        caps[i].cl_name = String_val(Field(vcs,0));


        caps[i].cl_cap = (capability *)(String_val(Field(vcs,1))); 


    };   
    argv[args] = NULL;
    envp[strenvs] = NULL;
    caps[capenvs].cl_name = NULL;
    
    sp = _buildstack(buf,buflen,start,argv,envp,caps);


    res = alloc_string(5);
    datar=(UCHAR*)String_val(res);
    datar[0] = (UCHAR)WORD32;
    datar[1] = (UCHAR)(sp & 0xff);
    datar[2] = (UCHAR)((sp >> 8) & 0xff);
    datar[3] = (UCHAR)((sp >> 16) & 0xff);
    datar[4] = (UCHAR)((sp >> 24) & 0xff);
                                                                    
    CAMLreturn(res);
};               
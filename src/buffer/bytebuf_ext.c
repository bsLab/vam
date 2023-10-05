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
**    $VERSION:     1.05
**
**    $INFO:
**
** Buffer management.
**
** There are two different buffers: 
**
**      physical buffers -> MASTER
**      logical  buffers -> SLAVE (subwindows from a physical buffer)
** 
**
** External C functions.
**
** TODO:
**      caml_buf structure allocation within data space (master only)
**      --> faster allocation/freeing
**
**
**    $ENDOFINFO
**
*/





#include <mlvalues.h>
#include <alloc.h>
#include <custom.h>
#include <callback.h>
#include <memory.h>
#include <signals.h>
#include <fail.h>
#include <stdio.h>

#include "buf.h"


/* #define DEBUG_LEVEL 1 */
#include "sys/debug.h"


static  void    buf_finalize(value v);
static  int     buf_compare(value v1, value v2);
static  long    buf_hash(value v);
static  void    buf_serialize(value, unsigned long *, unsigned long *);
static  unsigned long buf_deserialize(void * dst);


static struct custom_operations buf_ops = 
{
    "_buffer",
    buf_finalize,
    buf_compare,
    buf_hash,
    buf_serialize,
    buf_deserialize
};


static char msg[1000];

/*
** Create a new phyisical buffer.
*/

CAMLexport value
ext_buf_physical (value valsize)
{
    CAMLparam0();
    CAMLlocal1(res);
    int used,max;
     
    struct caml_buf *vbuf;  
    int size = Int_val(valsize);
        
    if (size < 0) 
    {
        sprintf(msg,"Bytebuf.buf_physical: invalid size (%d)",
                (int)size);    
        invalid_argument(msg);
    };

    /*
    ** Force fast garbage collection for large arrays
    */
  
    used = 1;
    if(size < 100)
        max = 100 ;
    else if (size < 1000)
        max = 50;
    else if (size < 10000)
        max = 30;
    else if (size < 100000)
        max = 20; 
    else if (size < 1000000)
        max = 10;
    else max = 5;


    res = alloc_custom(&buf_ops,
                     sizeof(struct caml_buf *),
                     used, max);

                                             
    vbuf = (struct caml_buf *) malloc(sizeof(struct caml_buf));
    *((struct caml_buf **)Data_custom_val(res)) = vbuf;

    
    DPRINTF(1,("buf_physical: vbuf=%x, size=%d\n",(int)vbuf,size));                            
                            
    vbuf->size = size;
    vbuf->data = (unsigned char *)malloc(size+1);
    vbuf->state = BUF_MASTER;
    vbuf->prev = (struct caml_buf *)NULL;
    vbuf->next = (struct caml_buf *)NULL;
    
    if (vbuf->data == NULL)
    {
        failwith("buf_physical: can't allocate buffer");    
    }
    
    CAMLreturn(res);
};

/*
** Create a logical (slave) buffer from a physical or an already logical
** buffer (master). Aka buffer window.
*/

CAMLexport value
ext_buf_logical (value valmaster, value valpos, value valsize)
{
    CAMLparam0();
    CAMLlocal1(res);
    int used,max;
     
    struct caml_buf *vbuf_m=Buf_val(valmaster);
    struct caml_buf *vbuf;
      
    int size_m = vbuf_m->size;

    int size = Int_val(valsize);
    int pos  = Int_val(valpos);
            
    if (size < 0 || (size+pos) > size_m) 
    {
        sprintf(msg,"Bytebuf.buf_logical: invalid pos,size (%d<-%d,%d)",
                (int)size_m,(int)pos,(int)size);    
        invalid_argument(msg);
    };

    /*
    ** Force fast garbage collection for large arrays
    */
  
    used = 1;
    if(size < 100)
        max = 100 ;
    else if (size < 1000)
        max = 50;
    else if (size < 10000)
        max = 30;
    else if (size < 100000)
        max = 20; 
    else if (size < 1000000)
        max = 10;
    else max = 5;


    res = alloc_custom(&buf_ops,
                     sizeof(struct caml_buf *),
                     used, max);

                                             
    vbuf = (struct caml_buf *) malloc(sizeof(struct caml_buf));
    *((struct caml_buf **)Data_custom_val(res)) = vbuf;

    DPRINTF(1,("buf_logical: vbuf=%x, size=%d vbuf_m->next=%x\n",
            (int)vbuf,size,(int)vbuf_m->next));

    vbuf->size = size;
    vbuf->data = (unsigned char *)(&(vbuf_m->data[pos]));
    vbuf->state = BUF_SLAVE;


    if (vbuf_m->next == NULL)
    {
        /*
        ** This is the first slave.
        */
        
        vbuf->prev = vbuf_m;
        vbuf->next = (struct caml_buf *)NULL;
        vbuf_m->next = vbuf;
    }
    else
    {
        /*
        ** There are already slaves. Insert the new one between
        ** master and the last slave.
        */
        
        (vbuf_m->next)->prev = vbuf;
        vbuf->next = vbuf_m->next;
        vbuf->prev = vbuf_m;
        vbuf_m->next = vbuf;
        
        
    };


       
    CAMLreturn(res);
};

/*
** Create a new buffer window (physical) and copy the source content.
*/

CAMLexport value
ext_buf_copy (value valmaster, value valpos, value valsize)
{
    CAMLparam0();
    CAMLlocal1(res);
    int used,max;
     
    struct caml_buf *vbuf_m=Buf_val(valmaster);
    struct caml_buf *vbuf;
      
    int size_m = vbuf_m->size;

    int size = Int_val(valsize);
    int pos  = Int_val(valpos);
            
    if (size < 0 || (size+pos) >= size_m) 
    {
        sprintf(msg,"Bytebuf.buf_copy: invalid pos,size (%d<-%d,%d)",
                (int)size_m,(int)pos,(int)size);    
        invalid_argument(msg);
    };

    /*
    ** Force fast garbage collection for large arrays
    */
  
    used = 1;
    if(size < 100)
        max = 100 ;
    else if (size < 1000)
        max = 50;
    else if (size < 10000)
        max = 30;
    else if (size < 100000)
        max = 20; 
    else if (size < 1000000)
        max = 10;
    else max = 5;


    res = alloc_custom(&buf_ops,
                     sizeof(struct caml_buf),
                     used, max);

                                             
    vbuf = Buf_val (res);
                            
    vbuf->size = size;
    vbuf->data = (unsigned char *)malloc(size+1);
    
    if (vbuf->data == NULL)
    {
        failwith ("buf_copy: can't allocate new buffer");
    }
    
    vbuf = (struct caml_buf *) malloc(sizeof(struct caml_buf));
    *((struct caml_buf **)Data_custom_val(res)) = vbuf;

    DPRINTF(1,("buf_copy: vbuf=%x, size=%d vbuf_m->next=%x\n",
            (int)vbuf,size,(int)vbuf_m->next));

    vbuf->size = size;
    vbuf->state = BUF_MASTER;


    if (vbuf_m->next == NULL)
    {
        /*
        ** This is the first slave.
        */
        
        vbuf->prev = vbuf_m;
        vbuf->next = (struct caml_buf *)NULL;
        vbuf_m->next = vbuf;
    }
    else
    {
        /*
        ** There are already slaves. Insert the new one between
        ** master and the last slave.
        */
        
        (vbuf_m->next)->prev = vbuf;
        vbuf->next = vbuf_m->next;
        vbuf->prev = vbuf_m;
        vbuf_m->next = vbuf;
        
        
    };


    memcpy(vbuf->data,
          (unsigned char *)(&(vbuf_m->data[pos])),
          size);
    
    CAMLreturn(res);
};

/*
** Get a byte from a buffer.
*/

CAMLexport value
ext_buf_get(value valbuf,value valpos)
{
    struct caml_buf *vbuf=Buf_val(valbuf);
    int pos=Int_val(valpos);
    
    if (pos < 0 || pos >= vbuf->size)
    {
        sprintf(msg,"Bytebuf.buf_get: invalid pos (%d->%d)",
                (int)pos,(int)vbuf->size);    
        invalid_argument(msg);
    }    

    return Val_int((unsigned int) vbuf->data[pos]);

};

/*
** Set a byte in a buffer.
*/

CAMLexport value
ext_buf_set(value valbuf,value valpos,value valbyte)
{
    struct caml_buf *vbuf=Buf_val(valbuf);
    int pos=Int_val(valpos);
    int byte=Int_val(valbyte);
    
    if (pos < 0 || pos >= vbuf->size)
    {
        sprintf(msg,"Bytebuf.buf_set: invalid pos (%d->%d)",
                (int)pos,(int)vbuf->size);    
        invalid_argument(msg);
    }    
    vbuf->data[pos] = (unsigned char)byte;

    return Val_unit;

};


/*
** Extract a string from a buffer.
*/

CAMLexport value
ext_buf_gets(value valbuf,value valpos,value valsize)
{
    CAMLparam0();
    CAMLlocal1(str);
    
    struct caml_buf *vbuf=Buf_val(valbuf);
    int pos = Int_val(valpos);
    int size = Int_val(valsize);
    
    if (pos < 0 || size < 0 || pos+size > vbuf->size)
    {
        sprintf(msg,"Bytebuf.buf_gets: invalid pos/size (%d,%d->%d)",
                (int)pos,size,(int)vbuf->size);    
        invalid_argument(msg);
    }    


    str = alloc_string(size);

    memcpy(String_val(str),&(vbuf->data[pos]),size);
    CAMLreturn(str);

};

/*
** Copy a string into a buffer.
*/

CAMLexport value
ext_buf_sets(value valbuf,value valpos,value valstr)
{
    struct caml_buf *vbuf=Buf_val(valbuf);
    int pos=Int_val(valpos);
    char *str=String_val(valstr);
    int size = string_length(valstr);
    
    if (pos < 0 || pos+size > vbuf->size)
    {
        sprintf(msg,"Bytebuf.buf_sets: invalid pos,size (%d,%d->%d)",
                (int)pos,(int)size,(int)vbuf->size);    
        invalid_argument(msg);
    }    
    memcpy(&(vbuf->data[pos]),str,size);

    return Val_unit;

};

/*
** Extract a char byte from a buffer.
*/

CAMLexport value
ext_buf_getc(value valbuf,value valpos)
{
    CAMLparam0();
    CAMLlocal1(str);
    
    struct caml_buf *vbuf=Buf_val(valbuf);
    int pos = Int_val(valpos);
    
    if (pos < 0 || pos >= vbuf->size)
    {
        sprintf(msg,"Bytebuf.buf_getc: invalid pos (%d->%d)",
                (int)pos,(int)vbuf->size);    
        invalid_argument(msg);
    }    


    CAMLreturn(Val_int((unsigned int)vbuf->data[pos]));

};

/*
** Copy a char byte into a buffer.
*/

CAMLexport value
ext_buf_setc(value valbuf,value valpos,value valc)
{
    struct caml_buf *vbuf=Buf_val(valbuf);
    int pos=Int_val(valpos);
    char c=Int_val(valc);
    
    if (pos < 0 || pos >= vbuf->size)
    {
        sprintf(msg,"Bytebuf.buf_setc: invalid pos (%d->%d)",
                (int)pos,(int)vbuf->size);    
        invalid_argument(msg);
    }    
    vbuf->data[pos]=c;

    return Val_unit;

};

/*
** Get the size of a buffer.
*/

CAMLexport value
ext_buf_len(value valbuf)
{
    struct caml_buf *vbuf=Buf_val(valbuf);

    return Val_int(vbuf->size);

};

CAMLexport value
ext_buf_info(value valbuf)
{
    CAMLparam0();
    CAMLlocal1(infostr);
    char str[200];
    int size;
    
    struct caml_buf *vbuf=Buf_val(valbuf);
    
    sprintf(str,"size=%d type=%s",
                (int)vbuf->size,
                ((vbuf->state & 0x30)== BUF_MASTER ? "Physical" : "Logical"));

    size = strlen(str);
    
    infostr = alloc_string(size);    

    memcpy(String_val(infostr),str,size);
    
    CAMLreturn(infostr);
};



/*
** String <-> Buffer functions
*/

CAMLexport value
ext_blit_bb(value valsrc,
            value valsrcpos,
            value valdst,
            value valdstpos, 
            value vallen)
{
    struct caml_buf *vsrcbuf = Buf_val(valsrc);
    struct caml_buf *vdstbuf = Buf_val(valdst);
    int srcpos = Int_val(valsrcpos);
    int dstpos = Int_val(valdstpos);
    int len = Int_val(vallen);

    if (srcpos < 0 || len < 0 || srcpos+len >  vsrcbuf->size) 
    {
        sprintf(msg,"Bytebuf.blit_bb: invalid srcbuf pos,len (%d<-%d,%d)",
                (int)vsrcbuf->size,(int)srcpos,(int)len);    
        invalid_argument(msg);
    };
    if (dstpos < 0 || dstpos+len >  vdstbuf->size) 
    {
        sprintf(msg,"Bytebuf.blit_bb: invalid dstbuf pos,len (%d<-%d,%d)",
                (int)vdstbuf->size,(int)dstpos,(int)len);    
        invalid_argument(msg);
    };

    memcpy(&((vdstbuf->data)[dstpos]),
           &((vsrcbuf->data)[srcpos]),
           len);

    return Val_unit;
}

CAMLexport value
ext_blit_bs(value valsrc,
            value valsrcpos,
            value valdst,
            value valdstpos, 
            value vallen)
{
    struct caml_buf *vsrcbuf = Buf_val(valsrc);
    char *vdstbuf = String_val(valdst);
    int srcpos = Int_val(valsrcpos);
    int dstpos = Int_val(valdstpos);
    int dstsize = string_length(valdst);
    int len = Int_val(vallen);

    if (srcpos < 0 || len < 0 || srcpos+len >  vsrcbuf->size) 
    {
        sprintf(msg,"Bytebuf.blit_bs: invalid srcbuf pos,len (%d<-%d,%d)",
                (int)vsrcbuf->size,(int)srcpos,(int)len);    
        invalid_argument(msg);
    };
    if (dstpos < 0 || dstpos+len >  dstsize) 
    {
        sprintf(msg,"Bytebuf.blit_bs: invalid dstbuf pos,len (%d<-%d,%d)",
                (int)dstsize,(int)dstpos,(int)len);    
        invalid_argument(msg);
    };

    memcpy(&(vdstbuf[dstpos]),
           &((vsrcbuf->data)[srcpos]),
           len);

    return Val_unit;
}

CAMLexport value
ext_blit_sb(value valsrc,
            value valsrcpos,
            value valdst,
            value valdstpos, 
            value vallen)
{
    char *vsrcbuf = String_val(valsrc);
    struct caml_buf *vdstbuf = Buf_val(valdst);
    int srcpos = Int_val(valsrcpos);
    int dstpos = Int_val(valdstpos);
    int srcsize = string_length(valsrc);
    int len = Int_val(vallen);

    if (srcpos < 0 || len < 0 || srcpos+len >  srcsize) 
    {
        sprintf(msg,"Bytebuf.blit_sb: invalid srcbuf pos,len (%d<-%d,%d)",
                (int)srcsize,(int)srcpos,(int)len);    
        invalid_argument(msg);
    };
    if (dstpos < 0 || dstpos+len >  vdstbuf->size) 
    {
        sprintf(msg,"Bytebuf.blit_sb: invalid dstbuf pos,len (%d<-%d,%d)",
                (int)vdstbuf->size,(int)dstpos,(int)len);    
        invalid_argument(msg);
    };

    memcpy(&((vdstbuf->data)[dstpos]),
           &(vsrcbuf[srcpos]),           
           len);

    return Val_unit;
}

CAMLexport value
ext_fill(value valbuf,
            value valpos,
            value vallen,
            value valfill)
{
    struct caml_buf *vbuf = Buf_val(valbuf);
    int pos = Int_val(valpos);
    int len = Int_val(vallen);
    int fill = Int_val(valfill);
    
    if (pos < 0 || len < 0 || pos+len >  vbuf->size) 
    {
        sprintf(msg,"Bytebuf.fill: invalid buf pos,len (%d<-%d,%d)",
                (int)vbuf->size,(int)pos,(int)len);    
        invalid_argument(msg);
    };

    memset(&((vbuf->data)[pos]),
           fill,
           len);

    return Val_unit;
}

static void
buf_finalize (value v)
{
    struct caml_buf *vbuf=Buf_val(v);

    DPRINTF(1,("buf_finalize: %x [state=%x,size=%d]\n",(int)vbuf,
                vbuf->state,vbuf->size));

    
    if (vbuf->state & BUF_SLAVE)
    {
        DPRINTF(1,("... removing BUF_SLAVE (next=%x,prev=%x,prev->next=%x)\n",
                (int)vbuf->next,
                (int)vbuf->prev,
                (int)(vbuf->prev)->next));
        
        
        (vbuf->prev)->next = vbuf->next;
        if(vbuf->next != NULL)
        {
            (vbuf->next)->prev = vbuf->prev;
        }   
        /*
        ** If this slave is the last one, and the master
        ** was marked BUF_DYING, free the master and the data.
        */
        if ((vbuf->prev)->next == NULL &&
            (vbuf->prev)->state & BUF_DYING)
        {
            DPRINTF(1,("... freeing dying BUF_MASTER\n"));
            free((vbuf->prev)->data);
            free(vbuf->prev);
        }     
    }
    else
    {
        if (vbuf->next == NULL)
        {
            DPRINTF(1,("... freeing BUF_MASTER\n"));
            
            /*
            ** Simple. No slaves anymore. Free the data.
            */
            free(vbuf->data);
        } 
        else
        {
            struct caml_buf *vbuf_c; 
        
            DPRINTF(1,("... marking BUF_DYING (next=%x,prev=%x,next->prev=%x)\n",
                (int)vbuf->next,
                (int)vbuf->prev,
                (int)(vbuf->next)->prev));
            /*
            ** Not so simple. There are slaves, so we can't free
            ** the data here. But the vbuf structure will be freed
            ** by the GC. So we must copy and save it.
            */
            
            vbuf_c = (struct caml_buf *)malloc(
                            sizeof(struct caml_buf));
            if (vbuf_c == NULL)
            {          
                /* Emergency stop */
                exit(1);
            }
                              
            vbuf_c->state = BUF_MASTER | BUF_DYING;
            vbuf_c->data = vbuf->data;
            vbuf_c->size = vbuf->size;
            
            vbuf_c->next = vbuf->next;
            vbuf_c->prev = (struct caml_buf *)NULL;
         
            (vbuf->next)->prev = vbuf_c;                   

            DPRINTF(1,("new vbuf=%x\n",(int)vbuf_c));
        }
    }
    DPRINTF(1,("Done.\n"));
};            


static int
buf_compare(value v1,value v2)
{
    return -1;
};

static long
buf_hash(value v)
{
    return 0;
};

static void
buf_serialize(value v,
              unsigned long * wsize_32,
              unsigned long * wsize_64)
                                                              
{
    return;
};

unsigned long
buf_deserialize(void *dst)
{
    return 0;
};

#include <io.h>


/*
** Buffer file read and write operations
*/

CAMLexport value
ext_input_buf_n (value vchannel,
              value vbuf,
              value vstart,
              value vlength)
{
    struct channel * channel = Channel(vchannel);
    struct caml_buf *buf=Buf_val(vbuf);
    int size = Int_val(vlength);
    int pos  = Int_val(vstart);
    int n;

    /*
    ** buffer bounding checks
    */
    if (size < 0 || (size+pos) > buf->size) 
    {
        sprintf(msg,"Bytebuf.input_buf: invalid pos,size (%d<-%d,%d)",
                (int)buf->size,(int)pos,(int)size);    
        invalid_argument(msg);
    };
    
    n=getblock(channel,&buf->data[pos],size);
    return Val_int(n);
};


CAMLexport value
ext_output_buf_n (value vchannel,
               value vbuf,
               value vstart,
               value vlength)
{
    struct channel * channel = Channel(vchannel);
    struct caml_buf *buf=Buf_val(vbuf);
    int size = Int_val(vlength);
    int pos  = Int_val(vstart);
    int n;

    /*
    ** buffer bounding checks
    */
    if (size < 0 || (size+pos) > buf->size) 
    {
        sprintf(msg,"Bytebuf.output_buf: invalid pos,size (%d<-%d,%d)",
                (int)buf->size,(int)pos,(int)size);    
        invalid_argument(msg);
    };
    
    n=putblock(channel,&buf->data[pos],size);
    return Val_int(n);
};

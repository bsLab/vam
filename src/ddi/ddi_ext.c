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
**    $INITIAL:     (C) 2005-2006 BSSLAB
**    $CREATED:     9.1.2005
**    $VERSION:     1.05
**
**    $INFO:
**
**  Device Driver Interface for Amoeba. Only implemented in the vamrun
**  version running on the top of a native Amoeba kernel (AMOEBA_RAW).
**
**
**
**    $ENDOFINFO
**
*/

/* #define DPRINTF_LEVEL 1 */


#include <mlvalues.h>
#include <alloc.h>
#include <custom.h>
#include <callback.h>
#include <memory.h>
#include <signals.h>
#include <fail.h>
#include <stdio.h>

#include "buf.h"
#include "machtype.h"

#include <amoeba.h>
#include <stderr.h>

#include <sys/debug.h>
#include <module/ar.h>

#include <config.h>

#include <string.h>

#ifdef AMOEBA_RAW
#include <sys/iomap.h>
#include <ioport.h>
#include <sys/utimer.h>
#include <sys/isr.h>
#else
#define MAX_DEVNAME_LEN 40

struct isr_handler 
{
    int     irq;            /* IRQ number */
    int     flags;          /* ISR flags  */
    char            devname[MAX_DEVNAME_LEN];    
    event           ev;          
    unsigned long   id;          
};
typedef struct isr_handler isr_handler_t,*isr_handler_p;
#define IRQ_NORMAL      0    
#define IRQ_SHARED      1
#endif
/*
** Convert capability from ML to C
*/
static void tocap(value vcap,capability *cap)
{
    int obj_val;

    value vport = Field(vcap,0);
    value vpriv_obj = Field(Field(vcap,1),0);
    value vpriv_rig = Field(Field(vcap,1),1);
    value vpriv_ran = Field(Field(vcap,1),2);
    
    memcpy(&cap->cap_port,String_val(Field(vport,0)),PORTSIZE);

    obj_val = Int_val(Field(vpriv_obj,0));        
    cap->cap_priv.prv_object[0]=obj_val &0xff;
    cap->cap_priv.prv_object[1]=(obj_val >> 8)&0xff;
    cap->cap_priv.prv_object[2]=(obj_val >> 16)&0xff;
    cap->cap_priv.prv_rights = Int_val(Field(vpriv_rig,0));

    memcpy(&cap->cap_priv.prv_random,
           String_val(Field(vpriv_ran,0)),PORTSIZE);

        
    return;
};

/*
** Check an IO port region
** vstart, vsize: Machtype.int32
*/

CAMLexport value
ext_io_check_region (value vstart, value vsize,value vcap)
{
    CAMLparam0();
    CAMLlocal1(res);

    int start,size;
    capability syscap;
    errstat stat=STD_OK;

    UCHAR *us1=(unsigned char*)String_val(vstart);
    UCHAR *us2=(unsigned char*)String_val(vsize);

    start=INT32_R(MACHDATA(us1));
    size=INT32_R(MACHDATA(us2));

    tocap(vcap,&syscap);

    DPRINTF(1,("ext_io_check_region: start=%x size=%x syscap=%s\n",
                start,size,ar_cap(&syscap)));

#ifdef AMOEBA_RAW
    stat=io_check_region(start,size,&syscap);
#endif

    res=alloc_tuple(1);
    Store_field(res,0,Val_int(stat));
    CAMLreturn(res);

};

/*
** Map an IO port region 
** vstart, vsize: Machtype.int32
*/

CAMLexport value
ext_io_map_region (value vstart, value vsize,value vname,value vcap)
{
    CAMLparam0();
    CAMLlocal1(res);

    int start,size;
    capability syscap;
    errstat stat=STD_OK;

    char *devname=String_val(vname);

    UCHAR *us1=(unsigned char*)String_val(vstart);
    UCHAR *us2=(unsigned char*)String_val(vsize);

    start=INT32_R(MACHDATA(us1));
    size=INT32_R(MACHDATA(us2));

    tocap(vcap,&syscap);

    DPRINTF(1,("ext_io_map_region: start=%x size=%x devname=%s syscap=%s\n",
                start,size,devname,ar_cap(&syscap)));

#ifdef AMOEBA_RAW
    stat=io_map_region(start,size,devname,&syscap);
#endif

    res=alloc_tuple(1);
    Store_field(res,0,Val_int(stat));
    CAMLreturn(res);

};

/*
** Unmap an IO port region
** vstart, vsize: Machtype.int32
*/

CAMLexport value
ext_io_unmap_region (value vstart, value vsize,value vcap)
{
    CAMLparam0();
    CAMLlocal1(res);

    int start,size;
    capability syscap;
    errstat stat=STD_OK;

    UCHAR *us1=(unsigned char*)String_val(vstart);
    UCHAR *us2=(unsigned char*)String_val(vsize);

    start=INT32_R(MACHDATA(us1));
    size=INT32_R(MACHDATA(us2));

    tocap(vcap,&syscap);

    DPRINTF(1,("ext_io_unmap_region: start=%x size=%x syscap=%s\n",
                start,size,ar_cap(&syscap)));

#ifdef AMOEBA_RAW
    stat=io_unmap_region(start,size,&syscap);
#endif

    res=alloc_tuple(1);
    Store_field(res,0,Val_int(stat));
    CAMLreturn(res);

};


/*
** Output a byte value to an IO port. The port address must be mapped
** in the process.
*/

CAMLexport value
ext_io_out_byte (value vaddr, value vval)
{
    int addr,val;

    UCHAR *us1=(unsigned char*)String_val(vaddr);
    UCHAR *us2=(unsigned char*)String_val(vval);

    addr=INT32_R(MACHDATA(us1));
    val=INT32_R(MACHDATA(us2));


    DPRINTF(100,("ext_io_out_byte: addr=%x val=%x\n",
                addr,val));

#ifdef AMOEBA_RAW
    out_byte(addr,val);
#else
    printf("ext_io_out_byte: addr=%x val=%x\n",
                addr,val);
#endif


    return Val_unit;
};



/*
** Read a byte value from an IO port. The port address must be mapped
** in the process. The returned value if from type Machtype.int32!
*/

CAMLexport value
ext_io_in_byte (value vaddr)
{
    CAMLparam0();
    CAMLlocal1(res);
 
    UCHAR *data=(UCHAR)NULL;
    int addr,val=0;

    UCHAR *us1=(unsigned char*)String_val(vaddr);
    addr=INT32_R(MACHDATA(us1));


    DPRINTF(100,("ext_io_in_byte: addr=%x\n",
                addr));

#ifdef AMOEBA_RAW
    val=in_byte(addr);
#else
    printf("ext_io_in_byte: addr=%x\n",
                addr);
#endif

    res=alloc_string(1+SIZEOF_INT);
    data=(UCHAR*)String_val(res);
    MACHTYPE_W(data,INT32);
    INT32_W(MACHDATA(data),val);

    CAMLreturn(res);

};


/*
** Read len bytes from an IO port and write values into buffer. 
** The port address must be mapped in the process. 
*/

CAMLexport value
ext_io_in_bytes (value vaddr,value vlen,value vbuf)
{
    struct caml_buf *buf=Buf_val(vbuf);
    char *str=buf->data;
    UCHAR *data=(UCHAR)NULL;

    UCHAR *us1=(unsigned char*)String_val(vaddr);
    int addr=INT32_R(MACHDATA(us1));
    UCHAR *us2=(unsigned char*)String_val(vlen);
    int len=INT32_R(MACHDATA(us2));


    DPRINTF(100,("ext_io_in_bytes: addr=%x len=%x\n",
                addr,len));

#ifdef AMOEBA_RAW    
    ins_byte(addr,str,len);
#else
    printf("ext_io_in_bytes: addr=%x len=%x\n",
                addr,len);
#endif

    return Val_unit;
};


/*
** Init and reinit a software timer.
*/

#define Event_val(v) (* ((event **) Data_custom_val(v)))

CAMLexport value
ext_timer_init (value vev, value vint, value vunit, value vonce,
                value vreinit)
{

    interval tint=Int_val(vint);
    int      tuni=Int_val(vunit);
    int      tonce=Int_val(vonce);
    int      reinit=Int_val(vreinit);
    event    *ev = Event_val(vev);
    int res;

    DPRINTF(1,("ext_timer_init: ev=%x interval=%d unit=%d once=%d\n",
                (int)ev,(int)tint,tuni,tonce));

#ifdef AMOEBA_RAW
    if (reinit==0)
        res=timer_init((timer_event *)ev,tint,tuni,tonce);
    else
        res=timer_reinit((timer_event *)ev,tint,tuni,tonce);
#endif

    return Val_int(res);
};


/*
** Wait for the timer event
*/
CAMLexport value
ext_timer_await (value vev)
{

    event    *ev = Event_val(vev);
    int res;

    DPRINTF(100,("ext_timer_await: ev=%x\n",
                (int)ev));

#ifdef AMOEBA_RAW
    res=timer_await((timer_event *)ev);
#endif

    return Val_int(res);
};


/*
** User level interrupt handlers
*/

#define Isr_val(v) (*((isr_handler_p *) Data_custom_val(v)))

static void
isr_finalize (value v)
{
  isr_handler_p isr=Isr_val(v);
  
};
static int
isr_compare(value v1,value v2)
{
    return -1;
};

static long
isr_hash(value v)
{
    return 0;
};
static void
isr_serialize(value v,
              unsigned long * wsize_32,
              unsigned long * wsize_64)
                                                              
{
    return;
};

unsigned long
isr_deserialize(void *dst)
{
    return 0;
};
        


static struct custom_operations isr_ops = 
{
    "_ddi_isr",
    isr_finalize,
    isr_compare,
    isr_hash,
    isr_serialize,
    isr_deserialize
};                        

/*
** Register an interrupt handler
*/
CAMLexport value
ext_interrupt_register (value irq,value flags,value vname,value vcap)
{
    CAMLparam0();
    CAMLlocal3(visr,ret,vstat);
    isr_handler_p isr;
    
    errstat err=STD_COMBAD;      
    capability syscap;
    
    isr = (isr_handler_p)malloc(sizeof (isr_handler_t));
    if (isr == NULL)
      failwith("interrupt_register: no space\n");

    tocap(vcap,&syscap);
    isr->irq=Int_val(irq);
    if (Int_val(flags) == 0)
    {
      DPRINTF(0,("ext_interrupt_register: flags=IRQ_NORMAL\n"));
      isr->flags=IRQ_NORMAL;
    }
    else
    {
      DPRINTF(0,("ext_interrupt_register: flags=IRQ_SHARED\n"));
      isr->flags=IRQ_SHARED;
    };      
    strcpy(isr->devname,String_val(vname));


    visr = alloc_custom(&isr_ops,
                     sizeof(isr_handler_p),
                     1, 100);

    *((isr_handler_p *)Data_custom_val(visr)) = isr;

#ifdef AMOEBA_RAW
    enter_blocking_section();            
    err=interrupt_register(isr,&syscap);
    leave_blocking_section();
#endif
 
    ret=alloc_tuple(2);     
    vstat=alloc_tuple(1);
    Store_field(vstat,0,Val_int((int)err));                            
    Store_field(ret,0,vstat);
    Store_field(ret,1,visr);

    CAMLreturn(ret);
    
};

/*
** Unregister previously allocated ISR
*/
CAMLexport value
ext_interrupt_unregister (value visr,value vcap)
{
    CAMLparam0();
    CAMLlocal1(vstat);
    
    isr_handler_p isr=Isr_val(visr);
    errstat err=STD_COMBAD;
    capability syscap;

    tocap(vcap,&syscap);
  
#ifdef AMOEBA_RAW
    enter_blocking_section();
    err = interrupt_unregister(isr,&syscap);
    leave_blocking_section();
#endif  
  
    vstat=alloc_tuple(1);
    Store_field(vstat,0,Val_int((int)err));
    
    CAMLreturn(vstat);
};

/*
** Wait for interrupt event...
*/
CAMLexport value
ext_interrupt_await (value visr,value tmo)
{
    CAMLparam0();
    CAMLlocal1(vstat);
    
    isr_handler_p isr=Isr_val(visr);
    errstat err=STD_COMBAD;
  
#ifdef AMOEBA_RAW
    enter_blocking_section();
    err = interrupt_await(isr->irq,isr->ev,Int_val(tmo));
    leave_blocking_section();
#endif  
  
    vstat=alloc_tuple(1);
    Store_field(vstat,0,Val_int((int)err));
    
    CAMLreturn(vstat);
};

/*
** Acknowledge IRQ...
*/
CAMLexport value
ext_interrupt_done (value visr,value done)
{
    CAMLparam0();
    CAMLlocal1(vstat);
    
    isr_handler_p isr=Isr_val(visr);
    errstat err=STD_COMBAD;
  
    
#ifdef AMOEBA_RAW
    enter_blocking_section();
    if (Int_val(done) == 0)
      err = interrupt_done(isr->irq,IRQ_SERVICED);
    else
      err = interrupt_done(isr->irq,IRQ_UNKNOWN);
    leave_blocking_section();
#endif  
  
    vstat=alloc_tuple(1);
    Store_field(vstat,0,Val_int((int)err));
    
    CAMLreturn(vstat);
};


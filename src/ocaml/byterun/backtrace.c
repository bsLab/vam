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
**    $INITIAL:     (C) 2000 INRIA
**    $CREATED:     2001.12.07
**    $MODIFIED:    2003.11.08
**    $VERSION:     1.19
**
**    $INFO:
**
**      Stack backtrace for uncaught exceptions 
**
**    $ENDOFINFO
**
*/


#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include "config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif


#include "mlvalues.h"
#include "alloc.h"
#include "io.h"
#include "instruct.h"
#include "intext.h"
#include "exec.h"
#include "fix_code.h"
#include "startup.h"
#include "stacks.h"
#include "sys.h"
#include "backtrace.h"

CAMLexport int backtrace_active = 0;
CAMLexport int backtrace_pos = 0;
CAMLexport int backtrace_self=0;
CAMLexport int backtrace_verbose=0;

CAMLexport code_t * backtrace_buffer = NULL;
CAMLexport value backtrace_last_exn = Val_unit;

/*
** The debug event table. One entry for one module.
*/

static int              event_table_entries=0;
static int              event_self_init=0;
struct event_module     event_table[MAX_MODULES]; 

/*
** The code segment table
*/
struct code_seg         code_table[MAX_MODULES];
static int              code_table_entries=0;


static int              backtrace_init=0;

#define BACKTRACE_BUFFER_SIZE 1024

/*
** Location of fields in the Instruct.debug_event record 
*/
enum { 
    EV_POS = 0,
    EV_MODULE = 1,
    EV_CHAR = 2,
    EV_KIND = 3, 
    EV_STACKSIZE = 7
};

extern code_t exc_ccall_pc;

/*
** Initialize the backtrace machinery 
*/

static char *code_name_core="!!!###BYTECODE###CORE____%&!!!";
static code_t end_code;

void init_backtrace(void)
{
  
    if(backtrace_init==0)
    {

        end_code=(code_t) ((char *) start_code + code_size);

        register_global_root(&backtrace_last_exn);

        /*
        ** Note: lazy initialization of backtrace_buffer in stash_backtrace
        ** to simplify the interface with the thread libraries 
        */

        code_table[code_table_entries].code_name=code_name_core;
        code_table[code_table_entries].pc_start=&start_code;
        code_table[code_table_entries].pc_end=&end_code;
        code_table[code_table_entries].seg_num=1;
    
        code_table_entries++;

        backtrace_init=1;
    }
  
    return;
}

/*
** Store the return addresses contained in the given stack fragment
** into the backtrace array 
*/

void stash_backtrace(value exn, code_t pc, value * sp)
{

    if (pc != NULL) pc = pc - 1;
    if (exn != backtrace_last_exn) 
    {
        backtrace_pos = 0;
        backtrace_last_exn = exn;
    }

    if (backtrace_buffer == NULL) 
    {
        backtrace_buffer = malloc(BACKTRACE_BUFFER_SIZE * sizeof(code_t));
        if (backtrace_buffer == NULL) return;
    }

    if (backtrace_pos >= BACKTRACE_BUFFER_SIZE) return;

    backtrace_buffer[backtrace_pos++] = pc;
    for (/*nothing*/; sp < trapsp; sp++) 
    {
        code_t p = (code_t) *sp;

        if (backtrace_pos >= BACKTRACE_BUFFER_SIZE) break;
        backtrace_buffer[backtrace_pos++] = p;
    }
    return;  
}

/*
** Read the debugging info contained in the current bytecode executable.
** Store events in the event table, one module for one table slot.
*/

#ifndef O_BINARY
#define O_BINARY 0
#endif

static int read_debug_info_self(void)
{
    char * exec_name;
    char * modname;
    int fd;
    int evc;
    struct exec_trailer trail;
    struct channel * chan;
    uint32 num_events, orig, i;
    value evl, l;
    struct event_module *em;
  
    if(event_self_init == 1)
        return 0;

    print_stderr("Reading debug info for current bytecode...\n"); 
    exec_name = caml_main_argv[0];
    fd = attempt_open(&exec_name, &trail, 1);

    if (fd < 0) 
        return -1;
    read_section_descriptors(fd, &trail);
    if (seek_optional_section(fd, &trail, "DBUG") == -1) 
    {
        close(fd);
        return -1;
    } 
  
    chan = open_descriptor_in(fd);
    num_events = getword(chan);

    for (i = 0; i < num_events; i++) 
    {
        int modevents=0;
    
        orig = getword(chan);
        evl = input_val(chan);

        /*
        ** Determine length of list
        */
        for (l = evl,modevents=0; 
             l != Val_int(0); 
             l = Field(l, 1), modevents++); 

        if(modevents > 0)
        {
            em=&(event_table[event_table_entries]);
            
            em->evl=(struct event_info *)malloc(sizeof(struct event_info)*
                                                modevents);
            if(em->evl == NULL)
            {
                close_channel(chan);
                return -1;
            }
            else
                event_table_entries++;
            em->evl_length = modevents;
        }

    
        /*
        ** Relocate events in event list and store results in event table.
        */
        evc = 0;
    
        for (l = evl; l != Val_int(0); l = Field(l, 1)) 
        {
            int lpos,cpos;
            
            value ev = Field(l, 0);
            Field(ev, EV_POS) = Val_long(Long_val(Field(ev, EV_POS)) 
                                + orig + (long)start_code);

            lpos = Int_val(Field(ev,EV_CHAR)) / 1000;
            cpos = Int_val(Field(ev,EV_CHAR)) - lpos*1000;

        
            em->evl[evc].pc_ev = (code_t)(Long_val(Field(ev,EV_POS)));

            em->evl[evc].line_pos = lpos;
            em->evl[evc].char_pos = cpos;           
            em->evl[evc].stack_size = Int_val(Field(ev,EV_STACKSIZE));
            if(evc==0)
            {
                char    *modname = String_val(Field(ev,EV_MODULE));
                int namelen = string_length(Field(ev,EV_MODULE));
                em->module_name = (char *)malloc(namelen+1);
                if (em->module_name == NULL)
                {
                    close_channel(chan);
                    return -1;
                }                                                                

                if (backtrace_verbose)
                    print_stderr("Module name: %s(%d) (%d events)\n",modname,
                                 (event_table_entries-1),modevents);

                strncpy(em->module_name,modname,namelen);
                em->module_name[namelen] = '\000';
                                               
            }                                                    

            if (backtrace_verbose)
                print_stderr("  pc=%x line=%d\n",
                            em->evl[evc].pc_ev);
            evc++;                                                                         
        }
    }
    close_channel(chan);

  
    event_self_init=1;

    return 0;
}



/*
** Search the event for the given PC.  Return Val_false if not found. 
*/

struct event_module *
event_for_location(code_t pc)
{
    code_t pos;
    int modi,evi;
    struct event_module *emthis;
    unsigned long window=0;

    emthis = (struct event_module *)malloc(sizeof(struct event_module));

    if(emthis==NULL)
        return NULL;

    pos = pc;
 

    for (modi=0;modi< event_table_entries;modi++)
    {
        struct event_module *em=&(event_table[modi]);

        if (backtrace_verbose)
            print_stderr("searching %s(%d) (%d events)...\n",em->module_name,
                modi,em->evl_length);

        for (evi=0;evi<em->evl_length;evi++)
        {
            struct event_info *ev=&((*em).evl)[evi]; 
            /*
            ** GCC (64bit) problems under Linux?
            */
            unsigned long p1=((unsigned long)ev->pc_ev) & 0xFFFFFFF;
            unsigned long p2=((unsigned long)pos)       & 0xFFFFFFF;

            if (p1 == p2) 
            {
                emthis->module_name = em->module_name;
                emthis->evl_length  = 1;
                emthis->evl         = ev;
                return emthis;
            }
        }
    }

    /*
    ** Perhaps exception from C call pc frame ?
    ** ext_ccall_pc doesn't have an own closure. 
    ** Or an on the fly closure not marked with a debug frame!
    ** Try to find the closest closure around. Fuzzy logic, really!
    ** The search window is increased on each failed lookup untill
    ** a painfull threshold is reached.    
    */

    for(window=4;window < 20;window=window+4)
    {
      for (modi=0;/* exc_ccall_pc != NULL && */ modi< event_table_entries;modi++)
      {
        struct event_module *em=&(event_table[modi]);

        for (evi=0;evi<em->evl_length;evi++)
        {
            struct event_info *ev=&((*em).evl)[evi]; 
            /*
            ** GCC (64bit) problems under Linux?
            */
            unsigned long p1=(unsigned long)ev->pc_ev & 0xFFFFFFF;
            unsigned long p2=(unsigned long)pos & 0xFFFFFFF;


            if (((p2 >= p1) && (p2 < p1 + window)) ||
                ((p2 >= p1 - window) && (p2 < p1))) 
            {
                emthis->module_name = em->module_name;
                emthis->evl_length  = 1;
                emthis->evl         = ev;
                return emthis;
            };
        };
      };
    };

    /*
    ** Really  unknown.
    */
  
    return NULL;
}

/*
** Print the location corresponding to the given PC 
*/


static void print_location(int index)
{

    int i,n;
    code_t pc = backtrace_buffer[index];
    code_t p=NULL;
  
    char * info;
    struct event_module *ev;

    if (pc == NULL) 
    {
        print_stderr("Raised from a C function\n");
        /*
        ** The closure from where the C function was called.
        ** Print this location, too.
        */
        pc = exc_ccall_pc; 
    }
    
    if (pc != NULL)
    {
        /*
        ** Is pc a pointer within in a known code segemnt ?
        */
    
        for(i=0;i<code_table_entries;i++)
        {
            struct code_seg cs=code_table[i];

            for (n=0;n<cs.seg_num;n++)
            {
                code_t ps = cs.pc_start[n];
                code_t pe = cs.pc_end[n];

                if (pc >= ps && pc < pe) 
                    p=pc;
            }
        }
    }
    if(p==NULL)
        return;
    
    ev = event_for_location(pc);

    if (exc_ccall_pc == pc)
        exc_ccall_pc = NULL;
    
    if (is_instruction(*pc, RAISE)) 
    {
        /*
        ** Ignore compiler-inserted raise 
        */
        if (ev == NULL) return;
        /*
        ** Initial raise if index == 0, re-raise otherwise 
        */
        if (index == 0)
            info = "Raised at";
        else
          info = "Re-raised at";
    } 
    else 
    {
        info = "Called from";
    }

    if (ev == NULL && backtrace_self==1) 
    {
        print_stderr("=> %s unknown location [%x]\n", info, (int)pc);
    } 
    else if (ev != NULL)
    {
        int lpos = ev->evl->line_pos;
        int cpos = ev->evl->char_pos;
        print_stderr("=> %s module %s: line %d character %d\n",
                info,
                ev->module_name,
                lpos,cpos);
    }
}

/*
** Debug support for dynamically loaded compilation units (aka '.cmo' files)
** and on the fly compiled scripts. 
** Add all event locations for this module together with the source
** module name and the line/char positions to the 'event_table'.
** Argument 'evl' is an array of event arrays, and 'codel' an array of
** code pointern for each evl subarray.
*/


CAMLprim value debug_add_compunit(value evl,value codel) 
{
    int evi,evs,evc,csi;
    int subevents;
    int totevents;
    int events = Wosize_val(evl);
    int codes  = Wosize_val(codel);
    int found_ev=-1;
    int found_cs=-1;
    char *modname="";
        
    long code_start;
    long code_end;
        
    struct event_module *em;
    struct code_seg     *cs;
    
    if(events <= 0)
        return Val_unit;    /* ?? */

    if(events != codes)
        return Val_unit;    /* ?? */
        

    /*
    ** Find out the amount of total events
    */ 

    totevents = 0;
    for(evs=0;evs<events;evs++)
    {
        value evs_l = Field(evl,evs);
        /*
        ** Get the module name
        */
        if(Wosize_val(evs_l)>0)
        {
            value ev=Field(evs_l,0);
            modname = String_val(Field(ev,EV_MODULE));
        }

        totevents = totevents + Wosize_val(evs_l);     
    }

    if(totevents == 0)
        return Val_unit;
   
    /*
    ** Search the event table for an already existing
    ** module name and reuse this table slot.
    */

    for(evi=0;evi<event_table_entries;evi++)
    {
        int len1,len2;        
        em=&(event_table[evi]);

        len1=strlen(modname);
        len2=strlen(em->module_name);

        if((len1 == len2) && 
           (strcmp(modname,em->module_name)==0))
            found_ev=evi; 
    }

    if(found_ev < 0)
    {
        em=&(event_table[event_table_entries]);
        
        em->evl=(struct event_info *)malloc(sizeof(struct event_info)*totevents);
        if(em->evl == NULL)
        {
            return Val_unit;
        }
        else
            event_table_entries++;
    }
    else
    {
        em=&(event_table[found_ev]);
        free(em->evl);
        free(em->module_name);
        
        em->evl=(struct event_info *)malloc(sizeof(struct event_info)*totevents);
        if(em->evl == NULL)
        {
            return Val_unit;
        }
        
    }
    
    em->evl_length = totevents;
    evc = 0;

    /*
    ** Search the code table for an already existing
    ** module name and reuse this table slot.
    */

    for(csi=0;csi<code_table_entries;csi++)
    {
        int len1,len2;        
        cs=&(code_table[csi]);

        len1=strlen(modname);
        len2=strlen(cs->code_name);


        if((len1 == len2) && 
           (strcmp(modname,cs->code_name) == 0))
        {
            found_cs=csi;
        }
    
    }


    if(found_cs < 0)
    {
        cs=&code_table[code_table_entries];

        cs->code_name=(char *)malloc(strlen(modname)+1);
        if(cs->code_name==NULL)
            return Val_unit;
        cs->pc_start=(code_t *)malloc(sizeof(code_t)*codes);
        cs->pc_end=(code_t *)malloc(sizeof(code_t)*codes);
        if(cs->pc_start==NULL || cs->pc_end==NULL)
            return Val_unit;
        strcpy(cs->code_name,modname);    
        cs->seg_num=codes;

        code_table_entries++;

    }
    else
    {
        cs=&code_table[found_cs];
        
        free(cs->code_name);
        free(cs->pc_start);
        free(cs->pc_end);
        
        cs->code_name=(char *)malloc(strlen(modname)+1);
        if(cs->code_name==NULL)
            return Val_unit;
        cs->pc_start=(code_t *)malloc(sizeof(code_t)*codes);
        cs->pc_end=(code_t *)malloc(sizeof(code_t)*codes);
        if(cs->pc_start==NULL || cs->pc_end==NULL)
            return Val_unit;
        strcpy(cs->code_name,modname);    
        cs->seg_num=codes;
    }
    
    
    for (evs=0;evs<events;evs++)
    {    
        value evs_l = Field(evl,evs);
        
        code_start = (long)String_val(Field(Field(codel,evs),0));
        code_end   = code_start + 
                     (long)(Field(Field(codel,evs),1));

        subevents = Wosize_val(evs_l);

        cs->pc_start[evs] = (code_t)code_start;
        cs->pc_end[evs] = (code_t)code_end;
        
        for(evi=0;evi<subevents;evi++)
        {
            int lpos,cpos;
            value ev=Field(evs_l,evi);
        
            if(evi == 0)
            {
                char    *modname = String_val(Field(ev,EV_MODULE));
                int     namelen = string_length(Field(ev,EV_MODULE));

                em->module_name = (char *)malloc(namelen+1);
                if (em->module_name == NULL)
                    return Val_unit;   

                strncpy(em->module_name,modname,namelen);
                em->module_name[namelen] = '\000';
            
            };
            lpos = Int_val(Field(ev,EV_CHAR)) / 1000;
            cpos = Int_val(Field(ev,EV_CHAR)) - lpos*1000;
        
            em->evl[evc].pc_ev = (code_t)(Long_val(Field(ev,EV_POS))+code_start);
            em->evl[evc].line_pos = lpos;
            em->evl[evc].char_pos = cpos;           
            
            evc++;

        };
    };

    return Val_unit;
}


/*
** Print a backtrace 
*/

CAMLprim void print_exception_backtrace(void)
{
    value events;
    int i;

    print_stderr("Exception backtrace:\n");

    if (backtrace_self)
        read_debug_info_self ();


    if (event_table_entries == 0)
        return;

    for (i = 0; i < backtrace_pos; i++)
        print_location(i);
}

/*
** Print the current backtrace frame. 
*/
 
CAMLprim value print_backtrace(value v)  
{
    if(backtrace_active==0)
        return Val_unit;	

    if(backtrace_pos==0)
        return Val_unit;

    print_exception_backtrace();
  
    return Val_unit;  
}

/*
** Set the current backtrace mode:
**
** 0: disables the backtrace support
** 1: enables the backtrace support
** 2: disable backtracing of core bytcode (only modules and phrases)
** 3: enable backtracing of core bytecode (+ module and phrases)
**
*/

CAMLprim value print_backtrace_mode(value v) 
{
    int mode=Int_val(v);

    switch(mode)
    {
        case 0:
                backtrace_active=0;
                backtrace_pos=0;
                break;
        case 1:
                backtrace_active=1;
                backtrace_pos=0;
                break;
           
        case 2:
                backtrace_self=0; 
                break;
        case 3:
                backtrace_self=1;
                break;
        default:
                break;
    } 
  
    return Val_unit;  
};

/*
** Read debug info for the current bytecode - independent of
** backtrace settings - needed for debugging
*/
void init_debuginfo()
{
    read_debug_info_self ();
    return;
};


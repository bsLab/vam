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
**    $AUTHORS:     
**    $INITIAL:     (C)
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.03
**
**    $INFO:
**
**  VAM Semaphores mapped to AMUTHR.                                                                     
**
**    $ENDOFINFO
**
*/


                                                                     



#include <amoeba.h>
#include <errno.h>
#include <string.h>
#include <thread.h>
#include <module/mutex.h>
#include <semaphore.h>

#include <signal.h>
#include <sys/time.h>
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/io.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/signals.h"

#include "caml/stacks.h"
#include "caml/sys.h"

#include <stdio.h>

#define Max_sema_number 1000
#define Sema_val(v) (* ((semaphore **) Data_custom_val(v)))

/*
** Semaphore operations
*/
static void caml_sema_finalize(value wrapper)
{
    semaphore * sem = Sema_val(wrapper);
    stat_free(sem);
    return;
}
static int caml_sema_condition_compare(value wrapper1, value wrapper2)
{
    semaphore * sem1 = Sema_val(wrapper1);
    semaphore * sem2 = Sema_val(wrapper2);
    return sem1 == sem2 ? 0 : sem1 < sem2 ? -1 : 1;
}

static struct custom_operations caml_sema_ops = {
  "_semaphore",
  caml_sema_finalize,
  caml_sema_condition_compare,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};


CAMLprim value am_sema_create(value vlevel)
{
    CAMLparam0();
    CAMLlocal1(sem);

    int lev=Int_val(vlevel);
    semaphore *s;

    s=stat_alloc(sizeof(semaphore));
    sema_init(s,lev);

    sem = alloc_custom(&caml_sema_ops, sizeof(semaphore *),
                       1, Max_sema_number);

    Sema_val(sem)=s;
    CAMLreturn(sem);
};

CAMLprim value am_sema_down(value vsema)
{
    semaphore *s = Sema_val(vsema);

    enter_blocking_section();
      sema_down(s);
    leave_blocking_section();

    return Val_unit;
};

CAMLprim value am_sema_trydown(value vsema,value vdel)
{
    int ret;
    int del=Int_val(vdel);
    semaphore *s = Sema_val(vsema);

    enter_blocking_section();
      ret=sema_trydown(s,del);
    leave_blocking_section();

    if (ret >= 0)
        return Val_true;
    else
        return Val_false;
    
};

CAMLprim value am_sema_up(value vsema)
{
    semaphore *s = Sema_val(vsema);

    enter_blocking_section();
      sema_up(s);
    leave_blocking_section();

    return Val_unit;
};

CAMLprim value am_sema_print(value vsema)
{
    CAMLparam0();
    CAMLlocal1(str);

    semaphore *s = Sema_val(vsema);
    int lev=sema_level(s);
    char tmp[100];

    sprintf(tmp,"semaphore level=%d owner=%d blocked threads=%d",
            lev,
            (int)MU_OTID(s->sema_lock),
            (int)MU_CNT(s->sema_lock));

    str=alloc_string(strlen(tmp)+1);
    strcpy(String_val(str),tmp);
    
    CAMLreturn(str);    
};

CAMLprim value am_sema_level(value vsema)
{
    int ret;
    semaphore *s = Sema_val(vsema);
    int lev=sema_level(s);

    return Val_int(lev);
};

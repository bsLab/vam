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
**    $VERSION:     1.04
**
**    $INFO:
**
**  Amoeba RPC C-wrapper
**
**    $ENDOFINFO
**
*/




/*
** Amoeba RPC stubs for VAM/CaML
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

#include "amoeba_ext.h"
#include "buf.h"

#include <module/ar.h>


/*
** [err_stat,rep_size,hdr_rep] = trans 
**                          (hdr_req,buf_req,req_size,
**                                   buf_rep,rep_size)
*/

CAMLexport value ext_rpc_trans (value args)
{
    CAMLparam0();
    CAMLlocal3(ret_val,stat_val,hdr_rep);

    value hdr_req= Field(args,0);
    value buf_req= Field(args,1);
    value buf_req_size = Field(args,2);
    value buf_rep = Field(args,3);
    value buf_rep_size = Field(args,4); 

    struct caml_buf *vbuf1 = Buf_val(buf_req);
    struct caml_buf *vbuf2 = Buf_val(buf_rep);
    
    int err_status;
    header hdr1,hdr2;    
    
    long    res,trans_rep_size,trans_stat;
    
    bufptr  buf1,buf2;
    f_size_t    buf1_len=Int_val(buf_req_size);
    f_size_t    buf2_len=Int_val(buf_rep_size);

    int obj_val;
    
    value hdr1_port = Field(hdr_req,0);
    value hdr1_priv_obj = Field(Field(hdr_req,1),0);
    value hdr1_priv_rig = Field(Field(hdr_req,1),1);
    value hdr1_priv_ran = Field(Field(hdr_req,1),2);
    value hdr1_com  = Field(hdr_req,2);
    value hdr1_off  = Field(hdr_req,4);
    value hdr1_size = Field(hdr_req,5);
    value hdr1_ext  = Field(hdr_req,6);

    value hdr2_port;
    value hdr2_priv_obj;
    value hdr2_priv_rig;
    value hdr2_priv_ran;
    value hdr2_stat;

    
    if (buf1_len > vbuf1->size)
        failwith ("rpc_trans_ml: request buffer size mismatch");

    if (buf2_len > vbuf2->size)
        failwith ("rpc_trans_ml: reply buffer size mismatch");
    
    Begin_roots3(buf_req,buf_rep,hdr_req);
    
        memcpy(&hdr1.h_port,String_val(Field(hdr1_port,0)),PORTSIZE);     
        
        if (buf1_len  > 0)
            buf1 = vbuf1->data;
        else
            buf1 = NULL;

        if (buf2_len > 0)                
            buf2 = vbuf2->data;
        else
            buf2 = NULL;
    
        obj_val = Int_val(Field(hdr1_priv_obj,0));        
        hdr1.h_priv.prv_object[0]=obj_val &0xff;
        hdr1.h_priv.prv_object[1]=(obj_val >> 8)&0xff;
        hdr1.h_priv.prv_object[2]=(obj_val >> 16)&0xff;
        hdr1.h_priv.prv_rights = Int_val(Field(hdr1_priv_rig,0));
        
        memcpy(&hdr1.h_priv.prv_random,
               String_val(Field(hdr1_priv_ran,0)),PORTSIZE);
         
        hdr1.h_command = (command)(Int_val(Field(hdr1_com,0)));
        hdr1.h_offset  = (int32)(Int_val(hdr1_off));
        hdr1.h_size    = (uint16)(Int_val(hdr1_size));
        hdr1.h_extra   = (uint16)(Int_val(hdr1_ext));

        enter_blocking_section();

        res = rpc_trans (&hdr1,buf1,buf1_len,
                         &hdr2,buf2,buf2_len);

        leave_blocking_section();

        /*
        ** Allocate reply header
        */

        hdr_rep = ext_hdr_new (Val_unit);

        hdr2_port = Field(hdr_rep,0);
        hdr2_priv_obj = Field(Field(hdr_rep,1),0);
        hdr2_priv_rig = Field(Field(hdr_rep,1),1);
        hdr2_priv_ran = Field(Field(hdr_rep,1),2);
        hdr2_stat = Field(hdr_rep,3);



        Store_field(hdr2_stat,0,Val_int(ERR_CONVERT(hdr2.h_status)));
        Store_field(hdr_rep,4,Val_int((int)hdr2.h_offset));
        Store_field(hdr_rep,5,Val_int((int)hdr2.h_size));
        Store_field(hdr_rep,6,Val_int((int)hdr2.h_extra));
        

        obj_val = ((unsigned char)hdr2.h_priv.prv_object[0])      |
                  ((unsigned char)hdr2.h_priv.prv_object[1] << 8) |
                  ((unsigned char)hdr2.h_priv.prv_object[2] << 16) ;

        Store_field(hdr2_priv_obj,0,Val_int(obj_val));
        Store_field(hdr2_priv_rig,0,Val_int(hdr2.h_priv.prv_rights));
        
        memcpy(String_val(Field(hdr2_priv_ran,0)),
               &hdr2.h_priv.prv_random,PORTSIZE);

        memcpy(String_val(Field(hdr2_port,0)),
                &hdr2.h_port,PORTSIZE);     


    End_roots();
    
    ret_val = alloc_tuple(3);

        
    if (res<0)
    {
        trans_rep_size=0;
        trans_stat = ERR_CONVERT(res);
    }
    else
    {
        trans_rep_size = (int)res;
        trans_stat = STD_OK;
    }    

    stat_val = alloc_tuple(1);
    Store_field(stat_val,0,Val_int(trans_stat));

    Store_field(ret_val,0,stat_val);
    Store_field(ret_val,1,Val_int(trans_rep_size));
    Store_field(ret_val,2,hdr_rep);
    
    CAMLreturn(ret_val);    
}

/*
** [err_stat,req_size,hdr_rep] = getreq (port_req,buf_req,req_size);
*/

CAMLexport value ext_rpc_getreq (value args)
{
    CAMLparam0();
    CAMLlocal3(ret_val,stat_val,hdr_req);

    value port_req=Field(args,0); 
    value buf_req=Field(args,1);
    value buf_req_size = Field(args,2);

    struct caml_buf *vbuf1 = Buf_val(buf_req);
    
    int err_status;
    header hdr1;    
    
    long    res,rep_size,stat;
    
    bufptr  buf1;
    f_size_t    buf1_len=Int_val(buf_req_size);

    int obj_val;
    
    value hdr1_port;
    value hdr1_priv_obj;
    value hdr1_priv_rig;
    value hdr1_priv_ran;
    value hdr1_com;


    
    if (buf1_len > vbuf1->size)
        failwith ("rpc_getreq_ml: request buffer size mismatch");


    Begin_roots1(buf_req);

        memcpy(&hdr1.h_port,String_val(Field(port_req,0)),PORTSIZE);     
        
        if (buf1_len  > 0)
            buf1 = vbuf1->data;
        else
            buf1 = NULL;

        enter_blocking_section();

        res = rpc_getreq (&hdr1,buf1,buf1_len);

        leave_blocking_section();

        /*
        ** Allocate request header
        */
    
        hdr_req=ext_hdr_new(Val_unit);

        hdr1_port = Field(hdr_req,0);
        memcpy(String_val(Field(hdr1_port,0)),&hdr1.h_port,PORTSIZE);     
	
        hdr1_priv_obj = Field(Field(hdr_req,1),0);
        hdr1_priv_rig = Field(Field(hdr_req,1),1);
        hdr1_priv_ran = Field(Field(hdr_req,1),2);
        hdr1_com  = Field(hdr_req,2);
    
        obj_val = ((unsigned char)hdr1.h_priv.prv_object[0])      |
                  ((unsigned char)hdr1.h_priv.prv_object[1] << 8) |
                  ((unsigned char)hdr1.h_priv.prv_object[2] << 16) ;

        Store_field(hdr1_priv_obj,0,Val_int(obj_val));
        Store_field(hdr1_priv_rig,0,Val_int(hdr1.h_priv.prv_rights));
        
        memcpy(String_val(Field(hdr1_priv_ran,0)),
               &hdr1.h_priv.prv_random,
               PORTSIZE);

               
        Store_field(hdr1_com,0,Val_int(hdr1.h_command));
        Store_field(hdr_req,4,Val_int((int)hdr1.h_offset));
        Store_field(hdr_req,5,Val_int((int)hdr1.h_size));
        Store_field(hdr_req,6,Val_int((int)hdr1.h_extra));
        
    End_roots();


    ret_val = alloc_tuple(3);

        
    if (res<0)
    {
        rep_size=0;
        stat = ERR_CONVERT(res);
    }
    else
    {
        rep_size = (int)res;
        stat = STD_OK;
    }    

    stat_val = alloc_tuple(1);
    Store_field(stat_val,0,Val_int(stat));

    Store_field(ret_val,0,stat_val);
    Store_field(ret_val,1,Val_int(rep_size));
    Store_field(ret_val,2,hdr_req);
    
    CAMLreturn(ret_val);    
}

/*
** [err_stat ] = putrep (hdr_rep,buf_rep,rep_size);
*/

CAMLexport value ext_rpc_putrep (value args)
{
    CAMLparam0();
    CAMLlocal1(stat_val);

    value hdr_rep=Field(args,0);
    value buf_rep=Field(args,1);
    value buf_rep_size = Field(args,2);

    struct caml_buf *vbuf1 = Buf_val(buf_rep);
    
    int err_status;
    header hdr1;    
    
    long    res,rep_size,stat;
    
    bufptr  buf1;
    f_size_t    buf1_len=Int_val(buf_rep_size);

    int obj_val;
    
    value hdr1_port = Field(hdr_rep,0);
    value hdr1_priv_obj = Field(Field(hdr_rep,1),0);
    value hdr1_priv_rig = Field(Field(hdr_rep,1),1);
    value hdr1_priv_ran = Field(Field(hdr_rep,1),2);
    value hdr1_stat  = Field(hdr_rep,3);
    value hdr1_off  = Field(hdr_rep,4);
    value hdr1_size = Field(hdr_rep,5);
    value hdr1_ext  = Field(hdr_rep,6);

    
    if (buf1_len > vbuf1->size)
        failwith ("rpc_getrep_ml: reply buffer size mismatch");

    Begin_roots1(buf_rep);

        memcpy(&hdr1.h_port,String_val(Field(hdr1_port,0)),PORTSIZE);     
        
        if (buf1_len  > 0)
            buf1 = vbuf1->data;
        else
            buf1 = NULL;

        obj_val = Int_val(Field(hdr1_priv_obj,0));        
        hdr1.h_priv.prv_object[0]=obj_val &0xff;
        hdr1.h_priv.prv_object[1]=(obj_val >> 8)&0xff;
        hdr1.h_priv.prv_object[2]=(obj_val >> 16)&0xff;
        hdr1.h_priv.prv_rights = Int_val(Field(hdr1_priv_rig,0));
        memcpy(&hdr1.h_priv.prv_random,
               String_val(Field(hdr1_priv_ran,0)),PORTSIZE);
         
        hdr1.h_status = (Int_val(Field(hdr1_stat,0)));
        hdr1.h_offset  = (int32)(Int_val(hdr1_off));
        hdr1.h_size    = (uint16)(Int_val(hdr1_size));
        hdr1.h_extra   = (uint16)(Int_val(hdr1_ext));

        enter_blocking_section();

        res = rpc_putrep (&hdr1,buf1,buf1_len);
        
        leave_blocking_section();

    End_roots();
        
    if (res<0)
    {
        stat = ERR_CONVERT(res);
    }
    else
    {
        stat = STD_OK;
    }    

    stat_val = alloc_tuple(1);
    Store_field(stat_val,0,Val_int(stat));

    CAMLreturn(stat_val);    
}



/*
** [err_stat,rep_size,hdr_rep] = transo
**                          (hdr_req,buf_req,req_off,req_size,
**                                   buf_rep,rep_off,rep_size)
*/

CAMLexport value ext_rpc_transo (value args)
{
    CAMLparam0();
    CAMLlocal3(ret_val,stat_val,hdr_rep);

    value hdr_req= Field(args,0);
    value buf_req= Field(args,1);
    value buf_req_off  = Field(args,2);
    value buf_req_size = Field(args,3);
    value buf_rep = Field(args,4);
    value buf_rep_off  = Field(args,5);
    value buf_rep_size = Field(args,6); 
    
    struct caml_buf *vbuf1 = Buf_val(buf_req);
    struct caml_buf *vbuf2 = Buf_val(buf_rep);
            
    int err_status;
    header hdr1,hdr2;    
    
    long    res,trans_rep_size,trans_stat;
    
    bufptr  buf1,buf2;
    f_size_t    buf1_len=Int_val(buf_req_size);
    f_size_t    buf2_len=Int_val(buf_rep_size);
    int         buf1_off=Int_val(buf_req_off);
    int         buf2_off=Int_val(buf_rep_off);
    
    
    int obj_val;


        
    value hdr1_port = Field(hdr_req,0);
    value hdr1_priv_obj = Field(Field(hdr_req,1),0);
    value hdr1_priv_rig = Field(Field(hdr_req,1),1);
    value hdr1_priv_ran = Field(Field(hdr_req,1),2);
    value hdr1_com  = Field(hdr_req,2);
    value hdr1_off  = Field(hdr_req,4);
    value hdr1_size = Field(hdr_req,5);
    value hdr1_ext  = Field(hdr_req,6);

    value hdr2_port;
    value hdr2_priv_obj;
    value hdr2_priv_rig;
    value hdr2_priv_ran;
    value hdr2_stat;

    
    
    if ((buf1_len + buf1_off) > vbuf1->size)
        failwith ("rpc_transo_ml: request buffer size mismatch");

    if ((buf2_len + buf2_off) > vbuf2->size)
        failwith ("rpc_transo_ml: reply buffer size mismatch");
    
    Begin_roots3(buf_req,buf_rep,hdr_req);
    
        memcpy(&hdr1.h_port,String_val(Field(hdr1_port,0)),PORTSIZE);     
        
        if (buf1_len  > 0)
            buf1 = &((vbuf1->data)[buf1_off]);
        else
            buf1 = NULL;

        if (buf2_len > 0)                
            buf2 = &((vbuf2->data)[buf2_off]);
        else
            buf2 = NULL;
    
        obj_val = Int_val(Field(hdr1_priv_obj,0));        
        hdr1.h_priv.prv_object[0]=obj_val &0xff;
        hdr1.h_priv.prv_object[1]=(obj_val >> 8)&0xff;
        hdr1.h_priv.prv_object[2]=(obj_val >> 16)&0xff;
        hdr1.h_priv.prv_rights = Int_val(Field(hdr1_priv_rig,0));
        memcpy(&hdr1.h_priv.prv_random,
               String_val(Field(hdr1_priv_ran,0)),PORTSIZE);
         
        hdr1.h_command = (command)(Int_val(Field(hdr1_com,0)));
        hdr1.h_offset  = (int32)(Int_val(hdr1_off));
        hdr1.h_size    = (uint16)(Int_val(hdr1_size));
        hdr1.h_extra   = (uint16)(Int_val(hdr1_ext));

        enter_blocking_section();

        res = rpc_trans (&hdr1,buf1,buf1_len,
                         &hdr2,buf2,buf2_len);

        leave_blocking_section();

        /*
        ** Allocate reply header
        */

        hdr_rep = ext_hdr_new (Val_unit);

        hdr2_port = Field(hdr_rep,0);
        hdr2_priv_obj = Field(Field(hdr_rep,1),0);
        hdr2_priv_rig = Field(Field(hdr_rep,1),1);
        hdr2_priv_ran = Field(Field(hdr_rep,1),2);
        hdr2_stat = Field(hdr_rep,3);


        Store_field(hdr2_stat,0,Val_int(ERR_CONVERT(hdr2.h_status)));
        Store_field(hdr_rep,4,Val_int((int)hdr2.h_offset));
        Store_field(hdr_rep,5,Val_int((int)hdr2.h_size));
        Store_field(hdr_rep,6,Val_int((int)hdr2.h_extra));


        obj_val = ((unsigned char)hdr2.h_priv.prv_object[0])      |
                  ((unsigned char)hdr2.h_priv.prv_object[1] << 8) |
                  ((unsigned char)hdr2.h_priv.prv_object[2] << 16) ;

        Store_field(hdr2_priv_obj,0,Val_int(obj_val));
        Store_field(hdr2_priv_rig,0,Val_int(hdr2.h_priv.prv_rights));
        

        memcpy(String_val(Field(hdr2_port,0)),
                &hdr2.h_port,PORTSIZE);     


        memcpy(String_val(Field(hdr2_priv_ran,0)),
               &hdr2.h_priv.prv_random,
               PORTSIZE);


    End_roots();
    
    ret_val = alloc_tuple(3);

        
    if (res<0)
    {
        trans_rep_size=0;
        trans_stat = ERR_CONVERT(res);
    }
    else
    {
        trans_rep_size = (int)res;
        trans_stat = STD_OK;
    }    

    stat_val = alloc_tuple(1);
    Store_field(stat_val,0,Val_int(trans_stat));

    Store_field(ret_val,0,stat_val);
    Store_field(ret_val,1,Val_int(trans_rep_size));
    Store_field(ret_val,2,hdr_rep);
        
    CAMLreturn(ret_val);    
}

/*
** [err_stat,req_size,hdr_req] = getreqo (port_req,buf_req,req_off,req_size);
*/

CAMLexport value ext_rpc_getreqo (value args)
{
    CAMLparam0();
    CAMLlocal3(ret_val,stat_val,hdr_req);

    value port_req=Field(args,0);
    value buf_req=Field(args,1);
    value buf_req_off  = Field(args,2);
    value buf_req_size = Field(args,3);
   
    struct caml_buf *vbuf1 = Buf_val(buf_req);
           
    int err_status;
    header hdr1;    
    
    long    res,rep_size,stat;
    
    bufptr  buf1;
    f_size_t    buf1_len=Int_val(buf_req_size);
    int         buf1_off=Int_val(buf_req_off);
    
    int obj_val;
    
    value hdr1_port;
    value hdr1_priv_obj;
    value hdr1_priv_rig;
    value hdr1_priv_ran;
    value hdr1_com;


    
    if ((buf1_len + buf1_off) > vbuf1->size)
        failwith ("rpc_getreqo_ml: request buffer size mismatch");


    Begin_roots1(buf_req);

        memcpy(&hdr1.h_port,String_val(Field(port_req,0)),PORTSIZE);     
        
        if (buf1_len  > 0)
            buf1 = &((vbuf1->data)[buf1_off]);
        else
            buf1 = NULL;

        enter_blocking_section();

        res = rpc_getreq (&hdr1,buf1,buf1_len);

        leave_blocking_section();

        hdr_req=ext_hdr_new(Val_unit);

        hdr1_port = Field(hdr_req,0);
        memcpy(String_val(Field(hdr1_port,0)),&hdr1.h_port,PORTSIZE);     

        hdr1_priv_obj = Field(Field(hdr_req,1),0);
        hdr1_priv_rig = Field(Field(hdr_req,1),1);
        hdr1_priv_ran = Field(Field(hdr_req,1),2);
        hdr1_com  = Field(hdr_req,2);
    
        obj_val = ((unsigned char)hdr1.h_priv.prv_object[0])      |
                  ((unsigned char)hdr1.h_priv.prv_object[1] << 8) |
                  ((unsigned char)hdr1.h_priv.prv_object[2] << 16) ;

        Store_field(hdr1_priv_obj,0,Val_int(obj_val));
        Store_field(hdr1_priv_rig,0,Val_int(hdr1.h_priv.prv_rights));
        
        memcpy(String_val(Field(hdr1_priv_ran,0)),
               &hdr1.h_priv.prv_random,
               PORTSIZE);

               
        Store_field(hdr1_com,0,Val_int(hdr1.h_command));
        Store_field(hdr_req,4,Val_int((int)hdr1.h_offset));
        Store_field(hdr_req,5,Val_int((int)hdr1.h_size));
        Store_field(hdr_req,6,Val_int((int)hdr1.h_extra));
        
    End_roots();

    ret_val = alloc_tuple(3);
        
    if (res<0)
    {
        rep_size=0;
        stat = ERR_CONVERT(res);
    }
    else
    {
        rep_size = (int)res;
        stat = STD_OK;
    }    

    stat_val = alloc_tuple(1);
    Store_field(stat_val,0,Val_int(stat));

    Store_field(ret_val,0,stat_val);
    Store_field(ret_val,1,Val_int(rep_size));
    Store_field(ret_val,2,hdr_req);
    
    CAMLreturn(ret_val);    
}

/*
** [err_stat ] = putrepo (hdr_rep,buf_rep,rep_off,rep_size);
*/

CAMLexport value ext_rpc_putrepo (value args)
{
    CAMLparam0();
    CAMLlocal1(stat_val);

    value hdr_rep=Field(args,0);
    value buf_rep=Field(args,1);
    value buf_rep_off  = Field(args,2);
    value buf_rep_size = Field(args,3);

    struct caml_buf *vbuf1 = Buf_val(buf_rep);
            
    int err_status;
    header hdr1;    
    
    long    res,rep_size,stat;
    
    bufptr  buf1;
    f_size_t    buf1_len=Int_val(buf_rep_size);
    int         buf1_off=Int_val(buf_rep_off);
    
    int obj_val;
    
    value hdr1_port = Field(hdr_rep,0);
    value hdr1_priv_obj = Field(Field(hdr_rep,1),0);
    value hdr1_priv_rig = Field(Field(hdr_rep,1),1);
    value hdr1_priv_ran = Field(Field(hdr_rep,1),2);
    value hdr1_stat  = Field(hdr_rep,3);
    value hdr1_off  = Field(hdr_rep,4);
    value hdr1_size = Field(hdr_rep,5);
    value hdr1_ext  = Field(hdr_rep,6);

    
    if ((buf1_len+buf1_off) > vbuf1->size)
        failwith ("rpc_getrepo_ml: reply buffer size mismatch");

    Begin_roots1(buf_rep);

        memcpy(&hdr1.h_port,String_val(Field(hdr1_port,0)),PORTSIZE);     
        
        if (buf1_len  > 0)
            buf1 = &((vbuf1->data)[buf1_off]);
        else
            buf1 = NULL;

        obj_val = Int_val(Field(hdr1_priv_obj,0));        
        hdr1.h_priv.prv_object[0]=obj_val &0xff;
        hdr1.h_priv.prv_object[1]=(obj_val >> 8)&0xff;
        hdr1.h_priv.prv_object[2]=(obj_val >> 16)&0xff;
        hdr1.h_priv.prv_rights = Int_val(Field(hdr1_priv_rig,0));
        memcpy(&hdr1.h_priv.prv_random,
               String_val(Field(hdr1_priv_ran,0)),PORTSIZE);
         
        hdr1.h_status = (Int_val(Field(hdr1_stat,0)));
        hdr1.h_offset  = (int32)(Int_val(hdr1_off));
        hdr1.h_size    = (uint16)(Int_val(hdr1_size));
        hdr1.h_extra   = (uint16)(Int_val(hdr1_ext));

        enter_blocking_section();

        res = rpc_putrep (&hdr1,buf1,buf1_len);
        
        leave_blocking_section();

    End_roots();
        
    if (res<0)
    {
        stat = ERR_CONVERT(res);
    }
    else
    {
        stat = STD_OK;
    }    

    stat_val = alloc_tuple(1);
    Store_field(stat_val,0,Val_int(stat));

    CAMLreturn(stat_val);    
}

/*
** timeout <msec>
*/
            
CAMLexport value ext_rpc_timeout (value vtmo)
{
    interval tmo=Int_val(vtmo);
    interval old;
    old=timeout(tmo);
    return Val_int((int)old);
};



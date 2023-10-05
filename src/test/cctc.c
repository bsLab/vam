/*
** Test CC block allocation
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

#include <stdio.h>

value hdr_new (value unit)
{
    CAMLparam0();
    CAMLlocal5(hdr_v,port_v,priv_v,comm_v,stat_v);
    CAMLlocal5(port_s,priv_p_s,priv_p_v,priv_o,priv_r);
    
    hdr_v  = alloc_tuple(7);
    port_v = alloc_tuple(1);
    priv_v = alloc_tuple(3);
    port_s = alloc_string(PORTSIZE);
    priv_p_s = alloc_string(PORTSIZE);
    priv_p_v = alloc_tuple(1);
    priv_o = alloc_tuple(1);
    priv_r = alloc_tuple(1);
    comm_v = alloc_tuple(1);
    stat_v = alloc_tuple(1);
    
    
    /* 
    ** Build the private structure 
    */
    Store_field(priv_r,0,Val_int(0));
    Store_field(priv_o,0,Val_int(0));
    memset(String_val(priv_p_s),0,PORTSIZE);
    Store_field(priv_p_v,0,priv_p_s);
    Store_field(priv_v,0,priv_o);
    Store_field(priv_v,1,priv_r);
    Store_field(priv_v,2,priv_p_v);
    
    /*
    ** The port
    */
    memset(String_val(port_s),0,PORTSIZE);
    Store_field(port_v,0,port_s);

    /*
    ** Command and status field
    */
    
    Store_field(comm_v,0,Val_int(0));
    Store_field(stat_v,0,Val_int(0));
    
    /*
    ** Setup the header structure 
    */
    
    Store_field(hdr_v,0,port_v);
    Store_field(hdr_v,1,priv_v);
    Store_field(hdr_v,2,comm_v);
    Store_field(hdr_v,3,stat_v);
    Store_field(hdr_v,4,Val_int(0));
    Store_field(hdr_v,5,Val_int(0));
    Store_field(hdr_v,6,Val_int(0));

    
    CAMLreturn(hdr_v);
}


value tt_c (value args)
{
    CAMLparam0();
    CAMLlocal3(ret_val,stat_val,hdr2stat_val);
    value hdr_req= Field(args,0);
    value buf_req= Field(Field(args,1),0);
    value buf_req_size = Field(args,2);
    value hdr_rep = Field(args,3);
    value buf_rep = Field(Field(args,4),0);
    value buf_rep_size = Field(args,5); 
    
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
    
    if (buf1_len > string_length(buf_req))
        failwith ("rpc_trans_ml: request buffer size mismatch");

    if (buf2_len > string_length(buf_rep))
        failwith ("rpc_trans_ml: reply buffer size mismatch");
    
    Begin_roots2(buf_rep,hdr_rep);
    
        memcpy(&hdr1.h_port,String_val(Field(hdr1_port,0)),PORTSIZE);     
        
        if (buf1_len  > 0)
            buf1 = String_val(buf_req);
        else
            buf1 = NULL;

        if (buf2_len > 0)                
            buf2 = String_val(buf_rep);
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
        hdr1.h_size    = (bufsize)(Int_val(hdr1_size));
        hdr1.h_extra   = (uint16)(Int_val(hdr1_ext));

        enter_blocking_section();

        res = buf2_len;

        
        hdr2.h_status = 111;
        hdr2.h_offset = 112;
        hdr2.h_size = 113;
        hdr2.h_extra = 114;

        memset((char *)&hdr2.h_priv,0xaa,10);
        
        leave_blocking_section();

        hdr2_port = Field(hdr_rep,0);
        hdr2_priv_obj = Field(Field(hdr_rep,1),0);
        hdr2_priv_rig = Field(Field(hdr_rep,1),1);
        hdr2_priv_ran = Field(Field(hdr_rep,1),2);
#if 0
        hdr2_stat = Field(hdr_rep,3);


        Store_field(hdr2_stat,0,Val_int(ERR_CONVERT(hdr2.h_status)));
#endif
        hdr2stat_val = alloc_tuple(1);
        Store_field(hdr2stat_val,0,Val_int(ERR_CONVERT(hdr2.h_status)));
        Store_field(hdr_rep,3,hdr2stat_val);
        
        Store_field(hdr_rep,4,Val_int((int)hdr2.h_offset));
        Store_field(hdr_rep,5,Val_int((int)hdr2.h_size));
        Store_field(hdr_rep,6,Val_int((int)hdr2.h_extra));
        


        obj_val = (hdr2.h_priv.prv_object[0])      |
                  (hdr2.h_priv.prv_object[1] << 8) |
                  (hdr2.h_priv.prv_object[2] << 16) ;

        Store_field(hdr2_priv_obj,0,Val_int(obj_val));
        Store_field(hdr2_priv_rig,0,Val_int(hdr2.h_priv.prv_rights));
        
        memcpy(String_val(Field(hdr2_priv_ran,0)),
               &hdr2.h_priv.prv_random,
               PORTSIZE);

                                      
    End_roots();
    
    ret_val = alloc_tuple(2);

        
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
    
    CAMLreturn(ret_val);    
}

value extraise (value v1)
{
    failwith ("extraise: test");    
}
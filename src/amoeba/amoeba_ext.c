/* 
** This file is part of the FIREBALL AMOEBA System.
**
** Written by:
**               Stefan Bosse
**               sbosse@physik.uni-bremen.de
**
** Last modified:
**           28/04/02
**
** Changes:
**
**
**
** FIREBALL AMOEBA is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public License as 
** published by the Free Software Foundation; version 2.
**
** The FIREBALL AMOEBA is distributed in the hope that it will be usefull,
** but WITHOUT ANY WARRANTY; without even implied warranty of 
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
** General Public License for more details.
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


/*
** Retrurn a fresh header structure, initially empty.
*/

CAMLexport value ext_hdr_new (value unit)
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

/*
** Retrurn a fresh private structure, initially empty.
*/

CAMLexport value ext_priv_new (value unit)
{
    CAMLparam0();
    CAMLlocal1(priv_v);
    CAMLlocal4(priv_p_s,priv_p_v,priv_o,priv_r);
    
    priv_v = alloc_tuple(3);
    priv_p_s = alloc_string(PORTSIZE);
    priv_p_v = alloc_tuple(1);
    priv_o = alloc_tuple(1);
    priv_r = alloc_tuple(1);
    
    
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
    
    
    CAMLreturn(priv_v);
}

/*
** Retrurn a fresh port structure, initially empty.
*/

CAMLexport value ext_port_new (value unit)
{
    CAMLparam0();
    CAMLlocal1(port_v);
    CAMLlocal1(port_s);
    
    port_v = alloc_tuple(1);
    port_s = alloc_string(PORTSIZE);

    memset(String_val(port_s),0,PORTSIZE);
    Store_field(port_v,0,port_s);
    
    CAMLreturn(port_v);
}

/*
** Retrurn a fresh capability structure, initally empty.
*/

CAMLexport value ext_cap_new (value unit)
{
    CAMLparam0();
    CAMLlocal3(cap_v,port_v,priv_v);
    CAMLlocal5(port_s,priv_p_s,priv_p_v,priv_o,priv_r);
    
    cap_v = alloc_tuple(2);
    priv_v = alloc_tuple(3);
    port_v = alloc_tuple(1);
    port_s = alloc_string (PORTSIZE);
    priv_p_s = alloc_string(PORTSIZE);
    priv_p_v = alloc_tuple(1);
    priv_o = alloc_tuple(1);
    priv_r = alloc_tuple(1);
    
    
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

    /* and the port */
    
    memset(String_val(port_s),0,PORTSIZE);
    Store_field(port_v,0,port_s);
    
    Store_field(cap_v,0,port_v);
    Store_field(cap_v,1,priv_v);
    
    CAMLreturn(cap_v);
}

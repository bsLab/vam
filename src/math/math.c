/*
** Standard arithmetic operations supporting various
** data types.
*/

#include <stddef.h>
#include <stdarg.h>
#include <string.h>

#include "mlvalues.h"
#include "alloc.h"
#include "memory.h"
#include "fail.h"
#include "custom.h"

#include "math.h"

#define IS_UNKNOWN  0
#define IS_INT      1       /* caml int */
#define IS_FLOAT    2       /* caml double precision float */
#define IS_COMPLEX  3       /* (float*float) tuples */

/*
** 'a -> 'a -> 'a
*/

/*
** Add two values
*/

value uni_add (value v1,value v2)
{
    double  val1_f,val2_f,res_f;
    long    val1_i,val2_i,res_i;
    int     type1=IS_UNKNOWN;
    int     type2=IS_UNKNOWN;    

    /*
    ** Try to recognize argument types
    */

    
    if(Is_block(v1))
    {

        if(Tag_val(v1)==Double_tag)
            type1=IS_FLOAT;
        else if(Tag_val(v1)==0 && Wosize_val(v1)==2)
                type1=IS_COMPLEX;
        else
                type1=IS_UNKNOWN;
    }
    else
        type1=IS_INT;
        
    if(Is_block(v2))
    {
        if(Tag_val(v2)==Double_tag)
            type2=IS_FLOAT;
        else if (Tag_val(v2)==0 && Wosize_val(v2)==2)
            type2=IS_COMPLEX;
        else
            type2=IS_UNKNOWN;
    }
    else
        type2=IS_INT;
    
    if(type1!=type2)
    {
        failwith("Internal error: got different argument types");
    }    
    
    
    switch(type1)
    {
        
        case IS_INT:
            res_i=Int_val(v1)+Int_val(v2);
            return Val_int(res_i);
        break;
        
        case IS_FLOAT:
            res_f=Double_val(v1)+Double_val(v2);
            return copy_double(res_f);
        break;
        
        case IS_COMPLEX:
        {
            CAMLparam0();
            CAMLlocal1(ret);
            double re1,re2,im1,im2;
            
            re1 = Double_val(Field(v1,0));
            im1 = Double_val(Field(v1,1));
            re2 = Double_val(Field(v2,0));
            im2 = Double_val(Field(v2,1));
                                
            ret = alloc_tuple(2);
            Store_field(ret,0,
                copy_double(re1+re2));
            Store_field(ret,1,
                copy_double(im1+im2));
                
            CAMLreturn(ret);
            break;        
        }
        
        case IS_UNKNOWN:    
        default:
            invalid_argument("uni_add: arguments of unsupported type");
            break;
    }    
             
    failwith("Programming error");
    
    /*
    ** Never reached ! 
    */
    
    return 0;
    
}


/*
** Subtract two values
*/

value uni_sub (value v1,value v2)
{
    double  val1_f,val2_f,res_f;
    long    val1_i,val2_i,res_i;
    int     type1=IS_UNKNOWN;
    int     type2=IS_UNKNOWN;    

    /*
    ** Try to recognize argument types
    */

    
    if(Is_block(v1))
    {

        if(Tag_val(v1)==Double_tag)
            type1=IS_FLOAT;
        else if(Tag_val(v1)==0 && Wosize_val(v1)==2)
                type1=IS_COMPLEX;
        else
                type1=IS_UNKNOWN;
    }
    else
        type1=IS_INT;
        
    if(Is_block(v2))
    {
        if(Tag_val(v2)==Double_tag)
            type2=IS_FLOAT;
        else if (Tag_val(v2)==0 && Wosize_val(v2)==2)
            type2=IS_COMPLEX;
        else
            type2=IS_UNKNOWN;
    }
    else
        type2=IS_INT;
    
    if(type1!=type2)
    {
        failwith("Internal error: got different argument types");
    }    
    
    
    switch(type1)
    {
        
        case IS_INT:
            res_i=Int_val(v1)-Int_val(v2);
            return Val_int(res_i);
        break;
        
        case IS_FLOAT:
            res_f=Double_val(v1)-Double_val(v2);
            return copy_double(res_f);
        break;
        
        case IS_COMPLEX:
        {
            CAMLparam0();
            CAMLlocal1(ret);
            double re1,re2,im1,im2;
            
            re1 = Double_val(Field(v1,0));
            im1 = Double_val(Field(v1,1));
            re2 = Double_val(Field(v2,0));
            im2 = Double_val(Field(v2,1));
                                
            ret = alloc_tuple(2);
            Store_field(ret,0,
                copy_double(re1-re2));
            Store_field(ret,1,
                copy_double(im1-im2));
                
            CAMLreturn(ret);
            break;        
        }
        
        case IS_UNKNOWN:    
        default:
            invalid_argument("uni_sub: arguments of unsupported type");
            break;
    }    
             
    failwith("Programming error");
    
    /*
    ** Never reached ! 
    */
    
    return 0;
    
}








/*
** Multiply two values
*/

value uni_mul (value v1,value v2)
{
    double  val1_f,val2_f,res_f;
    long    val1_i,val2_i,res_i;
    int     type1=IS_UNKNOWN;
    int     type2=IS_UNKNOWN;    

    /*
    ** Try to recognize argument types
    */

    
    if(Is_block(v1))
    {

        if(Tag_val(v1)==Double_tag)
            type1=IS_FLOAT;
        else if(Tag_val(v1)==0 && Wosize_val(v1)==2)
                type1=IS_COMPLEX;
        else
                type1=IS_UNKNOWN;
    }
    else
        type1=IS_INT;
        
    if(Is_block(v2))
    {
        if(Tag_val(v2)==Double_tag)
            type2=IS_FLOAT;
        else if (Tag_val(v2)==0 && Wosize_val(v2)==2)
            type2=IS_COMPLEX;
        else
            type2=IS_UNKNOWN;
    }
    else
        type2=IS_INT;
    
    if(type1!=type2)
    {
        failwith("Internal error: got different argument types");
    }    
    
    
    switch(type1)
    {
        
        case IS_INT:
            res_i=Int_val(v1)*Int_val(v2);
            return Val_int(res_i);
        break;
        
        case IS_FLOAT:
            res_f=Double_val(v1)*Double_val(v2);
            return copy_double(res_f);
        break;
        
        case IS_COMPLEX:
        {
            CAMLparam0();
            CAMLlocal1(ret);
            double re1,re2,im1,im2;
            
            re1 = Double_val(Field(v1,0));
            im1 = Double_val(Field(v1,1));
            re2 = Double_val(Field(v2,0));
            im2 = Double_val(Field(v2,1));
                                
            ret = alloc_tuple(2);

            Store_field(ret,0,
                copy_double(re1*re2-im1*im2));
            Store_field(ret,1,
                copy_double(re1*im2+re2*im1));
                
            CAMLreturn(ret);
            break;        
        }
        
        case IS_UNKNOWN:    
        default:
            invalid_argument("uni_mul: arguments of unsupported type");
            break;
    }    
             
    failwith("Programming error");
    
    /*
    ** Never reached ! 
    */
    
    return 0;
    
}


/*
** Divide two values
*/

value uni_div (value v1,value v2)
{
    double  val1_f,val2_f,res_f;
    long    val1_i,val2_i,res_i;
    int     type1=IS_UNKNOWN;
    int     type2=IS_UNKNOWN;    

    /*
    ** Try to recognize argument types
    */

    
    if(Is_block(v1))
    {

        if(Tag_val(v1)==Double_tag)
            type1=IS_FLOAT;
        else if(Tag_val(v1)==0 && Wosize_val(v1)==2)
                type1=IS_COMPLEX;
        else
                type1=IS_UNKNOWN;
    }
    else
        type1=IS_INT;
        
    if(Is_block(v2))
    {
        if(Tag_val(v2)==Double_tag)
            type2=IS_FLOAT;
        else if (Tag_val(v2)==0 && Wosize_val(v2)==2)
            type2=IS_COMPLEX;
        else
            type2=IS_UNKNOWN;
    }
    else
        type2=IS_INT;
    
    if(type1!=type2)
    {
        failwith("Internal error: got different argument types");
    }    
    
    
    switch(type1)
    {
        
        case IS_INT:
            res_i=Int_val(v1)/Int_val(v2);
            return Val_int(res_i);
        break;
        
        case IS_FLOAT:
            res_f=Double_val(v1)/Double_val(v2);
            return copy_double(res_f);
        break;
        
        case IS_COMPLEX:
        {
            CAMLparam0();
            CAMLlocal1(ret);
            double re1,re2,im1,im2,denum;
            
            re1 = Double_val(Field(v1,0));
            im1 = Double_val(Field(v1,1));
            re2 = Double_val(Field(v2,0));
            im2 = Double_val(Field(v2,1));
                                
            ret = alloc_tuple(2);
            
            denum=re2*re2+im2*im2;
            Store_field(ret,0,
                copy_double((re1*re2+im1*im2)/denum));
            Store_field(ret,1,
                copy_double((re2*im1-re1*im2)/denum));
                
            CAMLreturn(ret);
            break;        
        }
        
        case IS_UNKNOWN:    
        default:
            invalid_argument("uni_div: arguments of unsupported type");
            break;
    }    
             
    failwith("Programming error");
    
    /*
    ** Never reached ! 
    */
    
    return 0;
    
}


/*
** Absolute value. Only int and float types are supported. For the
** complex data type use cabs.
*/

value uni_abs (value v)
{
    double  val1_f,res_f;
    long    val1_i,res_i;
    int     type=IS_UNKNOWN;

    /*
    ** Try to recognize argument types
    */

    
    if(Is_block(v))
    {

        if(Tag_val(v)==Double_tag)
            type=IS_FLOAT;
        else
            type=IS_UNKNOWN;
    }
    else
        type=IS_INT;
        
    
    switch(type)
    {
        
        case IS_INT:
            res_i=abs(Int_val(v));
            return Val_int(res_i);
        break;
        
        case IS_FLOAT:
            res_f=fabs(Double_val(v));
            return copy_double(res_f);
        break;
        
        case IS_UNKNOWN:    
        default:
            invalid_argument("uni_abs: arguments of unsupported type");
            break;
    }    
             
    failwith("Programming error");
    
    /*
    ** Never reached ! 
    */
    
    return 0;
    
}


/*
** Negate a value
*/

value uni_neg (value v)
{
    double  val_f,res_f;
    long    val_i,res_i;
    int     type=IS_UNKNOWN;

    /*
    ** Try to recognize argument types
    */

    
    if(Is_block(v))
    {

        if(Tag_val(v)==Double_tag)
            type=IS_FLOAT;
        else if(Tag_val(v)==0 && Wosize_val(v)==2)
                type=IS_COMPLEX;
        else
                type=IS_UNKNOWN;
    }
    else
        type=IS_INT;
        
    
    switch(type)
    {
        
        case IS_INT:
            res_i=-Int_val(v);
            return Val_int(res_i);
        break;
        
        case IS_FLOAT:
            res_f=-Double_val(v);
            return copy_double(res_f);
        break;
        
        case IS_COMPLEX:
        {
            CAMLparam0();
            CAMLlocal1(ret);
            double re,im;
            
            re = Double_val(Field(v,0));
            im = Double_val(Field(v,1));
                                
            ret = alloc_tuple(2);
            Store_field(ret,0,
                copy_double(-re));
            Store_field(ret,1,
                copy_double(-im));
                
            CAMLreturn(ret);
            break;        
        }
        
        case IS_UNKNOWN:    
        default:
            invalid_argument("uni_neg: arguments of unsupported type");
            break;
    }    
             
    failwith("Programming error");
    
    /*
    ** Never reached ! 
    */
    
    return 0;
    
}

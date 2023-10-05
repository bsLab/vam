/*
** Module [Complex]
**
** Type complex is (float*float) tuple.
*/

#include <stddef.h>
#include <stdarg.h>
#include <string.h>

#include <mlvalues.h>
#include <alloc.h>
#include <callback.h>
#include <memory.h>
#include <signals.h>
#include <fail.h>

#include "_math.h"

/*
** Add two complex numbers 
*/

CAMLexport value ext_add_complex (value c1,value c2)
{
    CAMLparam0();
    double re1,re2,im1,im2,rer,imr;
    CAMLlocal1(ret);
    
    /*
    ** Extract argument size and tags
    */
    
    if(Wosize_val(c1)!=2 || Tag_val(c1)!=0)
    {
        /*
        ** Invalid argument 1        
        */
        invalid_argument("argument 1 must be of type complex");
        
    };

    if(Wosize_val(c1)!=2 || Tag_val(c1)!=0)
    {
        /*
        ** Invalid argument 2
        */
        invalid_argument("argument 2 must be of type complex");
    };

    re1 = Double_val(Field(c1,0));
    im1 = Double_val(Field(c1,1));
    re2 = Double_val(Field(c2,0));
    im2 = Double_val(Field(c2,1));
    
    rer = re1 + re2;
    imr = im1 + im2;
            
    ret = alloc_tuple(2);
    
    Store_field(ret,0,copy_double(rer));
    Store_field(ret,1,copy_double(imr));
            
    CAMLreturn(ret);
}

/*
** Subtract two complex numbers 
*/

CAMLexport value ext_sub_complex (value c1,value c2)
{
    CAMLparam0();
    double re1,re2,im1,im2,rer,imr;
    CAMLlocal1(ret);
    
    /*
    ** Extract argument size and tags
    */
    
    if(Wosize_val(c1)!=2 || Tag_val(c1)!=0)
    {
        /*
        ** Invalid argument 1        
        */
        invalid_argument("argument 1 must be of type complex");
        
    };

    if(Wosize_val(c1)!=2 || Tag_val(c1)!=0)
    {
        /*
        ** Invalid argument 2
        */
        invalid_argument("argument 2 must be of type complex");
    };

    re1 = Double_val(Field(c1,0));
    im1 = Double_val(Field(c1,1));
    re2 = Double_val(Field(c2,0));
    im2 = Double_val(Field(c2,1));
    
    rer = re1 - re2;
    imr = im1 - im2;
            
    ret = alloc_tuple(2);
    
    Store_field(ret,0,copy_double(rer));
    Store_field(ret,1,copy_double(imr));
            
    CAMLreturn(ret);
}

/*
** Multiply two complex numbers 
*/

CAMLexport value ext_mul_complex (value c1,value c2)
{
    CAMLparam0();
    double re1,re2,im1,im2,rer,imr;
    CAMLlocal1(ret);
    
    /*
    ** Extract argument size and tags
    */
    
    if(Wosize_val(c1)!=2 || Tag_val(c1)!=0)
    {
        /*
        ** Invalid argument 1        
        */
        invalid_argument("argument 1 must be of type complex");
        
    };

    if(Wosize_val(c1)!=2 || Tag_val(c1)!=0)
    {
        /*
        ** Invalid argument 2
        */
        invalid_argument("argument 2 must be of type complex");
    };

    re1 = Double_val(Field(c1,0));
    im1 = Double_val(Field(c1,1));
    re2 = Double_val(Field(c2,0));
    im2 = Double_val(Field(c2,1));
    
    rer = (re1*re2 - im1*im2);
    imr = (re1*im2 + re2*im1);
            
    ret = alloc_tuple(2);
    
    Store_field(ret,0,copy_double(rer));
    Store_field(ret,1,copy_double(imr));
            
    CAMLreturn(ret);
}

/*
** Divide two complex numbers 
*/

CAMLexport value ext_div_complex (value c1,value c2)
{
    CAMLparam0();
    double re1,re2,im1,im2,rer,imr,denum;
    CAMLlocal1(ret);
    
    /*
    ** Extract argument size and tags
    */
    
    if(Wosize_val(c1)!=2 || Tag_val(c1)!=0)
    {
        /*
        ** Invalid argument 1        
        */
        invalid_argument("argument 1 must be of type complex");
        
    };

    if(Wosize_val(c1)!=2 || Tag_val(c1)!=0)
    {
        /*
        ** Invalid argument 2
        */
        invalid_argument("argument 2 must be of type complex");
    };

    re1 = Double_val(Field(c1,0));
    im1 = Double_val(Field(c1,1));
    re2 = Double_val(Field(c2,0));
    im2 = Double_val(Field(c2,1));
    
    
    denum = (re2*re2) + (im2*im2);
    
    rer = (re1*re2 + im1*im2)/denum;
    imr = (re2*im1 - re1*im2)/denum;
            
    ret = alloc_tuple(2);
    
    Store_field(ret,0,copy_double(rer));
    Store_field(ret,1,copy_double(imr));
            
    CAMLreturn(ret);
}

/*
** some fucntions from the amos library
*/

extern int azlog_(
    doublereal *ar,
    doublereal *ai, 
    doublereal *br,
    doublereal *bi,
    integer *ierr);
extern int azexp_(
    doublereal *ar,
    doublereal *ai, 
    doublereal *br,
    doublereal *bi);
extern int azsqrt_(
    doublereal *ar,
    doublereal *ai, 
    doublereal *br,
    doublereal *bi);

CAMLexport value ext_func_zexp (value c1)
{
    CAMLparam0();
    doublereal re1,im1,rer,imr;
    
    CAMLlocal1(ret);
    

    re1 = Double_val(Field(c1,0));
    im1 = Double_val(Field(c1,1));
    
    azexp_(&re1,&im1,&rer,&imr);
    
    ret = alloc_tuple(2);
    
    Store_field(ret,0,copy_double(rer));
    Store_field(ret,1,copy_double(imr));
            
    CAMLreturn(ret);

}

CAMLexport value ext_func_zlog (value c1)
{
    CAMLparam0();
    doublereal re1,im1,rer,imr;
    integer ierr;
        
    CAMLlocal1(ret);
    

    re1 = Double_val(Field(c1,0));
    im1 = Double_val(Field(c1,1));
    
    azlog_(&re1,&im1,&rer,&imr,&ierr);
    
    if(ierr!=0)
        failwith ("func_zlog: computation error");
        
    ret = alloc_tuple(2);
    
    Store_field(ret,0,copy_double(rer));
    Store_field(ret,1,copy_double(imr));
            
    CAMLreturn(ret);

}

CAMLexport value ext_func_zsqrt (value c1)
{
    CAMLparam0();
    doublereal re1,im1,rer,imr;
    
    CAMLlocal1(ret);
    

    re1 = Double_val(Field(c1,0));
    im1 = Double_val(Field(c1,1));
    
    azsqrt_(&re1,&im1,&rer,&imr);
    
    ret = alloc_tuple(2);
    
    Store_field(ret,0,copy_double(rer));
    Store_field(ret,1,copy_double(imr));
            
    CAMLreturn(ret);


}


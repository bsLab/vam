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
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2005-2006 BSSLAB
**    $CREATED:     17.7.2005
**    $VERSION:     1.02
**
**    $INFO:
**
**  VAM native matrix support (C-Layout).
**
**    $ENDOFINFO
**
*/

/* #define DEBUG_LEVEL 1 */
#include "sys/debug.h"


#include <mlvalues.h>
#include <alloc.h>
#include <custom.h>
#include <callback.h>
#include <memory.h>
#include <signals.h>
#include <fail.h>
#include <stdio.h>

#include "matrix.h"

static  void    matrix_finalize(value v);
static  int     matrix_compare(value v1, value v2);
static  long    matrix_hash(value v);
static  void    matrix_serialize(value, unsigned long *, unsigned long *);
static  unsigned long matrix_deserialize(void * dst);

static struct custom_operations matrix_ops = 
{
    "_matrix",
    matrix_finalize,
    matrix_compare,
    matrix_hash,
    matrix_serialize,
    matrix_deserialize
};

static char tmpbuf[100];

/*
** Return data size in bytes. Keep consistent with matrix_type.
*/
int matrix_data_size(int kind)
{
    switch (kind) {
        case M_FLOAT:
            return (sizeof(double));
            break;
        case M_FLOAT32:
            return 4;
            break;
        case M_FLOAT64:
            return 8;
            break;
        case M_INT:
            return (sizeof(int));
            break;
        case M_BYTE:
        case M_INT8:
            return 1;
            break;
        case M_INT16:
            return 2;
            break;
        case M_INT32:
            return 4;
            break;
        case M_INT64:
            return 8;
            break;
        default: printf("programming error in matrix_data_size %d\n",kind);
                 abort();
    };
};

char* matrix_data_type(int kind)
{
    switch (kind) {
        case M_FLOAT:
            return "float";
            break;
        case M_FLOAT32:
            return "float32";
            break;
        case M_FLOAT64:
            return "float64";
            break;
        case M_INT:
            return "int";
            break;
        case M_BYTE:
            return "byte";
            break;
        case M_INT8:
            return "int8";
            break;
        case M_INT16:
            return "int16";
            break;
        case M_INT32:
            return "int32";
            break;
        case M_INT64:
            return "int64";
            break;
        default: printf("programming error in matrix_data_type %d\n",kind);
                 abort();
    };
};

CAMLexport value
matrix_create (value vdim, value vkind, value vinit)
{
    CAMLparam1(vinit);
    CAMLlocal1(res);

    caml_matrix_p mat;
    int ndim;
    int size=1;
    int i,max,used;


    if (Is_long(vdim))
        ndim=1;
    else
        ndim=Wosize_val(vdim);

    if (ndim > MAX_DIM)
        failwith("matrix_create: invalid matrix dimension");

    mat=(caml_matrix_p)malloc(sizeof(caml_matrix_t));

    if (mat==0)
        failwith("matrix_create: can't allocate matrix structure");

    for(i=0;i<MAX_DIM;i++)
        mat->size[i]=0;

    if (ndim > 1)
      for(i=0;i<ndim;i++)
      {
        int sdim=Int_val(Field(vdim,i));
        size=size*sdim;
        mat->size[i] = sdim; 
      }
    else
    {
        size=Int_val(vdim);
        mat->size[0] = size;
    };
    mat->dim=ndim;
    mat->type=Int_val(vkind);
    if (mat->type == M_AUTO)
    {
        /*
        ** Determine type from initial value. Not all types
        ** are available in this case.
        */
        if (Is_long(vinit))
            mat->type = M_INT;
        else 
        {
            int tag=Tag_val(vinit);
            switch (tag) {
                case Double_tag:
                    mat->type=M_FLOAT;
                    break;
                case Custom_tag:
                    failwith("matrix_create: TODO custom type");
                    break;
                default:
                    failwith("matrix_create: data type not supported");
            };
        };
    };
    size=size*matrix_data_size(mat->type);    

    mat->state=0;

    mat->data=(char *)malloc(size);

    if (mat->data == 0)
        failwith("matrix_create: can't allocate data");



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

    res = alloc_custom(&matrix_ops,
                     sizeof(caml_matrix_p),
                     used, max);

    *((caml_matrix_p *)Data_custom_val(res)) = mat;

    /*
    ** Initialize matrix
    */
    switch (mat->type) {
        case M_INT:
            MATRIX_INIT(mat,int,Int_val(vinit));
            break;
        case M_FLOAT:
            MATRIX_INIT(mat,double,Double_val(vinit));
            break;
        default:
            failwith("matrix_create: type not supported");
    };    


    CAMLreturn(res);
};

#include <module/strmisc.h>

void
print_matrix_info(char *buf,char *end,caml_matrix_p mat)
{
    char *p=buf;
    int i;

    p=bprintf(p,end,"Matrix dim=%d type=%s [",
                    mat->dim,
                    matrix_data_type(mat->type));
    
    for(i=0;i<mat->dim;i++)
    {
        p=bprintf(p,end,"%d",mat->size[i]);
        if (i< (mat->dim-1))
            p=bprintf(p,end,",");
    };
    p=bprintf(p,end,"]");

    return;
};

CAMLexport value
matrix_info (value vmat)
{
    caml_matrix_p mat=Matrix_val(vmat);
    CAMLparam0();
    CAMLlocal1(res);

    char *p=tmpbuf;
    char *end=&tmpbuf[100];

    print_matrix_info(p,end,mat);

    res=copy_string(tmpbuf);

    CAMLreturn(res);
};

CAMLexport value
matrix_get (value vmat, value vdim)
{
    caml_matrix_p mat=Matrix_val(vmat);
    CAMLparam0();
    CAMLlocal1(res);

    int i;
    int dim[MAX_DIM];
    int ndim=0;

    if (Is_long(vdim))
        ndim=1;
    else
        ndim=Wosize_val(vdim); 
    if (ndim != mat->dim)
        failwith("matrix_get: dimension mismatch");
    
    if (ndim > 1)
      for(i=0;i<mat->dim;i++)
        dim[i] = Int_val(Field(vdim,i));
    else
        dim[0] = Int_val(vdim);

    switch (mat->type) {
        case M_INT:
        {
            int *data=(int *)mat->data;
            int n;
            MATRIX_OFF(mat,dim,n); 
            res=Val_int(data[n]);
            break;
        };
        case M_FLOAT:
        {
            double *data=(double *)mat->data;
            int n;
            MATRIX_OFF(mat,dim,n); 
            res=copy_double(data[n]);
            break;
        };
        default:
            failwith("matrix_get: unsupported data type");
    };
    CAMLreturn(res);
};


CAMLexport value
matrix_set (value vmat, value vdim, value vnew)
{
    caml_matrix_p mat=Matrix_val(vmat);
 
    int i;
    int dim[MAX_DIM];

    int ndim=0;

    if (Is_long(vdim))
        ndim=1;
    else
        ndim=Wosize_val(vdim); 
    if (ndim != mat->dim)
        failwith("matrix_set: dimension mismatch");
    
    if (ndim > 1)
      for(i=0;i<mat->dim;i++)
        dim[i] = Int_val(Field(vdim,i));
    else
        dim[0] = Int_val(vdim);


    switch (mat->type) {
        case M_INT:
        {
            int *data=(int *)mat->data;
            int n;
            MATRIX_OFF(mat,dim,n); 
            data[n]=Int_val(vnew);
            break;
        };
        case M_FLOAT:
        {
            double *data=(double *)mat->data;
            int n;
            MATRIX_OFF(mat,dim,n); 
            data[n]=Double_val(vnew);
            break;
        };
        default:
            failwith("matrix_set: unsupported data type");
    };
    return Val_unit;
};


/*
** Return dimension size array
*/
CAMLexport value
matrix_dim (value vmat)
{
    caml_matrix_p mat=Matrix_val(vmat);
    CAMLparam0();
    CAMLlocal1(res);
    int dim=mat->dim;
    int i;

    res=alloc_tuple(dim);

    for(i=0;i<dim;i++)
        Store_field(res,i,Val_int(mat->size[i]));
    

    CAMLreturn(res);
};


static void
matrix_finalize (value v)
{
    caml_matrix_p mat=Matrix_val(v);
    free(mat->data);
};

static long
matrix_hash(value v)
{
    return 0;
};


static int
matrix_compare(value v1,value v2)
{
    caml_matrix_p mat1=Matrix_val(v1);
    caml_matrix_p mat2=Matrix_val(v2);

    int i,size=1;

    if (mat1->dim != mat2->dim ||
        mat1->type != mat2->type)
        return -1;
    else 
    {
        for(i=0;i<mat1->dim;i++)
        {
            if (mat1->size[i] != mat2->size[i]) return -1;
            size=size*mat1->size[i];
        };
        switch (mat1->type) {
            case M_INT:
            {
                int * data1=(int *)mat1->data;
                int * data2=(int *)mat2->data;
                for(i=0;i<size;i++)
                    if (data1[i] > data2[i])
                        return 1;
                    else if (data1[i] < data2[i])
                        return (-1);
                break;
            };
            case M_FLOAT:
            {
                double * data1=(double *)mat1->data;
                double * data2=(double *)mat2->data;
                for(i=0;i<size;i++)
                    if (data1[i] > data2[i])
                        return 1;
                    else if (data1[i] < data2[i])
                        return (-1);
                break;
            };
            default:
                failwith("matrix_compare: type not supported");
        };
    };
    return 0;
};

static void
matrix_serialize(value v,
              unsigned long * wsize_32,
              unsigned long * wsize_64)
                                                              
{
    /* TODO */
    return;
};

unsigned long
matrix_deserialize(void *dst)
{
    /* TODO */
    return 0;
};


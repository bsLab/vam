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
**    $MODIFIED:    
**    $VERSION:     1.02
**
**    $INFO:
**
** Universal machine type support. 
**
** TODO
**  - 64 bit arithmetic, also on 32 bit machines
**  - signed logical operators are only correct for 32 bit values
**
**
**    $ENDOFINFO
**
*/






#define HAVE_LONGLONG


#include <mlvalues.h>
#include <alloc.h>
#include <custom.h>
#include <callback.h>
#include <memory.h>
#include <signals.h>
#include <fail.h>
#include <stdio.h>

#include <string.h>

#include "machtype.h"
#include <byteorder.h>

/*
** Return machine type id
*/
CAMLexport value
unimach_id(value v1)
{
    UCHAR *us;
    int type;
    UCHAR *data;
    if(Is_block(v1) == 0)
        failwith ("unimach_id: not a machine type");    
    if(Tag_val(v1) != String_tag)
        failwith ("unimach_id: not a machine type");    

    us=(UCHAR *)String_val(v1);
    type=MACHTYPE_R(us);

    return (Val_int(type));
};


CAMLexport value 
unimach_compare (value v1,value v2,char comp)
{
    value res;
    SINT i1,i2;
    UINT u,u1,u2;

#ifdef HAVE_LONGLONG
    LLSINT il,il1,il2;
    LLUINT ul,ul1,ul2;
#endif

    UCHAR *data;
    
    if (Is_long(v1)  && Is_long(v2))
    {
        /*
        ** OCaML integer 
        */
        int i1=Int_val(v1);
        int i2=Int_val(v2);
        switch (comp) 
        {
            case 'l':
                res=Val_int(i1<i2);
                break;
            case 'L':
                res=Val_int(i1<=i2);
                break;
            case 'g':
                res=Val_int(i1>i2);
                break;
            case 'G':
                res=Val_int(i1>=i2);
                break;
            case 'E':
                res=Val_int(i1==i2);
                break;
            default: 
                failwith("prog. error");
          };
    } 
    else if (Is_block(v1) && Tag_val(v1) == String_tag &&
             Is_block(v2) && Tag_val(v2) == String_tag)
    {
        /*
        ** unitype
        */ 
        UCHAR *us1=(unsigned char*)String_val(v1);
        int type1=MACHTYPE_R(us1);
        UCHAR *us2=(unsigned char*)String_val(v2);
        int type2=MACHTYPE_R(us2);

        if (type1 != type2)
            failwith ("unimach_compare: arguments of different types\n");
        switch(type1) {
            case INT8:
            {
                ARCH_INT8_TYPE i1=INT8_R(MACHDATA(us1));
                ARCH_INT8_TYPE i2=INT8_R(MACHDATA(us2));

                switch (comp) 
                {
                    case 'l':
                        res=Val_int(i1<i2);
                        break;
                    case 'L':
                        res=Val_int(i1<=i2);
                        break;
                    case 'g':
                        res=Val_int(i1>i2);
                        break;
                    case 'G':
                        res=Val_int(i1>=i2);
                        break;
                    case 'E':
                        res=Val_int(i1==i2);
                        break;
                    default: 
                        failwith("prog. error");
                };
            }
            case INT16:
            {
                ARCH_INT16_TYPE i1=INT16_R(MACHDATA(us1));
                ARCH_INT16_TYPE i2=INT16_R(MACHDATA(us2));
    
                switch (comp) 
                {
                    case 'l':
                        res=Val_int(i1<i2);
                        break;
                    case 'L':
                        res=Val_int(i1<=i2);
                        break;
                    case 'g':
                        res=Val_int(i1>i2);
                        break;
                    case 'G':
                        res=Val_int(i1>=i2);
                        break;
                    case 'E':
                        res=Val_int(i1==i2);
                        break;
                    default: 
                        failwith("prog. error");
                };
            }
            break;
            case INT32:
            {
                ARCH_INT32_TYPE i1=INT32_R(MACHDATA(us1));
                ARCH_INT32_TYPE i2=INT32_R(MACHDATA(us2));
                
                switch (comp) 
                {
                    case 'l':
                        res=Val_int(i1<i2);
                        break;
                    case 'L':
                        res=Val_int(i1<=i2);
                        break;
                    case 'g':
                        res=Val_int(i1>i2);
                        break;
                    case 'G':
                        res=Val_int(i1>=i2);
                        break;
                    case 'E':
                        res=Val_int(i1==i2);
                        break;
                    default: 
                        failwith("prog. error");
                };
            }
            break;
#ifdef HAVE_LONGLONG
            case INT64:
            {
                ARCH_INT64_TYPE i1=INT64_R(MACHDATA(us1));
                ARCH_INT64_TYPE i2=INT64_R(MACHDATA(us2));
                
                switch (comp) 
                {
                    case 'l':
                        res=Val_int(il1<il2);
                        break;
                    case 'L':
                        res=Val_int(il1<=il2);
                        break;
                    case 'g':
                        res=Val_int(il1>il2);
                        break;
                    case 'G':
                        res=Val_int(il1>=il2);
                        break;
                    case 'E':
                        res=Val_int(il1==il2);
                        break;
                    default: 
                        failwith("prog. error");
                };
            }
            break;
#endif

            case WORD8:
            case UINT8:
            {
                ARCH_UINT8_TYPE u1=UINT8_R(MACHDATA(us1));
                ARCH_UINT8_TYPE u2=UINT8_R(MACHDATA(us2));
                
                switch (comp) 
                {
                    case 'l':
                        res=Val_int(u1<u2);
                        break;
                    case 'L':
                        res=Val_int(u1<=u2);
                        break;
                    case 'g':
                        res=Val_int(u1>u2);
                        break;
                    case 'G':
                        res=Val_int(u1>=u2);
                        break;
                    case 'E':
                        res=Val_int(u1==u2);
                        break;
                    default: 
                        failwith("prog. error");
                };
            }
            break;
            case WORD16:
            case UINT16:
            {
                ARCH_UINT16_TYPE u1=UINT16_R(MACHDATA(us1));
                ARCH_UINT16_TYPE u2=UINT16_R(MACHDATA(us2));

                
                switch (comp) 
                {
                    case 'l':
                        res=Val_int(u1<u2);
                        break;
                    case 'L':
                        res=Val_int(u1<=u2);
                        break;
                    case 'g':
                        res=Val_int(u1>u2);
                        break;
                    case 'G':
                        res=Val_int(u1>=u2);
                        break;
                    case 'E':
                        res=Val_int(u1==u2);
                        break;
                    default: 
                        failwith("prog. error");
                };
            }
            break;
            case WORD32:
            case UINT32:
            {
                ARCH_UINT32_TYPE u1=UINT32_R(MACHDATA(us1));
                ARCH_UINT32_TYPE u2=UINT32_R(MACHDATA(us2));

                
                switch (comp) 
                {
                    case 'l':
                        res=Val_int(u1<u2);
                        break;
                    case 'L':
                        res=Val_int(u1<=u2);
                        break;
                    case 'g':
                        res=Val_int(u1>u2);
                        break;
                    case 'G':
                        res=Val_int(u1>=u2);
                        break;
                    case 'E':
                        res=Val_int(u1==u2);
                        break;
                    default: 
                        failwith("prog. error");
                };
            }
            break;
            default: failwith ("unimach_compare: unsupported type\n");
        };
    }
    else if (Is_block(v1) && Tag_val(v1) == Double_tag &&
             Is_block(v2) && Tag_val(v2) == Double_tag)
    {
        /*
        ** float
        */
        double d1=Double_val(v1);
        double d2=Double_val(v2);
        switch (comp) 
        {
            case 'l':
                res=Val_int(d1<d2);
                break;
            case 'L':
                res=Val_int(d1<=d2);
                break;
            case 'g':
                res=Val_int(d1>d2);
                break;
            case 'G':
                res=Val_int(d1>=d2);
                break;
            case 'E':
                res=Val_int(d1==d2);
                break;
            default: 
                failwith("prog. error");
          };
    
    }
    else failwith ("unimach_lt: unsupported or different argument types\n");
 
    return res;
}



CAMLexport value 
unimach_lt (value v1,value v2)
{
    return unimach_compare(v1,v2,'l');
};
CAMLexport value 
unimach_le (value v1,value v2)
{
    return unimach_compare(v1,v2,'L');
};




CAMLexport value 
unimach_gt (value v1,value v2)
{
    return unimach_compare(v1,v2,'g');
};
CAMLexport value 
unimach_ge (value v1,value v2)
{
    return unimach_compare(v1,v2,'G');
};

CAMLexport value 
unimach_eq (value v1,value v2)
{
    return unimach_compare(v1,v2,'E');
};



/*
** Conversion and creation of unidata values.
*/

CAMLexport value 
unimach_to_int(value v1)
{
    UCHAR *us;
    int type;
    int i=0;

    if(Is_block(v1) == 0)
        failwith ("unimach_to_int: not a machine type");    
    if(Tag_val(v1) != String_tag)
        failwith ("unimach_to_int: not a machine type");    

    us=(unsigned char*)String_val(v1);
    type=MACHTYPE_R(us);

    switch (type) {
        case UINT8:
        case WORD8:
        {
            ARCH_UINT8_TYPE d=UINT8_R(MACHDATA(us));
            i=(int)d;
        }
        break;
        case UINT16:
        case WORD16:
        {
            ARCH_UINT16_TYPE d=UINT16_R(MACHDATA(us));
            i=(int)d;
        }
        break;
        case UINT32:
        case WORD32:
        {
            ARCH_UINT32_TYPE d=UINT32_R(MACHDATA(us));
            i=(int)d;
        }
        break;

        case INT8:
        {
            ARCH_INT8_TYPE d=INT8_R(MACHDATA(us));
            i=(int)d;
        }
        break;
        case INT16:
        {
            ARCH_INT16_TYPE d=INT16_R(MACHDATA(us));
            i=(int)d;
        }
        break;
        case INT32:
        {
            ARCH_INT32_TYPE d=INT32_R(MACHDATA(us));
            i=(int)d;
        }
        break;
         
        default: failwith ("to_int: unknown or unsupported type\n");
    };    
    return Val_int(i);
}

CAMLexport value 
unimach_of_int(value vi, value vt)
{
    CAMLparam0();
    CAMLlocal1(res);

    UCHAR *data=(UCHAR)NULL;
    int i = Int_val(vi);
    int type = Int_val(vt);
        

    switch (type) {
        case UINT8:
        case WORD8:
        {
            UINT u=(UINT)i;
            res=alloc_string(2);
            data=(UCHAR*)String_val(res);
            UINT8_W(MACHDATA(data),i);
        }
        break;
        case UINT16:
        case WORD16:
        {
            UINT u=(UINT)i;
            res=alloc_string(1+SIZEOF_SHORT);
            data=(UCHAR*)String_val(res);
            UINT16_W(MACHDATA(data),i);
        }
        break;
        case UINT32:
        case WORD32:
        {
            UINT u=(UINT)i;
            res=alloc_string(1+SIZEOF_INT);
            data=(UCHAR*)String_val(res);
            UINT32_W(MACHDATA(data),i);
        }
        break;
        case INT8:
        {
            SINT s=(SINT)i;
            res=alloc_string(2);
            data=(UCHAR*)String_val(res);
            INT8_W(MACHDATA(data),i);
        }
        break;
        case INT16:
        {
            UINT u=(UINT)i;
            res=alloc_string(1+SIZEOF_SHORT);
            data=(UCHAR*)String_val(res);
            INT16_W(MACHDATA(data),i);
        }
        break;
        case INT32:
        {
            UINT u=(UINT)i;
            res=alloc_string(1+SIZEOF_INT);
            data=(UCHAR*)String_val(res);
            INT32_W(MACHDATA(data),i);
        }
        break;
        default: failwith ("of_int: unknown or unsupported type\n");
    };    
    if (data == NULL)
        failwith("of_int: data error\n");

    MACHTYPE_W(data,type);

    CAMLreturn(res);
};


static char * parse_sign_and_base(char * p,
                                  /*out*/ int * base,
                                  /*out*/ int * sign)
{
  *sign = 1;
  if (*p == '-') {
    *sign = -1;
    p++;
  }
  *base = 10;
  if (*p == '0') {
    switch (p[1]) {
    case 'x': case 'X':
      *base = 16; p += 2; break;
    case 'o': case 'O':
      *base = 8; p += 2; break;
    case 'b': case 'B':
      *base = 2; p += 2; break;
    }
  }
  return p;
};

static int parse_digit(char * p)
{
  int c = *p;
  if (c >= '0' && c <= '9')
    return c - '0';
  else if (c >= 'A' && c <= 'F')
    return c - 'A' + 10;
  else if (c >= 'a' && c <= 'f')
    return c - 'a' + 10;
  else
    return -1;
};

static long parse_long(char * p)
{
  unsigned long res;
  int sign, base, d;

  p = parse_sign_and_base(p, &base, &sign);
  d = parse_digit(p);
  if (d < 0 || d >= base) failwith("int_of_string");
  for (p++, res = d; /*nothing*/; p++) {
    d = parse_digit(p);
    if (d < 0 || d >= base) break;
    res = base * res + d;
  }
  if (*p != 0) failwith("of_string\n");
  return sign < 0 ? -((long) res) : (long) res;
};

CAMLexport value 
unimach_of_str(value vs, value vt)
{
    CAMLparam0();
    CAMLlocal1(res);

    UCHAR *data=(UCHAR)NULL;
    char *str = String_val(vs);
    int i=parse_long(str);
    int type = Int_val(vt);
        

    switch (type) {
        case UINT8:
        case WORD8:
        {
            UINT u=(UINT)i;
            res=alloc_string(2);
            data=(UCHAR*)String_val(res);
            UINT8_W(MACHDATA(data),i);
        }
        break;
        case UINT16:
        case WORD16:
        {
            UINT u=(UINT)i;
            res=alloc_string(3);
            data=(UCHAR*)String_val(res);
            UINT16_W(MACHDATA(data),i);
        }
        break;
        case UINT32:
        case WORD32:
        {
            UINT u=(UINT)i;
            res=alloc_string(5);
            data=(UCHAR*)String_val(res);
            UINT32_W(MACHDATA(data),i);
        }
        break;
        case INT8:
        {
            SINT s=(SINT)i;
            res=alloc_string(2);
            data=(UCHAR*)String_val(res);
            INT8_W(MACHDATA(data),i);
        }
        break;
        case INT16:
        {
            UINT u=(UINT)i;
            res=alloc_string(3);
            data=(UCHAR*)String_val(res);
            INT16_W(MACHDATA(data),i);
        }
        break;
        case INT32:
        {
            UINT u=(UINT)i;
            res=alloc_string(5);
            data=(UCHAR*)String_val(res);
            INT32_W(MACHDATA(data),i);
        }
        break;
        default: failwith ("of_str: unknown or unsupported type\n");
    };    
    if (data == NULL)
        failwith("of_str: data error\n");

    data[0]=(UCHAR)type;

    CAMLreturn(res);
}
                                                                                                                        

/*
** Arithmetic operations. Possible argument types:
**
** 1. int
** 2. float
** 3. unidata (of course!)
**
*/

CAMLexport value 
unimach_op (value v1,value v2,char op)
{
    CAMLparam0();
    CAMLlocal1(res);

    
    if (Is_long(v1)  && Is_long(v2))
    {
        /*
        ** OCaML integer 
        */
        switch (op) {
            case '+': res=Val_int(Int_val(v1) + Int_val(v2)); break;
            case '-': res=Val_int(Int_val(v1) - Int_val(v2)); break;
            case '*': res=Val_int(Int_val(v1) * Int_val(v2)); break;
            case '/': res=Val_int(Int_val(v1) / Int_val(v2)); break;
            case '&': res=Val_int(Int_val(v1) & Int_val(v2)); break;
            case '|': res=Val_int(Int_val(v1) | Int_val(v2)); break;
            default: invalid_argument("type not supported");
        };
    } 
    else if (Is_block(v1) && Tag_val(v1) == String_tag &&
             Is_block(v2) && Tag_val(v2) == String_tag)
    {
        /*
        ** unitype
        */ 
        UCHAR *us1=(UCHAR*)String_val(v1);
        int type1=MACHTYPE_R(us1);
        UCHAR *us2=(UCHAR*)String_val(v2);
        int type2=MACHTYPE_R(us2);
        UCHAR *usr;

        if (type1 != type2)
            failwith ("unimach_op: arguments of different types\n");

        switch(type1) {
            case INT8:
            {
                ARCH_INT8_TYPE i1=INT8_R(MACHDATA(us1));
                ARCH_INT8_TYPE i2=INT8_R(MACHDATA(us2));
                ARCH_INT8_TYPE rs;
                switch (op) {
                    case '+': rs=i1+i2; break;
                    case '-': rs=i1-i2; break;
                    case '*': rs=i1*i2; break;
                    case '/': rs=i1/i2; break;
                    case '&': rs=i1&i2; break;
                    case '|': rs=i1|i2; break;
                    default: invalid_argument("type not supported");
                };

                res=alloc_string(2);
                usr=(UCHAR*)String_val(res);
                INT8_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,INT8);                
            }
            case INT16:
            {
                ARCH_INT16_TYPE i1=INT16_R(MACHDATA(us1));
                ARCH_INT16_TYPE i2=INT16_R(MACHDATA(us2));
                ARCH_INT16_TYPE rs=i1+i2;
                switch (op) {
                    case '+': rs=i1+i2; break;
                    case '-': rs=i1-i2; break;
                    case '*': rs=i1*i2; break;
                    case '/': rs=i1/i2; break;
                    case '&': rs=i1&i2; break;
                    case '|': rs=i1|i2; break;
                    default: invalid_argument("type not supported");
                };
                res=alloc_string(1+SIZEOF_SHORT);
                usr=(UCHAR*)String_val(res);
                INT16_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,INT16);                
            }
            break;
            case INT32:
            {
                ARCH_INT32_TYPE i1=INT32_R(MACHDATA(us1));
                ARCH_INT32_TYPE i2=INT32_R(MACHDATA(us2));
                ARCH_INT32_TYPE rs=i1+i2;
                switch (op) {
                    case '+': rs=i1+i2; break;
                    case '-': rs=i1-i2; break;
                    case '*': rs=i1*i2; break;
                    case '/': rs=i1/i2; break;
                    case '&': rs=i1&i2; break;
                    case '|': rs=i1|i2; break;
                    default: invalid_argument("type not supported");
                };
                res=alloc_string(1+SIZEOF_INT);
                usr=(UCHAR*)String_val(res);
                INT32_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,INT32);                
            }
            break;

            case WORD8:
            case UINT8:
            {
                ARCH_UINT8_TYPE i1=UINT8_R(MACHDATA(us1));
                ARCH_UINT8_TYPE i2=UINT8_R(MACHDATA(us2));
                ARCH_UINT8_TYPE rs=i1+i2;
                switch (op) {
                    case '+': rs=i1+i2; break;
                    case '-': rs=i1-i2; break;
                    case '*': rs=i1*i2; break;
                    case '/': rs=i1/i2; break;
                    case '&': rs=i1&i2; break;
                    case '|': rs=i1|i2; break;
                    default: invalid_argument("type not supported");
                };
                res=alloc_string(2);
                usr=(UCHAR*)String_val(res);
                UINT8_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,type1);                
            }
            break;
            case WORD16:
            case UINT16:
            {
                ARCH_UINT16_TYPE i1=UINT16_R(MACHDATA(us1));
                ARCH_UINT16_TYPE i2=UINT16_R(MACHDATA(us2));
                ARCH_UINT16_TYPE rs=i1+i2;
                switch (op) {
                    case '+': rs=i1+i2; break;
                    case '-': rs=i1-i2; break;
                    case '*': rs=i1*i2; break;
                    case '/': rs=i1/i2; break;
                    case '&': rs=i1&i2; break;
                    case '|': rs=i1|i2; break;
                    default: invalid_argument("type not supported");
                };
                res=alloc_string(1+SIZEOF_SHORT);
                usr=(UCHAR*)String_val(res);
                UINT16_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,type1);                
            }
            break;
            case WORD32:
            case UINT32:
            {
                ARCH_UINT32_TYPE i1=UINT32_R(MACHDATA(us1));
                ARCH_UINT32_TYPE i2=UINT32_R(MACHDATA(us2));
                ARCH_UINT32_TYPE rs=i1+i2;
                switch (op) {
                    case '+': rs=i1+i2; break;
                    case '-': rs=i1-i2; break;
                    case '*': rs=i1*i2; break;
                    case '/': rs=i1/i2; break;
                    case '&': rs=i1&i2; break;
                    case '|': rs=i1|i2; break;
                    default: invalid_argument("type not supported");
                };
                res=alloc_string(1+SIZEOF_INT);
                usr=(UCHAR*)String_val(res);
                UINT32_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,type1);                
            }
            break;
            default: failwith ("unimach_opp: unsupported type\n");
        };
    }
    else if (Is_block(v1) && Tag_val(v1) == Double_tag &&
             Is_block(v2) && Tag_val(v2) == Double_tag)
    {
        /*
        ** float
        */
        float d;
        switch (op) {
            case '+':d=(Double_val(v1)+Double_val(v2)); break;
            case '-':d=(Double_val(v1)-Double_val(v2)); break;
            case '*':d=(Double_val(v1)*Double_val(v2)); break;
            case '/':d=(Double_val(v1)/Double_val(v2)); break;
            default: invalid_argument("type not supported");
        };
        res=copy_double(d);
    }
    else failwith ("unimach_op: unsupported or different argument types\n");
 
    CAMLreturn(res);    
}

CAMLexport value
unimach_add (value v1,value v2)
{
    return unimach_op(v1,v2,'+');
};
CAMLexport value
unimach_sub (value v1,value v2)
{
    return unimach_op(v1,v2,'-');
};
CAMLexport value
unimach_mul (value v1,value v2)
{
    return unimach_op(v1,v2,'*');
};
CAMLexport value
unimach_div (value v1,value v2)
{
    return unimach_op(v1,v2,'/');
};
CAMLexport value
unimach_land (value v1,value v2)
{
    return unimach_op(v1,v2,'&');
};
CAMLexport value
unimach_lor (value v1,value v2)
{
    return unimach_op(v1,v2,'|');
};




CAMLexport value 
unimach_lnot (value v1)
{
    CAMLparam0();
    CAMLlocal1(res);

    SINT i1;
    UCHAR *usr;
    
    if (Is_long(v1))
    {
        /*
        ** OCaML integer 
        */
        res=Val_int(~Int_val(v1));
    } 
    else if (Is_block(v1) && Tag_val(v1) == String_tag)
    {
        /*
        ** unitype
        */ 
        UCHAR *us1=(unsigned char*)String_val(v1);
        int type1=MACHTYPE_R(us1);

        switch(type1) {
            case INT8:
            {
                ARCH_INT8_TYPE i1=INT8_R(MACHDATA(us1));
                ARCH_INT8_TYPE rs=~i1;
                res=alloc_string(2);
                usr=(UCHAR*)String_val(res);
                INT8_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,INT8);                
            }
            case INT16:
            {
                ARCH_INT16_TYPE i1=INT16_R(MACHDATA(us1));
                ARCH_INT16_TYPE rs=~i1;
                res=alloc_string(1+SIZEOF_SHORT);
                usr=(UCHAR*)String_val(res);
                INT16_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,INT16);                
            }
            break;
            case INT32:
            {
                ARCH_INT32_TYPE i1=INT32_R(MACHDATA(us1));
                ARCH_INT32_TYPE rs=~i1;
                res=alloc_string(1+SIZEOF_INT);
                usr=(UCHAR*)String_val(res);
                INT32_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,INT32);                
            }
            break;
            case WORD8:
            case UINT8:
            {
                ARCH_UINT8_TYPE i1=UINT8_R(MACHDATA(us1));
                ARCH_UINT8_TYPE rs=~i1;
                res=alloc_string(2);
                usr=(UCHAR*)String_val(res);
                UINT8_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,type1);
            }
            break;
            case WORD16:
            case UINT16:
            {
                ARCH_UINT16_TYPE i1=UINT16_R(MACHDATA(us1));
                ARCH_UINT16_TYPE rs=~i1;
                res=alloc_string(1+SIZEOF_SHORT);
                usr=(UCHAR*)String_val(res);
                UINT16_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,type1);                
            }
            break;
            case WORD32:
            case UINT32:
            {
                ARCH_UINT32_TYPE i1=UINT32_R(MACHDATA(us1));
                ARCH_UINT32_TYPE rs=~i1;
                res=alloc_string(1+SIZEOF_INT);
                usr=(UCHAR*)String_val(res);
                UINT32_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,type1);                
            }
            break;
            default: failwith ("unimach_lnot: unsupported type\n");
        };
    }
    else
        failwith ("unimach_lnot: unsupported or different types\n");

    CAMLreturn(res);
}

CAMLexport value 
unimach_lsr (value v1,value v2)
{
    CAMLparam0();
    CAMLlocal1(res);

    SINT i1;
    UCHAR *usr;
    int shift=Int_val(v2);
    
    if (Is_long(v1))
    {
        /*
        ** OCaML integer 
        */
        res=Val_int(Int_val(v1) >> shift);
    } 
    else if (Is_block(v1) && Tag_val(v1) == String_tag)
    {
        /*
        ** unitype
        */ 
        UCHAR *us1=(unsigned char*)String_val(v1);
        int type1=MACHTYPE_R(us1);

        switch(type1) {
            case INT8:
            {
                ARCH_INT8_TYPE i1=INT8_R(MACHDATA(us1));
                ARCH_INT8_TYPE rs=i1 >> shift;
                res=alloc_string(2);
                usr=(UCHAR*)String_val(res);
                INT8_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,INT8);                
            }
            case INT16:
            {
                ARCH_INT16_TYPE i1=INT16_R(MACHDATA(us1));
                ARCH_INT16_TYPE rs=i1 >> shift;
                res=alloc_string(1+SIZEOF_SHORT);
                usr=(UCHAR*)String_val(res);
                INT16_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,INT16);                
            }
            break;
            case INT32:
            {
                ARCH_INT32_TYPE i1=INT32_R(MACHDATA(us1));
                ARCH_INT32_TYPE rs=i1 >> shift;
                res=alloc_string(1+SIZEOF_INT);
                usr=(UCHAR*)String_val(res);
                INT32_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,INT32);                
            }
            break;
            case WORD8:
            case UINT8:
            {
                ARCH_UINT8_TYPE i1=UINT8_R(MACHDATA(us1));
                ARCH_UINT8_TYPE rs=i1 >> shift;
                res=alloc_string(2);
                usr=(UCHAR*)String_val(res);
                UINT8_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,type1);                
            }
            break;
            case WORD16:
            case UINT16:
            {
                ARCH_UINT16_TYPE i1=UINT16_R(MACHDATA(us1));
                ARCH_UINT16_TYPE rs=i1 >> shift;
                res=alloc_string(1+SIZEOF_SHORT);
                usr=(UCHAR*)String_val(res);
                UINT16_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,type1);                
            }
            break;
            case WORD32:
            case UINT32:
            {
                ARCH_UINT32_TYPE i1=UINT32_R(MACHDATA(us1));
                ARCH_UINT32_TYPE rs=i1 >> shift;
                res=alloc_string(1+SIZEOF_INT);
                usr=(UCHAR*)String_val(res);
                UINT32_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,type1);                
            }
            break;
            default: failwith ("unimach_lsr: unsupported type\n");
        };
    }
    else
        failwith ("unimach_lsr: unsupported or different types\n");

    CAMLreturn(res);
}

CAMLexport value 
unimach_lsl (value v1,value v2)
{
    CAMLparam0();
    CAMLlocal1(res);

    SINT i1;
    UCHAR *usr;
    int shift=Int_val(v2);
    
    if (Is_long(v1))
    {
        /*
        ** OCaML integer 
        */
        res=Val_int(Int_val(v1) << shift);
    } 
    else if (Is_block(v1) && Tag_val(v1) == String_tag)
    {
        /*
        ** unitype
        */ 
        UCHAR *us1=(unsigned char*)String_val(v1);
        int type1=MACHTYPE_R(us1);

        switch(type1) {
            case INT8:
            {
                ARCH_INT8_TYPE i1=INT8_R(MACHDATA(us1));
                ARCH_INT8_TYPE rs=i1 << shift;
                res=alloc_string(2);
                usr=(UCHAR*)String_val(res);
                INT8_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,INT8);                
            }
            case INT16:
            {
                ARCH_INT16_TYPE i1=INT16_R(MACHDATA(us1));
                ARCH_INT16_TYPE rs=i1 << shift;
                res=alloc_string(1+SIZEOF_SHORT);
                usr=(UCHAR*)String_val(res);
                INT16_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,INT16);                
            }
            break;
            case INT32:
            {
                ARCH_INT32_TYPE i1=INT32_R(MACHDATA(us1));
                ARCH_INT32_TYPE rs=i1 << shift;
                res=alloc_string(1+SIZEOF_INT);
                usr=(UCHAR*)String_val(res);
                INT32_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,INT32);                
            }
            break;
            case WORD8:
            case UINT8:
            {
                ARCH_UINT8_TYPE i1=UINT8_R(MACHDATA(us1));
                ARCH_UINT8_TYPE rs=i1 << shift;
                res=alloc_string(2);
                usr=(UCHAR*)String_val(res);
                UINT8_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,type1);                
            }
            break;
            case WORD16:
            case UINT16:
            {
                ARCH_UINT16_TYPE i1=UINT16_R(MACHDATA(us1));
                ARCH_UINT16_TYPE rs=i1 << shift;
                res=alloc_string(1+SIZEOF_SHORT);
                usr=(UCHAR*)String_val(res);
                UINT16_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,type1);                
            }
            break;
            case WORD32:
            case UINT32:
            {
                ARCH_UINT32_TYPE i1=UINT32_R(MACHDATA(us1));
                ARCH_UINT32_TYPE rs=i1 << shift;
                res=alloc_string(1+SIZEOF_INT);
                usr=(UCHAR*)String_val(res);
                UINT32_W(MACHDATA(usr),rs);
                MACHTYPE_W(usr,type1);                
            }
            break;
            default: failwith ("unimach_lsl: unsupported type\n");
        };
    }
    else
        failwith ("unimach_lsl: unsupported or different types\n");

    CAMLreturn(res);
}




#define FORMAT_BUFFER_SIZE 32

static char * parse_format(value fmt,
                           char * suffix,
                           char format_string[],
                           char default_format_buffer[])
{
  int prec, lastletter;
  char * p;
  mlsize_t len, len_suffix;

  /*
  ** Copy Caml format fmt to format_string,
  ** adding the suffix before the last letter of the format 
  */
  len = string_length(fmt);
  len_suffix = strlen(suffix);
  if (len + len_suffix + 1 >= FORMAT_BUFFER_SIZE)
    invalid_argument("format_int: format too long");
  memmove(format_string, String_val(fmt), len);
  p = format_string + len - 1;
  lastletter = *p;
  /* Compress two-letter formats, ignoring the [lnL] annotation */
  if (p[-1] == 'l' || p[-1] == 'n' || p[-1] == 'L') p--;
  memmove(p, suffix, len_suffix);  p += len_suffix;
  *p++ = lastletter;
  *p = 0;
  /*
  ** Determine space needed for result and allocate it dynamically if needed 
  */
  prec = 22 + 5; /* 22 digits for 64-bit number in octal + 5 extra */
  for (p = String_val(fmt); *p != 0; p++) {
    if (*p >= '0' && *p <= '9') {
      prec = atoi(p) + 5;
      break;
    }
  }
  if (prec < FORMAT_BUFFER_SIZE)
    return default_format_buffer;
  else
    return stat_alloc(prec + 1);
}

CAMLprim value unimach_format(value fmt, value arg)
{
  UCHAR *us=(unsigned char*)String_val(arg);
  int type=MACHTYPE_R(us);
  int i;

                 
  char format_string[FORMAT_BUFFER_SIZE];
  char default_format_buffer[FORMAT_BUFFER_SIZE];
  char * buffer;
  value res;

  if(Tag_val(arg) != String_tag)
        failwith ("unimach_format: not a machine type");    

  buffer = parse_format(fmt, "", format_string, default_format_buffer);
    
  switch(type) {
        case UINT8:
        case WORD8:
        {
            ARCH_UINT8_TYPE d=UINT8_R(MACHDATA(us));;
            i=(int)d;
            sprintf(buffer, format_string, i);
        }
        break;
        case UINT16:
        case WORD16:
        {
            ARCH_UINT16_TYPE d=UINT16_R(MACHDATA(us));;
            i=(int)d;
            sprintf(buffer, format_string, i);        
        }
        break;
        case UINT32:
        case WORD32:
        {
            ARCH_UINT32_TYPE d=UINT32_R(MACHDATA(us));;
            i=(int)d;
            sprintf(buffer, format_string, i);        
        }
        break;

        case INT8:
        {
            ARCH_INT8_TYPE d=INT8_R(MACHDATA(us));;
            i=(int)d;
            sprintf(buffer, format_string, i);        
        }
        break;
        case INT16:
        {
            ARCH_INT16_TYPE d=INT16_R(MACHDATA(us));;
            i=(int)d;
            sprintf(buffer, format_string, i);        
        }
        break;
        case INT32:
        {
            ARCH_INT32_TYPE d=INT32_R(MACHDATA(us));;
            i=(int)d;
            sprintf(buffer, format_string, i);        
        }
        break;
        default: failwith ("format_machtype: unsupported type");
  };

  res = copy_string(buffer);
  if (buffer != default_format_buffer) stat_free(buffer);
  return res;
}

/*
** Convert machtype to string with content little endian order
*/

CAMLexport value 
unimach_to_data(value v1)
{
    CAMLparam0();
    CAMLlocal1(res);

    UCHAR *us=(UCHAR*)String_val(v1);
    UCHAR *urs;

    int type=MACHTYPE_R(us);
    UCHAR *data=MACHDATA(us);

    if(Is_block(v1) == 0)
        failwith ("unimach_to_data: not a machine type");    
    if(Tag_val(v1) != String_tag)
        failwith ("unimach_to_data: not a machine type");    

    switch (type) {
        case INT8:
        case UINT8:
        case WORD8:
        {
            res=alloc_string(1);
            urs=(UCHAR*)String_val(res);
            urs[0]=data[0];
            break;
        };
        case INT16:
        case UINT16:
        case WORD16:
        {
            res=alloc_string(SIZEOF_SHORT);
            urs=(UCHAR*)String_val(res);
            memcpy(urs,data,SIZEOF_SHORT);
            dec_s_le((ARCH_UINT16_TYPE*)urs);
            break;
        };
        case INT32:
        case UINT32:
        case WORD32:
        {
            res=alloc_string(SIZEOF_INT);
            urs=(UCHAR*)String_val(res);
            memcpy(urs,data,SIZEOF_INT);
            dec_l_le((ARCH_UINT32_TYPE*)urs);
            break;
        };
        default: failwith("prog error");

    };

    CAMLreturn(res);

}

/*
** Convert little endian format back to local endianess and return
** a machine type.
*/

CAMLexport value 
unimach_of_data(value v1,value v2)
{
    CAMLparam0();
    CAMLlocal1(res);

    UCHAR *str=(UCHAR *)String_val(v1);
    int type=Int_val(v2);
    
    UCHAR *urs;


    switch (type) {
        case INT8:
        case UINT8:
        case WORD8:
        {
            res=alloc_string(2);
            urs=(UCHAR*)String_val(res);
            MACHTYPE_W(urs,type);
            urs[1]=str[0];
            break;
        };
        case INT16:
        case UINT16:
        case WORD16:
        {
            res=alloc_string(1+SIZEOF_SHORT);
            urs=(UCHAR*)String_val(res);
            MACHTYPE_W(urs,type);
            memcpy(MACHDATA(urs),str,SIZEOF_SHORT);
            enc_s_le((ARCH_UINT16_TYPE*)MACHDATA(urs));
            break;
        };
        case INT32:
        case UINT32:
        case WORD32:
        {
            res=alloc_string(1+SIZEOF_INT);
            urs=(UCHAR*)String_val(res);
            MACHTYPE_W(urs,type);
            memcpy(MACHDATA(urs),str,SIZEOF_INT);
            enc_l_le((ARCH_UINT32_TYPE*)MACHDATA(urs));
            break;
        };
        default: failwith("prog error");

    };
    
    CAMLreturn(res);
}

CAMLexport value 
unimach_dec_be(value v1)
{
    CAMLparam0();
    CAMLlocal1(res);

    UCHAR *us=(UCHAR *)String_val(v1);
    int type=MACHTYPE_R(us);
    
    UCHAR *urs;


    switch (type) {
        case INT8:
        case UINT8:
        case WORD8:
        {
            res=alloc_string(2);
            urs=(UCHAR*)String_val(res);
            MACHTYPE_W(urs,type);
            urs[1]=us[1];
            break;
        };
        case INT16:
        case UINT16:
        case WORD16:
        {
            res=alloc_string(1+SIZEOF_SHORT);
            urs=(UCHAR*)String_val(res);
            MACHTYPE_W(urs,type);
            memcpy(MACHDATA(urs),MACHDATA(us),SIZEOF_SHORT);
            dec_s_be((ARCH_UINT16_TYPE*)MACHDATA(urs));
            break;
        };
        case INT32:
        case UINT32:
        case WORD32:
        {
            res=alloc_string(1+SIZEOF_INT);
            urs=(UCHAR*)String_val(res);
            MACHTYPE_W(urs,type);
            memcpy(MACHDATA(urs),MACHDATA(us),SIZEOF_INT);
            dec_l_be((ARCH_UINT32_TYPE*)MACHDATA(urs));

            break;
        };
        default: failwith("prog error");

    };
    
    CAMLreturn(res);
}

CAMLexport value 
unimach_dec_le(value v1)
{
    CAMLparam0();
    CAMLlocal1(res);

    UCHAR *us=(UCHAR *)String_val(v1);
    int type=MACHTYPE_R(us);
    
    UCHAR *urs;


    switch (type) {
        case INT8:
        case UINT8:
        case WORD8:
        {
            res=alloc_string(2);
            urs=(UCHAR*)String_val(res);
            MACHTYPE_W(urs,type);
            urs[1]=us[1];
            break;
        };
        case INT16:
        case UINT16:
        case WORD16:
        {
            res=alloc_string(1+SIZEOF_SHORT);
            urs=(UCHAR*)String_val(res);
            MACHTYPE_W(urs,type);
            memcpy(MACHDATA(urs),MACHDATA(us),SIZEOF_SHORT);
            dec_s_le((ARCH_UINT16_TYPE*)MACHDATA(urs));
            break;
        };
        case INT32:
        case UINT32:
        case WORD32:
        {
            res=alloc_string(1+SIZEOF_INT);
            urs=(UCHAR*)String_val(res);
            MACHTYPE_W(urs,type);
            memcpy(MACHDATA(urs),MACHDATA(us),SIZEOF_INT);
            dec_l_le((ARCH_UINT32_TYPE*)MACHDATA(urs));
            break;
        };
        default: failwith("prog error");

    };
    
    CAMLreturn(res);
}

CAMLexport value 
unimach_enc_le(value v1)
{
    CAMLparam0();
    CAMLlocal1(res);

    UCHAR *us=(UCHAR *)String_val(v1);
    int type=MACHTYPE_R(us);
    
    UCHAR *urs;


    switch (type) {
        case INT8:
        case UINT8:
        case WORD8:
        {
            res=alloc_string(2);
            urs=(UCHAR*)String_val(res);
            MACHTYPE_W(urs,type);
            urs[1]=us[1];
            break;
        };
        case INT16:
        case UINT16:
        case WORD16:
        {
            res=alloc_string(1+SIZEOF_SHORT);
            urs=(UCHAR*)String_val(res);
            MACHTYPE_W(urs,type);
            memcpy(MACHDATA(urs),MACHDATA(us),SIZEOF_SHORT);
            enc_s_le((ARCH_UINT16_TYPE*)MACHDATA(urs));
            break;
        };
        case INT32:
        case UINT32:
        case WORD32:
        {
            res=alloc_string(1+SIZEOF_INT);
            urs=(UCHAR*)String_val(res);
            MACHTYPE_W(urs,type);
            memcpy(MACHDATA(urs),MACHDATA(us),SIZEOF_INT);
            enc_l_le((ARCH_UINT32_TYPE*)MACHDATA(urs));
            break;
        };
        default: failwith("prog error");

    };
    
    CAMLreturn(res);
}

CAMLexport value 
unimach_enc_be(value v1)
{
    CAMLparam0();
    CAMLlocal1(res);

    UCHAR *us=(UCHAR *)String_val(v1);
    int type=MACHTYPE_R(us);
    
    UCHAR *urs;


    switch (type) {
        case INT8:
        case UINT8:
        case WORD8:
        {
            res=alloc_string(2);
            urs=(UCHAR*)String_val(res);
            MACHTYPE_W(urs,type);
            urs[1]=us[1];
            break;
        };
        case INT16:
        case UINT16:
        case WORD16:
        {
            res=alloc_string(1+SIZEOF_SHORT);
            urs=(UCHAR*)String_val(res);
            MACHTYPE_W(urs,type);
            memcpy(MACHDATA(urs),MACHDATA(us),SIZEOF_SHORT);
            enc_s_be((ARCH_UINT16_TYPE*)MACHDATA(urs));
            break;
        };
        case INT32:
        case UINT32:
        case WORD32:
        {
            res=alloc_string(1+SIZEOF_INT);
            urs=(UCHAR*)String_val(res);
            MACHTYPE_W(urs,type);
            memcpy(MACHDATA(urs),MACHDATA(us),SIZEOF_INT);
            enc_l_be((ARCH_UINT32_TYPE*)MACHDATA(urs));
            break;
        };
        default: failwith("prog error");

    };
    
    CAMLreturn(res);
}


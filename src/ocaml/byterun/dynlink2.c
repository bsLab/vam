#include "alloc.h"
#include "dynlink.h"
#include "fail.h"
#include "mlvalues.h"
#include "memory.h"
#include "misc.h"
#include "osdeps.h"
#include "prims.h"



CAMLprim value dynlink_open_lib(value filename)
{
    value result; 
    result = alloc_small(1, Abstract_tag);
    return result;
};
CAMLprim value dynlink_close_lib(value filename)
{
    return Val_unit;
};
CAMLprim value dynlink_lookup_symbol(value filename,value symbolname)
{
    value result;
    result = alloc_small(1, Abstract_tag);
    return result;
};
CAMLprim value dynlink_add_primitive(value filename)
{
    return Val_unit;
};
CAMLprim value dynlink_get_current_libs(value filename)
{
    value res;
    res = alloc_tuple(4);
    return res;
};

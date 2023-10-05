#!/bin/sh

SED=$1

(echo '#include "mlvalues.h"'; \
 echo '#include "prims.h"'; \
 $SED -e 's/.*/extern value &();/' primitives; \
 echo 'c_primitive builtin_cprim[] = {'; \
 $SED -e 's/.*/ &,/' primitives; \
 echo '  0 };'; \
 echo 'char * names_of_builtin_cprim[] = {'; \
 $SED -e 's/.*/ "&",/' primitives; \
 echo '  0 };'
)
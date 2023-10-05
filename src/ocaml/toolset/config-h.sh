#!/bin/bash
#
# Build an ocaml config file header from preprocessor arguments. -D and -U 
# arguments are permitted.
# 
# config-h.sh <h-template> <destfile> -D1 -D2 -U ....
#

TEMPL=$1
shift
DEST=$1
shift

echo "/* config.h header created from Amakefile.sys */" > $DEST
echo "#ifndef _config_" >> $DEST
echo "#define _config_" >> $DEST

while [ $# -gt 0 ]
do
    case $1 in
        -D*)      echo "" >> $DEST; 
                  OPT=${1/-D/#define };
                  echo -n ${OPT/=/ } >> $DEST;    
                  shift;;          
        -U*)      echo "" >> $DEST; 
                  OPT=${1/-U/#undef };
                  echo -n ${OPT/=/ }  >> $DEST;    
                  shift;;          
        *)        echo -n " $1" >> $DEST; shift;;
    esac
done 

#
# Append template header file
#

cat $TEMPL >> $DEST

exit 0






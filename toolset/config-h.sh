#!/bin/bash
#
# Build an ocaml config file header from preprocessor arguments. -D and -U 
# arguments are permitted.
# 
# config-h.sh <destfile> -D1 -D2 -U ....
#

TEMPL=$1
DEST=$2


echo "/* config.h header created from Amakefile.sys */" > $DEST
echo "#ifndef _config_" >> $DEST
echo "#define _config_" >> $DEST

while [ $# -gt 0 ]
do
    case $1 in
        -D*)      OPT=${1/-D/#define };echo ${OPT/=/ } >> $DEST;    shift;;          
        -U*)      OPT=${1/-U/#undef } ;echo ${OPT/=/ }  >> $DEST;    shift;;          
        *)        shift;;
    esac
done 

#
# Append template header file
#

echo "#endif" >> $DEST
exit 0






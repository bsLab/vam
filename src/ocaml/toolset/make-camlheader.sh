#!/bin/sh
 
ECHO=$1
TR=$2
OPATH=$3
OUT=$4

if [ $OUT = "camlheader" ] 
then
    $ECHO "#!$OPATH" > $OUT
else
    $ECHO "#!" | $TR -d '\012' > $OUT
fi

 
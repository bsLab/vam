#!/bin/bash

source $TOOLSET/build.env

PRG=$1
SRC=$2
OUT=$3



SEDCOMM1="s/@VAMDIR@/${INSTALLDIR//\//\\/}/"
SEDCOMM2="s/@AMUNIX_LIB_DIR@/${AMUNIX_LIB_DIR//\//\\/}/"
SEDCOMM3="s/@AMUNIX_INC_DIR@/${AMUNIX_INC_DIR//\//\\/}/"

$PRG    -e "$SEDCOMM1" \
        -e "$SEDCOMM2" \
        -e "$SEDCOMM3" \
    < $SRC > $OUT


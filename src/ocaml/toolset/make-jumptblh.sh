#!/bin/sh

SED=$1
SRC=$2
OUT=$3

$SED -n -e '/^  /s/ \([A-Z]\)/ \&\&lbl_\1/gp' \
     -e '/^}/q' $SRC > $OUT


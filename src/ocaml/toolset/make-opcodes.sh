#!/bin/sh

SED=$1
AWK=$2
VAMDIR=$3

$SED -n -e '/^enum/p' -e 's/,//g' -e '/^  /p'       \
    $VAMDIR/src/ocaml/byterun/instruct.h |          \
$AWK -f $VAMDIR/src/ocaml/toolset/make-opcodes.awk      \
        > opcodes.ml


#!/bin/sh


SED=$1
ECHO=$2
VAMDIR=$3

($ECHO 'let builtin_exceptions = [|';                               \
$SED -n -e 's|.*/\* \("[A-Za-z_]*"\) \*/$|  \1;|p'                  \
        $VAMDIR/src/ocaml/byterun/fail.h |                          \
$SED -e '$s/;$//';                                                  \
         $ECHO '|]';                                                \
         $ECHO 'let builtin_primitives = [|';                       \
         $SED -e 's/.*/  "&";/' -e '$s/;$//'                        \
         $VAMDIR/build-ocaml/byterun/primitives;                    \
         $ECHO '|]') > runtimedef.ml


#!/bin/sh

SED=$1
shift
SRC=$@

$SED -n -e 							                    \
    's/CAMLprim value \([a-z0-9_][a-z0-9_]*\).*/\1/p'   \
	$SRC


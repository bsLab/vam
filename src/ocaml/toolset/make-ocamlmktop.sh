#!/bin/sh

SED=$1
CHMOD=$2
BINDIR=$3
SRC=$4
DEST=$5

$SED -e "s|%%BINDIR%%|$BINDIR|" $SRC > $DEST
$CHMOD +x $DEST > /dev/null

exit 0
 
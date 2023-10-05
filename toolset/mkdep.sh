#!/bin/bash

##
##      ==================================
##      OOOO   OOOO OOOO  O      O   OOOO
##      O   O  O    O     O     O O  O   O
##      O   O  O    O     O     O O  O   O
##      OOOO   OOOO OOOO  O     OOO  OOOO
##      O   O     O    O  O    O   O O   O
##      O   O     O    O  O    O   O O   O
##      OOOO   OOOO OOOO  OOOO O   O OOOO
##      ================================== 
##      BSSLAB, Dr. Stefan Bosse sci@bsslab.de
##
##    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF: 
##    Free Software Foundation-Europe, GNU GPL License, Version 2
##
##    $MODIFIEDBY:  BSSLAB
##    $AUTHORS:     Stefan Bosse
##    $INITIAL:     (C) 2003 BSSLAB
##    $CREATED:     
##    $MODIFIED:    
##    $VERSION:     1.01
##
##    $INFO:
##
##
##    $ENDOFINFO
##



if [ "X$CONFROOT" != "X" ] 
then
    source $CONFROOT/toolset/build.env
else
    SED=sed
    AWK=awk
    RM=rm
fi

$SED -e '1d' < $1 | $AWK -F"\n" '
{
	z=split($1, inc, " ")
	for (i = 1; i < z; i++)
		print inc[i]
}' > $2

$RM $1

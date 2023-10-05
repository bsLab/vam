#!/bin/sh
#
# Move cmi file from source to dest if existing.
#



if [ -r $1 ] 
then
    mv $1 $2
fi

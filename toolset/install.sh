#!/bin/bash

#
# Install script for those who don't have the BSD install
# program.
#
#

source $TOOLSET/build.env

SOURCE=""
TARGET=""
COMPARE=false
VERBOSE=false

while [ x"$1" != x ]; do
    case $1 in
        -v) shift
            VERBOSE=true
            continue;;

        -d) shift
            if [ ! -d $1 ] 
            then
                if $VERBOSE
                then
                    $ECHO "Making directory $1..."
                fi
                $MKDIR $1
            fi 
            shift
            continue;;

        -C) COMPARE=true
            shift
            continue;;

        -T) TARGETFILE=$2
            shift
            shift
            continue;;

        *)  if [ "X$2" != "X" ]
            then
                SOURCE="$SOURCE $1"
            else
                TARGETDIR=$1
            fi
            shift
            continue;;

    esac
done

if [ "X$TARGETFILE" != "X" ] 
then
    FILE=`$BASENAME $SOURCE`
    if $COMPARE
    then
        if [ -f $TARGETFILE ]
        then
            $CMP $SOURCE $TARGETFILE
            if [ $? = 1 ] 
            then
                if $VERBOSE
                then
                    $ECHO "Installing $i in $TARGETFILE due to difference..."
                fi
                $CP $SOURCE $TARGETFILE
            fi
        else
            if $VERBOSE
            then
                $ECHO "Installing $SOURCE in $TARGETFILE..."
            fi
            $CP $SOURCE $TARGETFILE
    
        fi
    else
        if $VERBOSE
        then
            $ECHO "Installing $SOURCE in $TARGETFILE..."
        fi
        $CP $SOURCE $TARGETFILE
    fi

else

  for i in $SOURCE
  do
    FILE=`$BASENAME $i`
    if $COMPARE
    then
        if [ -f $TARGETDIR/$FILE ]
        then
            $CMP $i $TARGETDIR/$FILE
            if [ $? = 1 ] 
            then
                if $VERBOSE
                then
                    $ECHO "Installing $i in $TARGETDIR/$FILE due to difference..."
                fi
                $CP $i $TARGETDIR/$FILE
            fi
        else
            if $VERBOSE
            then
                $ECHO "Installing $i in $TARGETDIR/$FILE..."
            fi
            $CP $i $TARGETDIR/$FILE
    
        fi
    else
        if $VERBOSE
        then
            $ECHO "Installing $i in $TARGETDIR/$FILE..."
        fi
        $CP $i $TARGETDIR/$FILE
    fi
  done
fi
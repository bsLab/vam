#!/bin/sh


BYTECCCOMPOPTS=""
BYTECCLINKOPTS=""
BYTECCLIBS=""
NATIVECC="echo !!! NOT SUPPORTED BECAUSE NOT NEEDED !!!!"
NATIVECCCOMPOPTS=""
NATIVECCLINKOPTS=""


while [ $# -gt 0 ]
do
    case $1 in
        SED)               SED=$2; shift ;;
        CHMOD)             CHMOD=$2; shift;;
        OCAML_LIBDIR)      LIBDIR=$2; shift;;
        OCAML_BINDIR)      BINDIR=$2; shift;;
        BOOT_CC)           BYTECC=$2; shift;;
        RANLIB)            RANLIBCMD=$2; shift;;
        ARCH)              ARCH=$2; shift;;
        MODEL)             MODEL=$2; shift;;
        SYSTEM)            SYSTEM=$2; shift;;
        SYSTEM_ID)         SYSTEM_ID=$2; shift;;
        SRC)               SRC=$2; shift;;
        BOOT_CCSTD_FLAGS:)
                    shift
                    while [ "$1" != ":BOOT_CCSTD_FLAGS" ]
                    do
                        BYTECCCOMPOPTS="$BYTECCCOMPOPTS $1"
                        shift
                    done 
                    ;;
        BOOT_LD_FLAGS:)
                    shift
                    while [ "$1" != ":BOOT_LD_FLAGS" ]
                    do
                        BYTECCLINKOPTS="$BYTECCLINKOPTS $1"
                        shift
                    done 
                    ;;
        BOOT_CC_LIBS:)
                    shift
                    while [ "$1" != ":BOOT_CC_LIBS" ]
                    do
                        BYTECCLIBS="$BYTECCLIBS $1"
                        shift
                    done 
                    ;;
        *)
            echo "Unknown argument"
            ;;
    esac
    shift

done





        $SED -e "s|%%LIBDIR%%|$LIBDIR|"                           \
            -e "s|%%BYTERUN%%|$BINDIR/ocamlrun|"                    \
            -e "s|%%BYTECC%%|$BYTECC $BYTECCCOMPOPTS|"              \
            -e "s|%%BYTELINK%%|$BYTECC $BYTECCLINKOPTS|"            \
            -e "s|%%NATIVECC%%|$NATIVECC $NATIVECCCOMPOPTS|"        \
            -e "s|%%NATIVELINK%%|$NATIVECC $NATIVECCLINKOPTS|"      \
            -e "s|%%PARTIALLD%%|ld -r $NATIVECCLINKOPTS|"           \
            -e "s|%%BYTECCLIBS%%|$BYTECCLIBS|"                      \
            -e "s|%%NATIVECCLIBS%%|$NATIVECCLIBS|"                  \
            -e "s|%%RANLIBCMD%%|$RANLIBCMD|"                        \
            -e "s|%%ARCH%%|$ARCH|"                                  \
            -e "s|%%MODEL%%|$MODEL|"                                \
            -e "s|%%SYSTEM%%|$SYSTEM|"                              \
            -e "s|%%SYSTEM_ID%%|$SYSTEM_ID|"                        \
            -e "s|%%EXT_OBJ%%|.o|"                                  \
            -e "s|%%EXT_ASM%%|.s|"                                  \
            -e "s|%%EXT_LIB%%|.a|"                                  \
            -e "s|%%EXT_DLL%%|.so|"                                  \
            $SRC > config.ml

#        $CHMOD -w config.ml > /dev/null

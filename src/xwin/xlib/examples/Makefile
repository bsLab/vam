include ../../../config/Makefile.config

INC_PACKAGES= X11 str 
CROSS_INC_PACKAGES= -thread

EXAMPLES=pacman xprop xpmroot chess xpm2ml sqmove movePointer sendKey xbanner

byte:: byte-examples cross-byte-examples
opt:: native-examples cross-native-examples

include ../../../Makefile.generic
-include .depend


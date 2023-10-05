
include ../../Makefile.sys
include ../../Makefile.com

MLCMI = 	\
xfifo.cmi	\
xequeue.cmi	\
xtypes.cmi	\
concur.cmi	\
conv_event.cmi	\
jeton.cmi	\
keyBind.cmi	\
display.cmi	\
eloop.cmi	\
icccm.cmi	\
ll_trans.cmi	\
selection.cmi	\
shape.cmi	\
shbuf.cmi	\
wintbl.cmi	\
x.cmi		\
xA.cmi		\
xC.cmi		\
xGraphics.cmi	\
xasync.cmi	\
xauth.cmi	\
xbuffer.cmi	\
xdebug.cmi	\
xlib.cmi	\
xmu.cmi		\
xpm.cmi		\
xproto.cmi	\
xrm.cmi		\
xsync.cmi	\
xterm.cmi	\
xtext.cmi	\
zpixmap.cmi	



MLMLI = 	\
concur.mli	\
conv_event.mli	\
display.mli	\
eloop.mli	\
icccm.mli	\
jeton.mli	\
ll_trans.mli	\
selection.mli	\
shape.mli	\
shbuf.mli	\
wintbl.mli	\
x.mli		\
xA.mli		\
xC.mli		\
keyBind.mli	\
xGraphics.mli	\
xasync.mli	\
xauth.mli	\
xbuffer.mli	\
xdebug.mli	\
xequeue.mli	\
xfifo.mli	\
xlib.mli	\
xmu.mli		\
xpm.mli		\
xproto.mli	\
xrm.mli		\
xsync.mli	\
xterm.mli	\
xtext.mli	\
xtypes.mli	\
zpixmap.mli	



MLSRC = 	\
concur.ml	\
conv_event.ml	\
display.ml	\
eloop.ml	\
icccm.ml	\
jeton.ml	\
keyBind.ml	\
ll_trans.ml	\
selection.ml	\
shape.ml	\
shbuf.ml	\
wintbl.ml	\
x.ml		\
xK.ml		\
xA.ml		\
xC.ml		\
xGraphics.ml	\
xasync.ml	\
xauth.ml	\
xbuffer.ml	\
xdebug.ml	\
xequeue.ml	\
xfifo.ml	\
xlib.ml		\
xmu.ml		\
xproto.ml	\
xsync.ml	\
xterm.ml	\
xtext.ml	\
xtypes.ml	\
zpixmap.ml	\
locale.ml
	
MLOBJ = 	\
concur.cmo	\
xdebug.cmo 	\
xA.cmo 		\
xfifo.cmo 	\
xequeue.cmo 	\
jeton.cmo 	\
xtypes.cmo 	\
wintbl.cmo 	\
xbuffer.cmo 	\
xauth.cmo 	\
ll_trans.cmo 	\
conv_event.cmo 	\
shbuf.cmo	\
display.cmo 	\
xproto.cmo 	\
x.cmo 		\
xsync.cmo 	\
xasync.cmo	\
xC.cmo		\
xK.cmo 		\
keyBind.cmo 	\
xlib.cmo 	\
xtext.cmo 	\
icccm.cmo 	\
zpixmap.cmo  	\
shape.cmo 	\
xmu.cmo 	\
eloop.cmo 	\
selection.cmo 	\
xterm.cmo 	\
xpm.cmo 	\
xGraphics.cmo 	\
xrm.cmo		\
locale.cmo

#COBJ = plot.o
#CSRC = plot.c

TMPOBJ = xK.ml xK.cmo

MLINC = -I ./ -I ../../interface 

.SUFFIXES: .ml .mli .cmo .cmi 

.mli.cmi:
	$(MLC) -c $(MLINC)  $<	

.ml.cmo:
	$(MLC) -c $(MLINC)  $<	


#
# The targets
#
xpm.cmo: xpm.mll
	ocamllex xpm.mll
	$(MLC) -c $(MLINC) xpm.ml

xrm.cmo: xrm.mll
	ocamllex xrm.mll
	$(MLC) -c $(MLINC) xrm.ml


xwin.cma: $(TMPOBJ) $(MLMLI) $(MLCMI) $(MLOBJ) $(MLSRC) 
	$(MLAR) -a $(MLINC) $(MLOBJ) -o xwin.cma

xK.ml: xK.ml.cpp 
	$(CP) xK.ml.cpp xK.ml
	echo >> xK.ml
	echo "let name_to_keysym = [" >> xK.ml
	$(CAT) xK.ml.cpp | $(SED) -n -e 's/let.*xk_\([a-zA-Z_0-9]*\).*=.*0x\([0-9A-Fa-f]*\).*$$/"\1",0x\2;/p' >> xK.ml
	echo "]" >> xK.ml
	echo "let keysym_to_name = [" >> xK.ml
	$(CAT) xK.ml.cpp | $(SED) -n -e 's/let.*xk_\([a-zA-Z_0-9]*\).*=.*0x\([0-9A-Fa-f]*\).*$$/0x\2,"\1";/p' >> xK.ml
	echo "]" >> xK.ml

cmicp: xwin.cma 
	$(CP) *.cmi ../../interface

all: $(TMPOBJ) xwin.cma cmicp

#
# Generic cleanup
#
clean: 
	$(RM) $(TMPOBJ) *.cmo *.cmi *.cma *.o  

include Makefile.depend

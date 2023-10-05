include ../../Makefile.sys
include ../../Makefile.com






RESULT=common
LIBINSTALL_SUBDIR=xlib

MLMLI =		\
parsers.mli	\
lexers.mli	\
utils.mli

MLCMI =		\
parsers.cmi	\
lexers.cmi	\
utils.cmi


MLSRC = 	\
str2.ml 	\
time.ml 	\
utils.ml

MLOBJ =		\
parsers.cmo	\
lexers.cmo	\
str2.cmo	\
time.cmo	\
utils.cmo


MLINC = -I ./ -I ../../interface -I ../xlib -I ../../ocamlsrc

.SUFFIXES: .ml .mli .cmo .cmi 

.mli.cmi:
	$(MLC) -c $(MLINC)  $<	

.ml.cmo:
	$(MLC) -c $(MLINC)  $<	

.mly.ml :
	ocamlyacc $<

parsers.cmo: parsers.mly
	ocamlyacc parsers.mly
	$(MLC) -c $(MLINC) parsers.ml

lexers.cmo: lexers.mll
	ocamllex lexers.mll
	$(MLC) -c $(MLINC) lexers.ml

		
#
# The targets
#


wx_common.cma: $(MLMLI) $(MLCMI) $(MLOBJ) $(MLSRC)
	$(MLAR) -a $(MLINC) $(MLOBJ) -o wx_common.cma



all: wx_common.cma 

#
# Generic cleanup
#
clean: 
	$(RM) $(TMPOBJ) *.cmo *.cmi *.cma *.o  


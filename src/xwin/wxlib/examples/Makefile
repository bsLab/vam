include ../../../config/Makefile.config

INC_PACKAGES= wxlib str 
CROSS_INC_PACKAGES= -thread

EXAMPLES:=demo_test demo_cursors demo_text demo_file demo_appli demo_panel demo_filesel demo_graphics demo_calc demo_table demo_tree demo_notebook demo_tex demo_bar demo_viewport demo_hello demo_hello2 demo_complex demo_button

ifdef LINUX_MIXER
  EXAMPLES := $(EXAMPLES) demo_treesel
endif


byte:: byte-examples cross-byte-examples
opt:: native-examples cross-native-examples

include ../../../Makefile.generic
-include .depend


demo_treesel: demo_treesel.cmo demo_mixer.o
	$(OCAMLCL) -custom $(INCLUDES) -o $* $(BYTE_ALL_X) $(BYTE_LINK_STR) $(BYTE_LINK_COM) WXlib.cma $*.cmo demo_mixer.o

demo_treesel.exe: demo_treesel.cmx demo_mixer.o
	$(OCAMLOPT) $(INCLUDES) -o $*.exe $(OPT_ALL_X) $(OPT_LINK_STR) $(OPT_LINK_COM) WXlib.cmxa $*.cmx demo_mixer.o


include ../../Makefile.sys
include ../../Makefile.com

MLCMI = 	\
wX_types.cmi 	\
wX_object.cmi 	\
wX_root.cmi 	\
wX_base.cmi 	\
wX_top.cmi 	\
wX_wmtop.cmi 	\
wX_label.cmi 	\
wX_pixmap.cmi 	\
wX_deleg.cmi 	\
wX_Graphics.cmi \
wX_adjust.cmi 	\
wX_popup.cmi 	\
wX_button.cmi 	\
wX_appli.cmi 	\
wX_bar.cmi 	\
wX_text.cmo	\
wX_dialog.cmi 	\
wX_display.cmi 	\
wX_dummy.cmi 	\
wX_filesel.cmi 	\
wX_ledit.cmi 	\
wX_swap.cmi 	\
wX_notebook.cmi \
wX_panel.cmi 	\
wX_port.cmi 	\
wX_radiobutton.cmi \
wX_scale.cmi 	\
wX_screen.cmi 	\
wX_scrollbar.cmi \
wX_selector.cmi \
wX_table.cmi 	\
wX_tree.cmi 	\
wX_viewport.cmi \
wX_xterm.cmi 

MLMLI = 	\
wX_Graphics.mli \
wX_adjust.mli 	\
wX_appli.mli 	\
wX_bar.mli 	\
wX_base.mli 	\
wX_button.mli 	\
wX_deleg.mli 	\
wX_dialog.mli 	\
wX_display.mli 	\
wX_dummy.mli 	\
wX_filesel.mli 	\
wX_label.mli 	\
wX_ledit.mli 	\
wX_notebook.mli \
wX_object.mli 	\
wX_panel.mli 	\
wX_pixmap.mli 	\
wX_popup.mli 	\
wX_port.mli 	\
wX_radiobutton.mli \
wX_root.mli 	\
wX_scale.mli 	\
wX_screen.mli 	\
wX_scrollbar.mli \
wX_selector.mli \
wX_swap.mli 	\
wX_table.mli 	\
wX_top.mli 	\
wX_tree.mli 	\
wX_types.mli 	\
wX_viewport.mli \
wX_wmtop.mli 	\
wX_xterm.mli 

MLSRC = 	\
wX_Graphics.ml 	\
wX_adjust.ml 	\
wX_appli.ml 	\
wX_bar.ml 	\
wX_base.ml 	\
wX_button.ml 	\
wX_deleg.ml 	\
wX_dialog.ml 	\
wX_display.ml 	\
wX_dummy.ml 	\
wX_filesel.ml 	\
wX_label.ml 	\
wX_ledit.ml 	\
wX_notebook.ml 	\
wX_object.ml 	\
wX_panel.ml 	\
wX_parent.ml 	\
wX_pixmap.ml 	\
wX_popup.ml 	\
wX_port.ml 	\
wX_radiobutton.ml \
wX_root.ml 	\
wX_scale.ml 	\
wX_screen.ml 	\
wX_scrollbar.ml \
wX_selector.ml 	\
wX_swap.ml 	\
wX_table.ml 	\
wX_text.ml 	\
wX_top.ml 	\
wX_tree.ml 	\
wX_types.ml 	\
wX_viewport.ml 	\
wX_wmtop.ml 	\
wX_xterm.ml

MLOBJ = 	\
wX_types.cmo 	\
wX_adjust.cmo 	\
wX_display.cmo 	\
wX_screen.cmo 	\
wX_base.cmo 	\
wX_object.cmo 	\
wX_root.cmo 	\
wX_top.cmo 	\
wX_dummy.cmo 	\
wX_bar.cmo 	\
wX_table.cmo 	\
wX_scrollbar.cmo \
wX_label.cmo 	\
wX_pixmap.cmo 	\
wX_viewport.cmo \
wX_text.cmo 	\
wX_button.cmo 	\
wX_popup.cmo 	\
wX_selector.cmo \
wX_deleg.cmo 	\
wX_port.cmo 	\
wX_wmtop.cmo  	\
wX_appli.cmo 	\
wX_dialog.cmo 	\
wX_dummy.cmo 	\
wX_panel.cmo 	\
wX_scale.cmo 	\
wX_ledit.cmo 	\
wX_radiobutton.cmo 	\
wX_Graphics.cmo \
wX_xterm.cmo 	\
wX_filesel.cmo 	\
wX_tree.cmo 	\
wX_swap.cmo 	\
wX_notebook.cmo \
wX_parent.cmo
	

#COBJ = plot.o
#CSRC = plot.c

#TMPOBJ = 

MLINC = -I ./ -I ../../interface -I ../xlib -I ../../ocamlsrc/ -I ../common

.SUFFIXES: .ml .mli .cmo .cmi 

.mli.cmi:
	$(MLC) -c $(MLINC)  $<	

.ml.cmo:
	$(MLC) -c $(MLINC)  $<	


#
# The targets
#


wxlib.cma: $(TMPOBJ) $(MLMLI) $(MLCMI) $(MLOBJ) $(MLSRC) 
	$(MLAR) -a $(MLINC) $(MLOBJ) -o wxlib.cma


cmicp: wxlib.cma 
	$(CP) *.cmi ../../interface

all: $(TMPOBJ) wxlib.cma cmicp

#
# Generic cleanup
#
clean: 
	$(RM) $(TMPOBJ) *.cmo *.cmi *.cma *.o  



ifeq ($(FEATURE_VISUAL), 1)
PROGS+=codemap 
endif

ifeq ($(FEATURE_VISUAL),1)
GTKINCLUDE=external/lablgtk2
CAIROINCLUDE=external/cairo2 external/cairo2-gtk
GUIDIRS=commons_wrappers/gui

VISUALDIRS=code_map
endif

    h_program-visual/lib.cma \

    h_version-control/lib.cma \

  h_program-visual \

  h_version-control \

  $(VISUALDIRS) \

  $(GUIDIRS) \

INCLUDEDIRS=commons_wrappers/gui
 $(GTKINCLUDE) $(CAIROINCLUDE) \


#------------------------------------------------------------------------------
# codemap target (was pfff_visual)
#------------------------------------------------------------------------------
SYSLIBS_CM= \
 external/lablgtk2/lablgtk.cma \
 external/cairo2/cairo.cma \
 external/cairo2-gtk/cairo_gtk.cma
OBJS_CM=code_map/lib.cma

GTKLOOP=gtkThread.cmo

codemap: $(LIBS) commons_wrappers/gui/lib.cma $(OBJS_CM) $(OBJS) main_codemap.cmo
	$(OCAMLC) -thread $(CUSTOM) -o $@ $(SYSLIBS) threads.cma \
            $(SYSLIBS_CM) $(GTKLOOP) $^

codemap.opt: $(LIBS:.cma=.cmxa) commons_wrappers/gui/lib.cmxa $(OBJS_CM:.cma=.cmxa) $(OPTOBJS) main_codemap.cmx
	$(OCAMLOPT) -thread $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) threads.cmxa\
          $(SYSLIBS_CM:.cma=.cmxa) $(GTKLOOP:.cmo=.cmx)  $^



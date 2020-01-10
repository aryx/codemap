SRC=cairo_helpers.ml async.ml \
    flag_visual.ml style2.ml help.ml \
    parsing2.ml \
    editor_connection.ml \
    model_database_code.ml model_graph_code.ml \
    model2.ml controller2.ml \
    completion2.ml \
    draw_labels.ml draw_legend.ml \
    draw_macrolevel.ml draw_microlevel.ml  \
    view_mainmap.ml view_minimap.ml view_overlays.ml \
    ui_search.ml ui_navigation.ml ui_layers.ml \
    view2.ml

##############################################################################
# Literate Programming rules
##############################################################################

TEXMAIN=Visual.tex.nw
TEX=Visual.tex

# set of noweb (.nw) files

# must be in the same order of the #include for syncweb multi files support
# to work
SRC_ORIG=Visual.tex.nw

# set of ml files, the views. Also used by make lpclean.
SRC_NOT_SYNCED=custom_list_generic.ml
SRC_ML= $(filter-out $(SRC_NOT_SYNCED), $(SRC))
SRC_NO_MLI=flag_visual.mli
SRC_MLI=$(filter-out $(SRC_NO_MLI), $(SRC_ML:ml=mli))

SRC_VIEWS=$(SRC_MLI) $(SRC_ML) ../main_codemap.ml

#------------------------------------------------------------------------------
sync:
	for i in $(SRC_VIEWS); do echo $$i; $(SYNCWEB) $(SRC_ORIG) $$i || exit 1; done 


pdf:
	$(NOWEB) $(TEXMAIN) > $(TEX)
	pdflatex $(TEX)
#	pdflatex $(TEX)

lpclean::
	rm -f $(TEX)

lpdistclean::
	rm -f $(SRC_VIEWS) .md5sum_* $(TEX)

clean::
	rm -f *.aux *.toc *.log *.brf *.out

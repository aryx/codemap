SYNCFLAGS=-md5sum_in_auxfile -less_marks -lang ocaml -verbose

VERSION=4
DOC=Visual

#TODO? add a Visua_extra.nw?
SRC_ORIG=Visual.nw

TOP=../..
LIB=$(TOP)/src/gui/

SRC_VIEWS_WITH_MLI=$(LIB)/cairo_helpers.ml $(LIB)/async.ml \
    $(LIB)/style2.ml $(LIB)/help.ml \
    $(LIB)/parsing2.ml \
    $(LIB)/editor_connection.ml \
    $(LIB)/model_database_code.ml $(LIB)/model_graph_code.ml \
    $(LIB)/model2.ml $(LIB)/controller2.ml \
    $(LIB)/draw_labels.ml $(LIB)/draw_legend.ml \
    $(LIB)/draw_macrolevel.ml $(LIB)/draw_microlevel.ml  \
    $(LIB)/view_mainmap.ml $(LIB)/view_minimap.ml $(LIB)/view_overlays.ml \
    $(LIB)/ui_search.ml $(LIB)/ui_navigation.ml $(LIB)/ui_layers.ml \
    $(LIB)/view2.ml \

SRC_VIEWS_JUST_ML= \
   $(LIB)/flag_visual.ml \
   $(LIB)/completion2.ml \
   $(TOP)/src/main/Main.ml

SRC_VIEWS=$(SRC_VIEWS_WITH_MLI) $(SRC_VIEWS_WITH_MLI:ml=mli) $(SRC_VIEWS_JUST_ML)

# set of ml files, the views. Also used by make lpclean.
#SRC_NOT_SYNCED=custom_list_generic.ml
#SRC_ML= $(filter-out $(SRC_NOT_SYNCED), $(SRC))
#SRC_NO_MLI=flag_visual.mli
#SRC_MLI=$(filter-out $(SRC_NO_MLI), $(SRC_ML:ml=mli))
#SRC_VIEWS=$(SRC_MLI) $(SRC_ML) ../main_codemap.ml


include ~/github/semgrep-internals/Makefile.common

#INDEXER=~/github/syncweb/indexer/_build/default/main_indexer.exe
#index:
#	$(INDEXER) -lang cmt $(SRC_INDEX)  \
#    | sed -e 's#Main.cmt#Main#' \
#    | sed -e 's#semgrep-core/_build/default/##' \
#    | sed -e 's#semgrep/semgrep-core/_build/default/bin/.Main.eobjs/byte/#semgrep/bin/#' \
#    | grep -v 'Common.=.*' \
#    | grep -v 'Common2' \
#    | grep -v 'OUnit.>' \
#    | grep -v 'fuzzy' \
#    | grep -v 'simple_refactoring' \
#    | grep -v 'main_spatch' \
#    > defs_and_uses.list


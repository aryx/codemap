SYNCFLAGS=-md5sum_in_auxfile -less_marks -lang ocaml

VERSION=2
DOC=Visual

SRC_ORIG=Visual.nw

SRC_VIEWS_WITH_MLI=cairo_helpers.ml async.ml \
    style2.ml help.ml \
    parsing2.ml \
    editor_connection.ml \
    model_database_code.ml model_graph_code.ml \
    model2.ml controller2.ml \
    draw_labels.ml draw_legend.ml \
    draw_macrolevel.ml draw_microlevel.ml  \
    view_mainmap.ml view_minimap.ml view_overlays.ml \
    ui_search.ml ui_navigation.ml ui_layers.ml \
    view2.ml \

SRC_VIEWS_JUST_ML= \
   flag_visual.ml \
   completion2.ml \
   ../bin/main_codemap.ml

SRC_VIEWS=$(SRC_VIEWS_WITH_MLI) $(SRC_VIEWS_WITH_MLI:ml=mli) $(SRC_VIEWS_JUST_ML)

# set of ml files, the views. Also used by make lpclean.
#SRC_NOT_SYNCED=custom_list_generic.ml
#SRC_ML= $(filter-out $(SRC_NOT_SYNCED), $(SRC))
#SRC_NO_MLI=flag_visual.mli
#SRC_MLI=$(filter-out $(SRC_NO_MLI), $(SRC_ML:ml=mli))
#SRC_VIEWS=$(SRC_MLI) $(SRC_ML) ../main_codemap.ml


include ~/semgrep-internals/Makefile.common


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


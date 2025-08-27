#TODO: switch to mkfile and reuse principia/docs/latest/mkcommon
# instead of ~/github/semgrep-internals/Makefile.common
# or move semgrep-internals/Makefile.common in principia/docs/latest

# -verbose
SYNCFLAGS=-md5sum_in_auxfile -less_marks -lang ocaml

VERSION=4
DOC=Visual

#TODO? add a Visua_extra.nw?
SRC_ORIG=Visual.nw

TOP=../..
LIB=$(TOP)/src/gui/

SRC_VIEWS_WITH_MLI=$(LIB)/cairo_helpers.ml $(LIB)/async.ml \
    $(LIB)/Style.ml $(LIB)/help.ml \
    $(LIB)/Parsing.ml \
    $(LIB)/Completion.ml \
    $(LIB)/editor_connection.ml \
    $(LIB)/model_database_code.ml $(LIB)/model_graph_code.ml \
    $(LIB)/Model.ml $(LIB)/Controller.ml \
    $(LIB)/draw_labels.ml $(LIB)/draw_legend.ml \
    $(LIB)/draw_macrolevel.ml $(LIB)/draw_microlevel.ml  \
    $(LIB)/view_mainmap.ml $(LIB)/view_minimap.ml $(LIB)/view_overlays.ml \
    $(LIB)/ui_search.ml $(LIB)/ui_navigation.ml $(LIB)/ui_layers.ml \
    $(LIB)/View.ml \

SRC_VIEWS_JUST_ML= \
   $(LIB)/flag_visual.ml \
   $(TOP)/src/main/Main.ml

SRC_VIEWS=$(SRC_VIEWS_WITH_MLI) $(SRC_VIEWS_WITH_MLI:ml=mli) $(SRC_VIEWS_JUST_ML)

# set of ml files, the views. Also used by make lpclean.
#SRC_NOT_SYNCED=custom_list_generic.ml
#SRC_ML= $(filter-out $(SRC_NOT_SYNCED), $(SRC))
#SRC_NO_MLI=flag_visual.mli
#SRC_MLI=$(filter-out $(SRC_NO_MLI), $(SRC_ML:ml=mli))
#SRC_VIEWS=$(SRC_MLI) $(SRC_ML) ../main_codemap.ml

#TODO: use in principia instead?
include ~/github/semgrep-internals/Makefile.common

# note: the grep _build/default is for files like Lexer_nw.ml which are
# just under _build/ and not in src/ but we still don't want them
#TODO: remove the second grep and lpize those files instead
index:
	$(INDEXER) -lang cmt $(TOP) \
    | grep -v 'TODO_NO_SOURCE_FOUND' \
    | grep -v '_build/default' \
    | grep -v 'Log_' \
    | grep -v 'version_control/' \
    > defs_and_uses.list


#    | grep -E -v 'flag_.*visual|ast_nw.ml|Main.ml|Log_.*.ml|archi_code_lexer.ml|main_vcs.ml|Gui.ml|Lexer_nw.ml|ast_html.ml' \

xxxx: echo \
    | sed -e 's#Main.cmt#Main#' \
    | sed -e 's#semgrep-core/_build/default/##' \
    | sed -e 's#semgrep/semgrep-core/_build/default/bin/.Main.eobjs/byte/#semgrep/bin/#' \
    | grep -v 'Common.=.*' \
    | grep -v 'Common2' \
    | grep -v 'OUnit.>' \
    | grep -v 'fuzzy' \
    | grep -v 'simple_refactoring' \
    | grep -v 'main_spatch' \


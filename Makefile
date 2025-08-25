###############################################################################
# Prelude
###############################################################################

###############################################################################
# Main targets
###############################################################################

# TODO: add {,test} at some point
default:
	dune build _build/install/default/bin/codemap
all:
	dune build
clean:
	dune clean

test:
	dune runtest
install:
	dune install

codemap.opam: dune-project
	dune build $@

build-docker:
	docker build -t "codemap" .

.PHONY: all clean install test dump

###############################################################################
# Developer targets
###############################################################################

# -filter semgrep
visual:
	./bin/codemap -screen_size 3 -efuns_client efuns_client -emacs_client /dev/null .

sync:
	@echo go to docs/literate/

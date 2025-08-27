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

.PHONY: all clean install test


codemap.opam: dune-project
	dune build $@

build-docker:
	docker build -t "codemap" .
build-docker-ocaml5:
	docker build -t "codemap" --build-arg OCAML_VERSION=5.2.1 .

###############################################################################
# Developer targets
###############################################################################

# -filter semgrep
visual:
	./bin/codemap -screen_size 3 -efuns_client efuns_client -emacs_client /dev/null .
sync:
	@echo go to docs/literate/

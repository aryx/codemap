
# TODO: add {,test} at some point
default:
	dune build _build/install/default/bin/codemap
all:
	dune build
clean:
	dune clean
	rm -f *.opam

test:
	dune runtest
install:
	dune install

codemap.opam: dune-project
	dune build $@

# -filter semgrep
visual:
	./bin/codemap -screen_size 3 -efuns_client efuns_client -emacs_client /dev/null .

.PHONY: all clean install test dump

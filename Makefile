all:
	dune build
#	dune build ./_build/default/tests/test.bc
clean:
	dune clean
test:
	dune runtest
install:
	dune install

# -filter semgrep
visual:
	./bin/codemap -screen_size 3 -efuns_client efuns_client -emacs_client /dev/null .

.PHONY: all clean install test dump

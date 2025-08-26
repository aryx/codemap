# Build codemap (and many semgrep libs) with OCaml 4.14.2 via OPAM on Ubuntu Linux.

FROM ubuntu:22.04
# alt: 24.04

# Setup a basic C dev environment
RUN apt-get update # needed otherwise can't find any package
RUN apt-get install -y build-essential autoconf automake pkgconf git wget curl

# Setup OPAM and OCaml
RUN apt-get install -y opam
# Initialize opam (disable sandboxing due to Docker)
RUN opam init --disable-sandboxing -y
RUN opam switch create 4.14.2 -v


# Install semgrep libs (and its many dependencies)
WORKDIR /semgrep
# alt: add semgrep as a submodule in codemap source or LATER add semgrep as opam packages
RUN git clone --depth=1 --recurse-submodules https://github.com/aryx/semgrep-libs /semgrep
#coupling: https://github.com/aryx/semgrep-libs/blob/master/Dockerfile
# external dependencies of semgrep-libs itself
# alt: make install-deps-UBUNTU-for-semgrep-core
RUN apt-get install -y pkg-config libpcre3-dev libpcre2-dev libgmp-dev libev-dev libcurl4-gnutls-dev
RUN ./configure
RUN eval $(opam env) && make && make dune-build-all
# subset of make install-semgrep-libs (dune build; dune install)
# LATER: move in codemap.opam (and dune-project) at some point
# alt: RUN eval $(opam env) && make install-semgrep-libs
RUN eval $(opam env) && dune install \
   TCB commons commons2 profiling tracing process_limits parallelism pcre2 \
   testo testo-util testo-diff \
   gitignore paths glob git_wrapper \
   lib_parsing ast_generic tree-sitter lib_parsing_tree_sitter tree-sitter-lang \
   parser_ocaml parser_lisp parser_scala \
   parser_cpp parser_java \
   parser_python parser_javascript parser_ruby parser_php \
   parser_go parser_rust \
   parser_yaml parser_jsonnet parser_dockerfile parser_bash \
   pfff-lang_GENERIC-naming \
   semgrep aliengrep spacegrep
#TODO: remove semgrep aliengrep ... but need split semgrep.typing and semgrep.core
# and its Lang
#TODO: can't rm -rf because then can't find -ltree-sitter :(
# RUN rm -rf /semgrep

# Add external libs
# alt: use opam-depext
RUN apt-get install -y libcairo2-dev libgtk2.0-dev

# Back to codemap
WORKDIR /src

# Install other dependencies
COPY codemap.opam configure ./
RUN ./configure

# Now let's build from source
COPY . .

RUN eval $(opam env) && make
RUN eval $(opam env) && make all
RUN eval $(opam env) && make install

# Test
RUN eval $(opam env) && codemap --help
# TODO make test

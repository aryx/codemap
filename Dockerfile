# Build codemap with OCaml 4.14.2 via OPAM on Ubuntu.
# semgrep-libs and semgrep-pfff-langs are vendored as git submodules.

FROM ubuntu:22.04
# alt: 24.04

# Setup a basic C dev environment
RUN apt-get update # needed otherwise can't find any package
RUN apt-get install -y build-essential autoconf automake pkgconf git wget curl

# Setup OPAM and OCaml
RUN apt-get install -y opam
RUN opam init --disable-sandboxing -y # (disable sandboxing due to Docker)
# can adjust with docker --build-arg OCAML_VERSION=5.2.1 ...
ARG OCAML_VERSION=4.14.2
RUN opam switch create ${OCAML_VERSION} -v

# Add external deps of codemap and its submodules
# coupling: semgrep-libs needs pcre, gmp, ev, curl
RUN apt-get install -y pkg-config libpcre3-dev libpcre2-dev libgmp-dev libev-dev libcurl4-gnutls-dev
# codemap GUI needs cairo and gtk2
RUN apt-get install -y libcairo2-dev libgtk2.0-dev

WORKDIR /src

# Install dependencies (copy minimal files for layer caching)
COPY codemap.opam configure Makefile ./
# Copy enough submodule content for configure/setup to work
COPY semgrep-libs/TCB/ ./semgrep-libs/TCB/
COPY semgrep-pfff-langs/scripts/setup-tree-sitter.sh ./semgrep-pfff-langs/scripts/
COPY semgrep-pfff-langs/libs/ocaml-tree-sitter-core/ ./semgrep-pfff-langs/libs/ocaml-tree-sitter-core/
RUN ./configure

# Now copy the full source and build
COPY . .
RUN eval $(opam env) && make
RUN eval $(opam env) && make all
RUN eval $(opam env) && make install

# Test
RUN eval $(opam env) && codemap --help
# TODO make test

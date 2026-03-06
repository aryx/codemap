# Build codemap with OCaml 4.14.2 via OPAM on Ubuntu.
# semgrep-pfff-libs and semgrep-pfff-langs are vendored as git submodules.

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
# coupling: semgrep-pfff-libs needs pcre, gmp, ev, curl
RUN apt-get install -y pkg-config libpcre3-dev libpcre2-dev libgmp-dev libev-dev libcurl4-gnutls-dev
# codemap GUI needs cairo and gtk2
RUN apt-get install -y libcairo2-dev libgtk2.0-dev

WORKDIR /src

# Install dependencies (copy minimal files for Docker layer caching)
COPY codemap.opam configure ./
# Copy enough submodule content for configure to work
COPY semgrep-pfff-libs/TCB/ ./semgrep-pfff-libs/TCB/
COPY semgrep-pfff-langs/scripts/setup-tree-sitter.sh ./semgrep-pfff-langs/scripts/
COPY semgrep-pfff-langs/parsing_infra/ocaml-tree-sitter-core/ ./semgrep-pfff-langs/parsing_infra/ocaml-tree-sitter-core/
RUN ./configure

# Now copy the full source and build
COPY . .
RUN eval $(opam env) && make
# Note: 'make all' (dune build) is not used here because it would try to build
# all libraries in vendored submodules, including ones that depend on osemgrep
# or JS packages (cohttp-lwt, js_of_ocaml-lwt, semgrep.core) not needed by codemap.
RUN eval $(opam env) && make install

# Test
RUN eval $(opam env) && codemap --help
# TODO make test

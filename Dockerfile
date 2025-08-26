# Build codemap (and semgrep libs) with OCaml 4.14.2 via OPAM on Ubuntu Linux.

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

# Add external libs
# alt: use opam-depext
RUN apt-get install -y libcairo2-dev libgtk2.0-dev


# Install semgrep libs (and its many dependencies)
WORKDIR /semgrep
# alt: add semgrep as a submodule in codemap source or LATER add semgrep as opam packages
RUN git clone --recurse-submodules https://github.com/aryx/semgrep-libs /semgrep
#coupling: https://github.com/aryx/semgrep-libs/blob/master/Dockerfile
# external dependencies of semgrep-libs itself
# alt: make install-deps-UBUNTU-for-semgrep-core
RUN apt-get install -y pkg-config libpcre3-dev libpcre2-dev libgmp-dev libev-dev libcurl4-gnutls-dev
RUN ./configure
RUN eval $(opam env) && make
RUN eval $(opam env) && make install-semgrep-libs
#TODO: can't because then can't find -ltree-sitter
# RUN rm -rf /semgrep

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
# TODO

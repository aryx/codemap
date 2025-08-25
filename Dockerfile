# Build codemap (and semgrep-libs) with OCaml 4.14.2 via OPAM on Ubuntu Linux.

FROM ubuntu:22.04

# Setup a basic C dev environment
RUN apt-get update # needed otherwise can't find any package
RUN apt-get install -y build-essential autoconf automake pkgconf git wget curl

# Setup OPAM and OCaml
RUN apt-get install -y opam
# Initialize opam (disable sandboxing due to Docker)
RUN opam init --disable-sandboxing -y
RUN opam switch create 4.14.2 -v


WORKDIR /src

# Install semgrep-libs
# TODO

# Install other dependencies
COPY codemap.opam configure ./
RUN ./configure

# Now let's build from source
COPY . .

RUN eval $(opam env) && make

# Test
# TODO

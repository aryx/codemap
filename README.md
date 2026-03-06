# Codemap

Semantic source code visualizer. Renders treemap-based bird's-eye
views of codebases with code thumbnails and syntax highlighting
via a GTK2/Cairo GUI. Uses [semgrep](https://github.com/semgrep/semgrep)
parsers for multi-language semantic highlighting.

![codemap screenshot](docs/codemap-screenshot.png)

## Building

### Prerequisites

OCaml 4.14+ (via opam >= 2.1), gcc, git, curl, pkg-config.

On Ubuntu/Debian:
```bash
apt-get install build-essential pkg-config opam curl libcairo2-dev libgtk2.0-dev
```

On macOS:
```bash
brew install opam pkg-config cairo gtk+
```

C libraries (pcre, pcre2, gmp, libev, libcurl) are installed automatically
by `./configure` via opam — no need to install them manually.

### Quick start

```bash
git clone --recurse-submodules https://github.com/aryx/codemap
cd codemap
./configure     # installs opam deps and sets up tree-sitter (run infrequently)
make            # routine build
make test       # run tests
```

### Docker

A reference build using Ubuntu is provided:

```bash
docker build -t codemap .
```

To build with OCaml 5:
```bash
docker build -t codemap --build-arg OCAML_VERSION=5.2.1 .
```

## Usage

```bash
codemap ~/my-project
```

This launches a GTK-based GUI that lets you visualize source code
and perform code search.

See the [old codemap page at Facebook](https://github.com/facebookarchive/pfff/wiki/CodeMap)
for more information.

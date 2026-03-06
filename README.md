# Codemap

Semantic source code visualizer. Renders treemap-based bird's-eye
views of codebases with code thumbnails and syntax highlighting
via a GTK2/Cairo GUI. Uses [semgrep](https://github.com/semgrep/semgrep)
parsers for multi-language semantic highlighting.

![codemap screenshot](docs/codemap-screenshot.jpg)

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

## Inspiration

Codemap combines ideas from several software visualization projects:

- **[SeeSoft](https://ieeexplore.ieee.org/document/177365)** — Eick, Steffen, and Sumner at Bell Labs (1992). One of the earliest tools for visualizing line-oriented software statistics, mapping each line of code to a thin colored row. See also Ball and Eick's follow-up, [Software Visualization in the Large](https://ieeexplore.ieee.org/document/488299) (1996), which extended these ideas with multiple visual representations (line, pixel, file summary, hierarchical).
- **[Code Thumbnails](https://dl.acm.org/doi/10.1145/1168149.1168174)** — DeLine et al. at Microsoft Research (2006). Miniature code renderings as navigation aids, showing that the visual structure of code provides useful spatial cues even at tiny scales.
- **[Treemap visualization of the Linux kernel](https://hal.inria.fr/hal-00850778)** — Fekete and Plaisant. The idea of using treemaps for source code layout, which codemap combines with code thumbnails and SeeSoft-style coloring.

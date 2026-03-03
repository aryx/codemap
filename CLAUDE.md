# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Codemap is a semantic source code visualizer written in OCaml. It renders treemap-based bird's-eye views of codebases with code thumbnails and semantic syntax highlighting via a GTK2/Cairo GUI. It has deep ties to the semgrep ecosystem for multi-language parsing.

## Build Commands

```bash
make              # Build the codemap binary (dune build)
make all          # Build everything
make test         # Run tests (dune runtest)
make clean        # Clean build artifacts
make install      # Install via dune
make visual       # Launch the GUI on the current directory
make build-docker # Build Docker image (OCaml 4.14.2 default, also supports 5.2.1)
```

## Architecture

**Entry point:** `src/main/Main.ml` — CLI argument parsing and GUI launch.

**Key layers:**

- `src/gui/` — GTK-based GUI using MVC pattern (Model.ml, View.ml, Controller.ml, Parsing.ml). The `Viewer` library.
- `libs/visualization/` — Treemap layout algorithms (pivot, ordered) and Cairo rendering.
- `libs/graph_code/` — Dependency graph representation and analysis for code entities.
- `src/highlight_code/` + `src/highlighters/` — Semantic highlighting infrastructure with per-language highlighters (C++, Go, Java, JS, ML, PHP, Python, Scala, etc.).
- `src/archi_code/` — Architecture specification parser (OCamllex lexer + parser).
- `src/database_code/` — In-memory entity database, big grep indexing, completion.
- `src/layer_code/` — Visualization layers/overlays.
- `languages/` — Language-specific parsers (C via Menhir, HTML, Noweb, Text/Org).
- `libs/commons2_/` — Large shared utility library.
- `libs/gui/` — Low-level GTK/Cairo wrapper (`Gui.ml`).

**External dependencies:** Relies heavily on semgrep libraries for parsing (`parser_python.menhir`, `parser_cpp.menhir`, `parser_java.menhir`, `ast_generic`, etc.). See `Dockerfile` for the full dependency setup.

## Build System

- **Dune** with `dune-project` at lang level 2.7 (intentionally, to avoid `.pp.ml` generation issues).
- PPX preprocessors: `ppx_deriving.show`, `profiling.ppx`.
- Compiler warnings config: `-w -52-6` (some warnings suppressed).
- OPAM file (`codemap.opam`) is auto-generated from `dune-project` — run `make codemap.opam` after changes.
- CI: GitHub Actions with Docker, testing on OCaml 4.14.2 and 5.2.1.

## Conventions

- **Literate programming with syncweb:** This project uses [syncweb](https://github.com/aryx/syncweb) for literate programming. The canonical documentation is in `docs/Visual.nw` (Noweb format), and source files are synced from it.
- **DO NOT modify `(* s: ... *)`, `(* e: ... *)`, or `(* x: ... *)` comments** in source files. These are syncweb chunk markers that link source code to the Noweb documentation. Altering them will break `make sync` in `docs/`.
- File filtering uses `.codemapignore` (similar to `.gitignore`).
- The `bin/codemap` symlink points to the built binary in `_build/`.

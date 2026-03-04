(lang dune 2.7)
; 2.7 instead of 3.7 so dune does not generate confusing .pp.ml for codegraph
; and syncweb_indexer

;; This file is used both to configure 'dune' as usual, and
;; to autogenerate the codemap.opam for 'opam'.

;; classic dune-project settings
(name codemap)

;; Opam package declarations for public libraries and public executables
;; defined in various dune files.
;; See the documentation for declaring opam packages for more info:
;; https://dune.readthedocs.io/en/stable/opam.html#opam-generation
(generate_opam_files)

;; Default attributes of opam packages
(source (github aryx/codemap))
(homepage "https://github.com/aryx/codemap")
(maintainers "Yoann Padioleau")
(authors "Yoann Padioleau")

;; coupling: if you modify this package, type 'make codemap.opam'
(package (name codemap)
  (synopsis "Source code visualizer")
  (description "\
Codemap is great.

For more information see https://github.com/aryx/codemap
"
  )

  (maintainers "Yoann Padioleau <yoann.padioleau@gmail.com>")
  (authors "Yoann Padioleau <yoann.padioleau@gmail.com>")
  (license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")

  (depends
    (dune (>= 3.7))
    ; codemap's own deps
    lablgtk
    cairo2-gtk
    ounit
    uutf
    uucp
    ocamlgraph
    ; External deps of vendored submodules (semgrep-libs, semgrep-pfff-langs).
    ; These are needed because the submodules are built from source as part of
    ; the dune workspace, so their opam deps are not resolved automatically.
    ; alt: check in the generated .opam files in the submodule repos and use
    ;  opam install --deps-only on them in configure instead of listing here.
    ; coupling: semgrep-libs/dune-project, semgrep-pfff-langs/dune-project
    ; -- from semgrep-libs (mainly commons) --
    ANSITerminal
    alcotest
    alcotest-lwt
    atdgen
    bos
    (cmdliner (and (>= "1.3.0") (< "2.0.0")))
    digestif
    fmt
    fpath
    git
    logs
    lwt
    lwt_ppx
    memtrace
    ocolor
    pcre
    pcre2
    ppx_deriving
    ppx_deriving_yojson
    ppx_hash
    ppx_inline_test
    ppx_sexp_conv
    ppxlib
    re
    semver
    sexplib
    testo
    timedesc
    ; TODO: trace/opentelemetry are pulled in transitively via
    ;  profiling -> process_limits -> tracing -> tracing.unix.
    ;  We should find a way to remove these dependencies from codemap.
    trace
    (opentelemetry (< "0.11"))
    (opentelemetry-client-ocurl (< "0.11"))
    ambient-context-lwt
    uri
    uuidm
    yojson
    ; -- from semgrep-pfff-langs --
    ; coupling: menhir version must match what semgrep-pfff-langs parsers
    ;  were tested with — newer menhir can generate different conflict reports
    ;  and slightly different parser code (e.g., tuple shapes in actions).
    (menhir (= "20230608"))
    parmap
    tsort
    visitors
    yaml
  )
)

; note that many of those packages are used by codegraph

(package (name commons2_)
  (synopsis "XXX")
  (description "XXX")
)

(package (name database_code)
  (synopsis "XXX")
  (description "XXX")
)
(package (name layer_code)
  (synopsis "XXX")
  (description "XXX")
)

(package (name highlight_code)
  (synopsis "XXX")
  (description "XXX")
)
(package (name highlighters)
  (synopsis "XXX")
  (description "XXX")
)
(package (name visualization)
  (synopsis "XXX")
  (description "XXX")
)

(package (name files-format)
  (synopsis "XXX")
  (description "XXX")
)

; parser_c now comes from vendored semgrep-pfff-langs
(package (name parser_nw)
  (synopsis "XXX")
  (description "XXX")
)
(package (name codemap_parser_html)
  (synopsis "HTML parser for codemap")
  (description "Full menhir-based HTML parser used by codemap.")
)
(package (name parser_text)
  (synopsis "XXX")
  (description "XXX")
)

(package (name graph_code)
  (synopsis "XXX")
  (description "XXX")
)

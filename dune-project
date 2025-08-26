(lang dune 3.7)

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

  ; TODO: and there are tons of dependencies to semgrep not listed below
  ; see Dockerfile and its 'dune install ...' command
  (depends
    (dune (>= 2.7.0))
    lablgtk
    cairo2-gtk
    ounit
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

(package (name parser_c)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_nw)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_html)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_text)
  (synopsis "XXX")
  (description "XXX")
)

(package (name graph_code)
  (synopsis "XXX")
  (description "XXX")
)

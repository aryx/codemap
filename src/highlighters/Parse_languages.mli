type origin_info =
  (* those were extracted from the tree-sitter Concrete Syntax Tree (CST) *)
  | InCST
  (* those are all the ranges in the file that do not correspond to
   * an info in the CST (e.g., space, comments), that is the
   * tokens from the extra: field in tree-sitter grammars 
   *)
  | Extra

val parse_ocaml: 
  Fpath.t -> (AST_ocaml.program, Parser_ml.token) Parsing_result.t

val parse_rust:
  Fpath.t -> AST_generic.program * (Tok.t * origin_info) list

val parse_jsonnet:
  Fpath.t -> AST_generic.program * (Tok.t * origin_info) list

val parse_yaml:
  Fpath.t -> AST_generic.program * (Tok.t * origin_info) list

val parse_bash:
  Fpath.t -> AST_generic.program * (Tok.t * origin_info) list

val parse_dockerfile:
  Fpath.t -> AST_generic.program * (Tok.t * origin_info) list

val parse_lisp:
  Fpath.t -> AST_generic.program * (Tok.t * origin_info) list


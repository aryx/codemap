type origin_info =
  (* those were extracted from the tree-sitter Concrete Syntax Tree (CST) *)
  | InCST
  (* those are all the ranges in the file that do not correspond to
   * an info in the CST (e.g., space, comments), that is the
   * tokens from the extra: field in tree-sitter grammars 
   *)
  | Extra

val parse_ocaml: 
  string (* filename *) -> (AST_ocaml.program, Parser_ml.token) Parsing_result.t

val parse_rust:
  string (* filename *) -> AST_generic.program * (Tok.t * origin_info) list

val parse_jsonnet:
  string (* filename *) -> AST_generic.program * (Tok.t * origin_info) list

val parse_yaml:
  string (* filename *) -> AST_generic.program * (Tok.t * origin_info) list

val parse_bash:
  string (* filename *) -> AST_generic.program * (Tok.t * origin_info) list

val parse_dockerfile:
  string (* filename *) -> AST_generic.program * (Tok.t * origin_info) list

val parse_lisp:
  string (* filename *) -> AST_generic.program * (Tok.t * origin_info) list


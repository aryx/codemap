type origin_info =
  (* those were extracted from the tree-sitter Concrete Syntax Tree (CST) *)
  | InCST
  (* those are all the ranges in the file that do not correspond to
   * an info in the CST (e.g., space, comments), that is the
   * tokens from the extra: field in tree-sitter grammars 
   *)
  | Extra

val parse_ocaml: 
  Common.filename -> (Ast_ml.program, Parser_ml.token) Parsing_result.t

val parse_rust:
  Common.filename -> AST_generic.program * (Tok.t * origin_info) list

val parse_jsonnet:
  Common.filename -> AST_generic.program * (Tok.t * origin_info) list

val parse_yaml:
  Common.filename -> AST_generic.program * (Tok.t * origin_info) list

val parse_bash:
  Common.filename -> AST_generic.program * (Tok.t * origin_info) list

val parse_dockerfile:
  Common.filename -> AST_generic.program * (Tok.t * origin_info) list

val parse_lisp:
  Common.filename -> AST_generic.program * (Tok.t * origin_info) list


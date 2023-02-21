
val parse_ocaml: 
  Common.filename -> (Ast_ml.program, Parser_ml.token) Parsing_result.t

val parse_rust:
  Common.filename -> AST_generic.program * Parse_info.t list


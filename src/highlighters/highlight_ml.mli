
val visit_program:
  ?lexer_based_tagger:bool ->
  tag_hook: (Tok.t -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  (*(Database_php.id * Common.filename * Database_php.database) option -> *)
  Fpath.t ->
  AST_ocaml.program * Parser_ml.token list ->
  unit

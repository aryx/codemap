
val visit_program:
  ?lexer_based_tagger:bool ->
  tag_hook: (Tok.t -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  Fpath.t ->
  AST_scala.program * Parser_scala.token list ->
  unit

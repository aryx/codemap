
val visit_program:
  ?lexer_based_tagger:bool ->
  tag_hook:
    (Parse_info.t -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  Common.filename ->
  AST_scala.program * Parser_scala.token list ->
  unit

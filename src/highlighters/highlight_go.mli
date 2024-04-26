val visit_program :
  tag_hook:(Tok.t -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  Fpath.t ->
  Ast_go.program * Parser_go.token list ->
  unit

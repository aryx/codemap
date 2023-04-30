val visit_program :
  tag_hook:(Tok.t -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  Ast_ruby.program option * Parser_ruby.token list ->
  unit

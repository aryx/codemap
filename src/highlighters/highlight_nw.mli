val visit_program :
  tag_hook:(Tok.t -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  (*(Database_php.id * Common.filename * Database_php.database) option -> *)
  Fpath.t ->
  Ast_nw.program * Lexer_nw.token list ->
  unit

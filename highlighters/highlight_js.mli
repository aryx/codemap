
val visit_program:
  tag_hook:
    (Parse_info.t -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  (*(Database_php.id * Common.filename * Database_php.database) option -> *)
  Fpath.t ->
  Ast_js.a_program * Parser_js.token list ->
  unit

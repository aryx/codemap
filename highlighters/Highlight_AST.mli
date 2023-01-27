
val visit_program:
  (Parse_info.t, 'a) Hashtbl.t *
  (Parse_info.t -> Highlight_code.category -> unit) ->
  AST_generic.program -> unit

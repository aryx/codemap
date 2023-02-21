
(* To be called to augment a lang-specific highlighter with name resolution
 * info computed with Naming_AST.ml
 *)
val visit_program:
  (Parse_info.t, 'a) Hashtbl.t (* already_tagged hash *) *
  (Parse_info.t -> Highlight_code.category -> unit) (* tag_hook *) ->
  AST_generic.program -> unit

(* to be called when we don't have a lang-specific highlighter, just
 * a generic AST (usually coming from tree-sitter)
 *)
val visit_for_highlight: 
  tag_hook:(Parse_info.t -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  Fpath.t ->
  AST_generic.program * (Parse_info.t * Parse_languages.origin_info) list -> unit

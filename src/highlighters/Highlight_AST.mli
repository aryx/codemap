
(* To be called to augment a lang-specific highlighter with name resolution
 * info computed with Naming_AST.ml
 *)
val visit_program:
  (Tok.t, bool) Hashtbl.t (* already_tagged hash *) *
  (Tok.t -> Highlight_code.category -> unit) (* tag_hook *) ->
  AST_generic.program -> unit

(* to be called when we don't have a lang-specific highlighter, just
 * a generic AST (usually coming from tree-sitter)
 *)
val visit_for_highlight: 
  tag_hook:(Tok.t -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  Fpath.t ->
  AST_generic.program * (Tok.t * Parse_languages.origin_info) list -> unit

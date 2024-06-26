
type ('ast, 'token) t = {
  parse: (Fpath.t -> ('ast * 'token list));

  (* The highlight function is provided as a visitor. 
   * note: there is no guarantee in which order the hooks are called as
   * some pfff modes visit first the AST and then the tokens
   * in which case some hooks for early tokens might be called
   * after tokens that come later but were present in the 
   * AST and visited.
   *)
  highlight:(tag_hook:(Tok.t -> Highlight_code.category -> unit) ->
                   Highlight_code.highlighter_preferences ->
                   Fpath.t ->
                   'ast * 'token list -> unit);
  info_of_tok:('token -> Tok.t);
}

val rust: 
  (AST_generic.program, Tok.t * Parse_languages.origin_info) t

val jsonnet: 
  (AST_generic.program, Tok.t * Parse_languages.origin_info) t

val yaml: 
  (AST_generic.program, Tok.t * Parse_languages.origin_info) t

val bash: 
  (AST_generic.program, Tok.t * Parse_languages.origin_info) t

val dockerfile: 
  (AST_generic.program, Tok.t * Parse_languages.origin_info) t

val lisp: 
  (AST_generic.program, Tok.t * Parse_languages.origin_info) t

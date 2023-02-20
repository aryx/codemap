
type ('ast, 'token) t = {
  parse: (Common.filename -> ('ast * 'token list) list);

  (* The highlight function is provided as a visitor. 
   * note: there is no guarantee in which order the hooks are called as
   * some pfff modes visit first the AST and then the tokens
   * in which case some hooks for early tokens might be called
   * after tokens that come later but were present in the 
   * AST and visited.
   *)
  highlight:(tag_hook:(Parse_info.t -> Highlight_code.category -> unit) ->
                   Highlight_code.highlighter_preferences ->
                   Fpath.t ->
                   'ast * 'token list -> unit);
  info_of_tok:('token -> Parse_info.t);
}

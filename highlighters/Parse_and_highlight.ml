(* Yoann Padioleau
 *
 * Copyright (C) 2023 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
*)
module HC = Highlight_code
module PL = Parse_languages

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type ('ast, 'token) t = {
  parse: (string (* filename *) -> ('ast * 'token list));
  highlight:(tag_hook:(Tok.t -> HC.category -> unit) ->
                   Highlight_code.highlighter_preferences ->
                   Fpath.t ->
                   'ast * 'token list -> unit);
  info_of_tok:('token -> Tok.t);
}

(*****************************************************************************)
(* helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
let rust = {
  parse = Parse_languages.parse_rust;
  highlight = (fun ~tag_hook prefs file (ast, toks) -> 
        Highlight_AST.visit_for_highlight ~tag_hook prefs file (ast, toks));
  info_of_tok = (fun (x, _origin) -> x);
}

let jsonnet = {
  parse = Parse_languages.parse_jsonnet;
  highlight = (fun ~tag_hook prefs file (ast, toks) -> 
     Highlight_AST.visit_for_highlight ~tag_hook prefs file (ast, toks);
     (* small customization for tokens which are not currently in the
      * generic AST and so could not be tagged in Highlight_AST (but
      * are in the tree-sitter CST).
      *)
     toks |> List.iter (fun (info, origin) ->
       let s = Tok.content_of_tok info in
       (match s, origin with
       | "then", PL.InCST -> tag_hook info HC.KeywordConditional
       | "error", PL.InCST -> tag_hook info HC.KeywordExn
       | "local", PL.InCST -> tag_hook info HC.Keyword
       | "tailstrict", PL.InCST -> tag_hook info HC.Attribute
       | _else_ -> ()
       )
  ));
  info_of_tok = (fun (x, _origin) -> x);
}

let lisp = {
  parse = Parse_languages.parse_lisp;
  highlight = (fun ~tag_hook prefs file (ast, toks) -> 
        Highlight_AST.visit_for_highlight ~tag_hook prefs file (ast, toks));
  info_of_tok = (fun (x, _origin) -> x);
}

(* TODO: the highlighter is not great because almost everything is encoded as
 * a string in the generic AST translation. Maybe worth starting from AST_bash instead?
 *)
let bash = {
  parse = Parse_languages.parse_bash;
  highlight = (fun ~tag_hook prefs file (ast, toks) -> 
        Highlight_AST.visit_for_highlight ~tag_hook prefs file (ast, toks);
     toks |> List.iter (fun (info, origin) ->
       let s = Tok.content_of_tok info in
       (match s, origin with
       | ("then" | "fi"), PL.InCST -> tag_hook info HC.KeywordConditional
       | _else_ -> ()
       )));
  info_of_tok = (fun (x, _origin) -> x);
}

let dockerfile = {
  parse = Parse_languages.parse_dockerfile;
  highlight = (fun ~tag_hook prefs file (ast, toks) -> 
        Highlight_AST.visit_for_highlight ~tag_hook prefs file (ast, toks));
  info_of_tok = (fun (x, _origin) -> x);
}

(* TODO *)
let yaml = {
  parse = Parse_languages.parse_yaml;
  highlight = (fun ~tag_hook prefs file (ast, toks) -> 
        Highlight_AST.visit_for_highlight ~tag_hook prefs file (ast, toks));
  info_of_tok = (fun (x, _origin) -> x);
}

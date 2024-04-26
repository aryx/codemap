(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *
 *)
open Fpath_.Operators

module TH = Token_helpers_nw

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the token list contains also the comment-tokens *)
type program_and_tokens = Ast_nw.program * Lexer_nw.token list

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens input_source =
  Lexer_nw.reset ();
  let token lexbuf =
    match Lexer_nw.current_mode () with
    | Lexer_nw.INITIAL -> Lexer_nw.tex lexbuf
    | Lexer_nw.IN_VERBATIM -> Lexer_nw.verbatim lexbuf
    | Lexer_nw.IN_VERB c -> Lexer_nw.verb c lexbuf
    | Lexer_nw.IN_NOWEB_CHUNK -> Lexer_nw.noweb lexbuf
  in
  Parsing_helpers.tokenize_all_and_adjust_pos input_source token TH.visitor_info_of_tok
    TH.is_eof
[@@profiling]

(*****************************************************************************)
(* Fuzzy parsing *)
(*****************************************************************************)

let parse_fuzzy file =
  let toks = tokens (Parsing_helpers.file file) in
  let trees =
    Lib_ast_fuzzy.mk_trees
      { Lib_ast_fuzzy.tokf = TH.info_of_tok; kind = TH.token_kind_of_tok }
      toks
  in
  (trees, toks)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse (filename : Fpath.t) =
  let stat = Parsing_stat.default_stat !!filename in
  let ast, toks = parse_fuzzy !!filename in
  ((ast, toks), stat)
[@@profiling]

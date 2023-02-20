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

(*****************************************************************************)
(* Tree-sitter or pfff *)
(*****************************************************************************)
(* TODO: move to semgrep/languages/ocaml/ at some point? *)

let parse_ocaml file =
  let res =
    Common.save_excursion Flag_parsing.error_recovery true (fun()->
        Parse_ml.parse file
        )
  in
  match res with
  | { Parsing_result.ast = []; tokens = _; stat } when stat.Parsing_stat.error_line_count > 0 ->
      (* trying with tree-sitter *)
      let res2 = Parse_ocaml_tree_sitter.parse file in
      (* hopefully the Parse_info.t in the leaves of the AST are matching
       * the Parse_info.t in the tokens of the menhir parser so the 
       * highlighters Hashtbl.mem still works even if the tokens come from
       * one place and the AST from another.
       *)
      (match res2.Tree_sitter_run.Parsing_result.program with
      | Some ast -> { res with ast }
      | None -> res
      )
  | _else_ -> res

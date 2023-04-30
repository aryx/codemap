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
open Common2 (* <=> *)
module R = Tree_sitter_run.Raw_tree
module H = Parse_tree_sitter_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* TODO: factorize with semgrep/src/parsing/Parse_target.ml at some point *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* TODO: this should probably be in a different file *)
type origin_info =
  (* those were extracted from the tree-sitter Concrete Syntax Tree (CST) *)
  | InCST
  (* those are all the ranges in the file that do not correspond to
   * an info in the CST (e.g., space, comments), that is the
   * tokens from the extra: field in tree-sitter grammars 
   *)
  | Extra

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* mostly a copy-paste of semgrep/libs/ast_generic/Raw_tree.visit *)
let visit ~v_token ~v_any x =
  let rec aux = function
  | R.Token tok -> v_token tok
  | R.List xs -> List.iter aux xs
  | R.Tuple xs -> List.iter aux xs
  | R.Case (_cons, x) -> aux x
  | R.Option opt -> (
      match opt with
      | None -> ()
      | Some x -> aux x)
  | R.Any any -> v_any any
  in
  aux x

(* add back the space and comments tokens that are skipped by tree-sitter
 * by looking at ranges of the file in env not covered by the Parse_info.t
 * we got from the CST.
 *)
let add_extra_infos file (infos : Tok.t list) : (Tok.t * origin_info) list =
  let bigstr = Common.read_file file in
  let max = String.length bigstr in
  let conv = Pos.full_charpos_to_pos_large file in

  let rec aux current xs =
    match xs with
    | [] ->
       if current < max
       then 
          let (line, column) = conv current in
          let str = String.sub bigstr current (max - current) in
          let loc = { Tok.pos = { Pos.file; line; column; charpos = current}; str } in
          [Tok.tok_of_loc loc, Extra]
       else []
    | x::xs ->
      if Tok.is_fake x
      then (* filter fake tokens *) aux current xs
      else
        let loc = Tok.unsafe_loc_of_tok x in
        (match current <=> loc.pos.charpos with
        | Inf ->
         let (line, column) = conv current in
         let str = String.sub bigstr current (loc.pos.charpos - current) in
         let loc2 = { Tok.pos = { Pos.file; line; column; charpos = current; };
                      str } in
         (Tok.tok_of_loc loc2, Extra)::aux (loc.pos.charpos) (x::xs)
      | Equal ->
         (x, InCST)::(aux (loc.pos.charpos + String.length loc.str) xs)
      | Sup ->
         raise Common.Impossible
      )
  in
  aux 0 infos

(* TODO? could return a (Parse_info.t, Parse_info.t) Common.either
 * where Left = comes from CST and Right = extra not in CST 
 * (e.g., space/comments)
 *)
let extract_infos_raw_tree file (raw : unit Tree_sitter_run.Raw_tree.t) : (Tok.t * origin_info) list =
  let infos = ref [] in
  let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
  visit ~v_token:(fun tok -> Common.push (H.token env tok) infos) ~v_any:(fun _ -> ()) raw;
  !infos |> List.rev |> add_extra_infos file

(*****************************************************************************)
(* Tree-sitter only *)
(*****************************************************************************)
(* LATER: factorize common code in all those parse_xxx *)

let parse_rust file =
  let res = Parse_rust_tree_sitter.parse file in
  let tokens = 
    let res = Tree_sitter_rust.Parse.file file in
    match res.Tree_sitter_run.Parsing_result.program with
    | None -> []
    | Some cst ->
       let raw = Tree_sitter_rust.Boilerplate.map_source_file () cst in
       extract_infos_raw_tree file raw
  in
  let ast = 
    match res.Tree_sitter_run.Parsing_result.program with
    | Some ast -> 
        Naming_AST.resolve Lang.Rust ast;
        ast
    | None -> []
  in
  ast, tokens

let parse_jsonnet ( file : Common.filename) =
  let res = Parse_jsonnet_tree_sitter.parse (Fpath.v file) in
  let tokens = 
    let res = Tree_sitter_jsonnet.Parse.file file in
    match res.Tree_sitter_run.Parsing_result.program with
    | None -> []
    | Some cst ->
       let raw = Tree_sitter_jsonnet.Boilerplate.map_document () cst in
       extract_infos_raw_tree file raw
  in
  let ast = 
    match res.Tree_sitter_run.Parsing_result.program with
    | Some ast -> 
        let gen = Jsonnet_to_generic.program ast in
        Naming_AST.resolve Lang.Jsonnet gen;
        gen
    | None -> []
  in
  ast, tokens

let parse_lisp file =
  let res = Parse_clojure_tree_sitter.parse file in
  let tokens = 
    let res = Tree_sitter_clojure.Parse.file file in
    match res.Tree_sitter_run.Parsing_result.program with
    | None -> []
    | Some cst ->
       let raw = Tree_sitter_clojure.Boilerplate.map_source () cst in
       extract_infos_raw_tree file raw
  in
  let ast = 
    match res.Tree_sitter_run.Parsing_result.program with
    | Some ast -> 
        Naming_AST.resolve Lang.Lisp ast;
        ast
    | None -> []
  in
  ast, tokens

let parse_bash file =
  let res = Parse_bash_tree_sitter.parse file in
  let tokens = 
    let res = Tree_sitter_bash.Parse.file file in
    match res.Tree_sitter_run.Parsing_result.program with
    | None -> []
    | Some cst ->
       let raw = Tree_sitter_bash.Boilerplate.map_program () cst in
       extract_infos_raw_tree file raw
  in
  let ast = 
    match res.Tree_sitter_run.Parsing_result.program with
    | Some ast -> 
        let gen = Bash_to_generic.program ast in
        Naming_AST.resolve Lang.Bash gen;
        gen
    | None -> []
  in
  ast, tokens

let parse_dockerfile file =
  let res = Parse_dockerfile_tree_sitter.parse file in
  let tokens = 
    let res = Tree_sitter_dockerfile.Parse.file file in
    match res.Tree_sitter_run.Parsing_result.program with
    | None -> []
    | Some _cst ->
        failwith "XXX"
(* TODO
       let raw = Tree_sitter_dockerfile.Boilerplate.map_document () cst in
       extract_infos_raw_tree file raw
*)
  in
  let ast = 
    match res.Tree_sitter_run.Parsing_result.program with
    | Some ast -> 
        let gen = Dockerfile_to_generic.program ast in
        Naming_AST.resolve Lang.Bash gen;
        gen
    | None -> []
  in
  ast, tokens

let parse_yaml _file =
  failwith "TODO"


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

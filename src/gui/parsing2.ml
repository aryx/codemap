(*s: parsing2.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2010-2012 Facebook
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
(*e: Facebook copyright *)
open Common

open Highlight_code
module FT = File_type
module HC = Highlight_code
module Db = Database_code
module Flag = Flag_visual
module PH = Parse_and_highlight

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * The main entry point of this module is tokens_with_categ_of_file
 * which is called in Draw_microlevel to "render" the content of a file.
 *)

(*****************************************************************************)
(* Parsing helpers *)
(*****************************************************************************)

(* This type is needed if we want to use a single hashtbl to memoize
 * all the parsed file.
 * Now that a few language highlighters rely internaly on a generic AST and
 * highlight_AST.ml, should we also memoized the (named) generic AST?
 *)
type ast = 
  (* generic, which is used currently for: 
   * - Rust
   * - TODO Ruby
   * - Jsonnet
   * - Yaml
   * - TODO Bash 
   * - TODO Docker
   * - Lisp/Scheme/Clojure/Sexp (but currently use Raw_tree so no
   *   great highlighting for now)
  *)
  | Generic of (AST_generic.program * (Tok.t * Parse_languages.origin_info) list)
(* old: was just lexer in old pfff
  | Csharp of Parse_csharp.program_and_tokens
  | Hs  of Parse_hs.program_and_tokens
  | Erlang of Parse_erlang.program_and_tokens
*)

  (* functional *)
  | ML  of (AST_ocaml.program, Parser_ml.token) Parsing_result.t
  | Scala of (AST_scala.program, Parser_scala.token) Parsing_result.t

  (* web *)
  | Html of Parse_html.program_and_tokens
  | Js  of (Ast_js.a_program, Parser_js.token) Parsing_result.t
  | Php of (Cst_php.program, Parser_php.token) Parsing_result.t

  (* system *)
  | Cpp of (Ast_cpp.program, Parser_cpp.token) Parsing_result.t
  | Go of (Ast_go.program, Parser_go.token) Parsing_result.t

  (* mainstream *)
  | Java of (Ast_java.program, Parser_java.token) Parsing_result.t

  (* scripting *)
  | Python of (AST_python.program, Parser_python.token) Parsing_result.t

  (* documentation *)
  | Noweb of Parse_nw.program_and_tokens
  (* less? | Org of Org_mode.org ? *)

let _hmemo_file = Hashtbl.create 101

(* with directories with many files, this is useful *)
let parse_cache parse_in extract (file : Fpath.t) =
  Profiling.profile_code "View.parse_cache" (fun () ->
    let mtime = UFile.filemtime file in
    let recompute = 
      if Hashtbl.mem _hmemo_file file
      then
        let (oldmtime, _ast) = Hashtbl.find _hmemo_file file in
        mtime > oldmtime
      else true
    in
    let ast =
      if recompute
      then begin
        let ast = parse_in file in
        Hashtbl.replace _hmemo_file file (mtime, ast);
        ast
      end
      else Hashtbl.find _hmemo_file file |> snd
    in
    extract ast
  )
(*****************************************************************************)
(* Semantic ehancement *)
(*****************************************************************************)

let use_arity_of_use_count n =
  match () with
  (* note that because my PHP object analysis have some threshold
   * on the number of callers (see threshold_callers_indirect_db)
   * the number for HugeUse can not be more than this one otherwise
   * you will miss some cases
   *)
  | _ when n >= 100 -> HugeUse
  | _ when n > 20   -> LotsOfUse
  | _ when n >= 10  -> MultiUse
  | _ when n >= 2   -> SomeUse
  | _ when n =|= 1    -> UniqueUse
  | _               -> NoUse

let rewrite_categ_using_entities s categ file entities =
  match Db.entity_kind_of_highlight_category_def categ with
  | None -> categ
  | Some e_kind ->

    let entities = 
      Hashtbl.find_all entities s |> List.filter (fun e ->
        (* we could have the full www dbcode but run the treemap on
         * a subdir in which case the root will not be the same.
         * It's a good approximation to just look at the basename.
         * The only false positive we will get if another file,
         * with the same name happened to also define entities
         * with the same name, which would be rare.
         * 
         * update: TODO use Model2.readable_to_absolute_filename_under_root ?
         *)
        Filename.basename e.Db.e_file = Filename.basename file &&
        (* some file have both a function and class with the same name *)
        Database_code.matching_def_short_kind_kind e_kind e.Db.e_kind 
      )
    in
    match entities with
      | [] -> categ
      | [e] ->
          let use_cnt = e.Db.e_number_external_users in
          let arity = use_arity_of_use_count use_cnt in
          if Database_code.is_entity_def_category categ
          then HC.rewrap_arity_def2_category arity categ 
          else categ
      | _x::_y::_xs ->
        (* TODO: handle __construct directly *)
        if not (List.mem s ["__construct"])
        then UCommon.pr2_once (spf "multi def found for %s in %s" s file);
        categ

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let tokens_with_categ_of_file_helper 
  {PH.parse; highlight; info_of_tok} (file : string) prefs hentities =
  
  if !Flag.verbose_visual then UCommon.pr2 (spf "Parsing: %s" file);
  let (ast, toks) = parse (Fpath.v file) in

  if !Flag.verbose_visual then UCommon.pr2 (spf "Highlighting: %s" file);
    let h = Hashtbl.create 101 in

    (* computing the token attributes *)
    highlight ~tag_hook:(fun info categ -> Hashtbl.add h info categ)
      prefs (Fpath.v file) (ast, toks);

    (* getting the text *)
    toks |> List_.filter_map (fun tok -> 
      let info = info_of_tok tok in
      let s = Tok.content_of_tok info in
      if not (Tok.is_origintok info)
      then None
      else 
        let categ = Common2_.hfind_option info h |> Option.map (fun categ ->
          rewrite_categ_using_entities s categ file hentities
        ) in
        Some (s, categ,{ Pos.l = Tok.line_of_tok info; c = Tok.col_of_tok info; })
    )

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* coupling: right now if you add a language here, you need to whitelist it
 * also in Draw_microlevel.draw_contents2.
 *)
let tokens_with_categ_of_file (file : string) hentities = 
  let ftype = FT.file_type_of_file (Fpath.v file) in
  let prefs = Highlight_code.default_highlighter_preferences in
  
  match ftype with
  (* currently abusing the OCaml parser to also parse ATD files
   * TODO? works also for Fsharp; at least the tokenizer 
   *)
  | FT.PL (FT.OCaml _) | FT.PL (FT.IDL FT.ATD) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache (fun file -> 
             ML (Parse_languages.parse_ocaml file)
         )
         (function 
         | ML {Parsing_result. ast; tokens; _} -> (ast, tokens)
         | _ -> raise Impossible));
        highlight = (fun ~tag_hook prefs file (ast, toks) -> 
          Highlight_ml.visit_program ~tag_hook prefs file (ast, toks));
        info_of_tok = Token_helpers_ml.info_of_tok;
        }
        file prefs hentities
  | FT.PL (FT.Web (FT.Php _)) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache (fun file ->
          Common.save_excursion Flag_parsing.error_recovery true (fun () ->
            (* todo: use database_light if given? we could so that
             * variables are better annotated.
             * note that database_light will be passed in
             * rewrite_categ_using_entities() at least.
             *)
(*
            let find_entity = None in
            (* work by side effect on ast2 too *)
            (try 
            Check_variables_php.check_and_annotate_program
              find_entity
              ast
             with Cst_php.TodoNamespace _ | Common.Impossible -> ()
            );
*)
            Php (Parse_php.parse file)
          ))
         (function  
          | Php {Parsing_result. ast; tokens; _} -> (ast, tokens)
          | _ -> raise Impossible));
         highlight = (fun ~tag_hook prefs _file (ast, toks) ->
          Highlight_php.visit_program ~tag:tag_hook prefs hentities 
            (ast, toks)
         );
         info_of_tok = Token_helpers_php.info_of_tok;
        }
        file prefs hentities

  | FT.PL (FT.Scala) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache (fun file -> 
           Common.save_excursion Flag_parsing.error_recovery true (fun()->
             Scala (Parse_scala.parse file)
         ))
         (function 
         | Scala {Parsing_result. ast; tokens; _} -> (ast, tokens)
         | _ -> raise Impossible));
        highlight = (fun ~tag_hook prefs file (ast, toks) -> 
          Highlight_scala.visit_program ~tag_hook prefs file (ast, toks));
        info_of_tok = Token_helpers_scala.info_of_tok;
        }
        file prefs hentities

  | FT.PL (FT.Python) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache (fun file -> 
           Common.save_excursion Flag_parsing.error_recovery true (fun()->
             Python (Parse_python.parse file))
         )
         (function 
         | Python {Parsing_result. ast; tokens; _} -> (Some ast, tokens)
         | _ -> raise Impossible
         ));
        highlight = (fun ~tag_hook prefs _file (ast, toks) -> 
          Highlight_python.visit_program ~tag_hook prefs (ast, toks));
        info_of_tok = Token_helpers_python.info_of_tok;
        }
        file prefs hentities

  | FT.PL (FT.Rust) ->
      let ph_with_cache = 
        { PH.rust with parse = (parse_cache 
              (fun file -> Generic (PH.rust.parse file))
              (function Generic (ast, toks) -> ast, toks | _ -> raise Impossible))} in
      tokens_with_categ_of_file_helper ph_with_cache
        file prefs hentities
(* TODO
  | FT.PL (FT.Ruby) ->
      let ph_with_cache = 
        { PH.ruby with parse = (parse_cache 
              (fun file -> Generic (PH.ruby.parse file))
              (function Generic (ast, toks) -> ast, toks | _ -> raise Impossible))} in
      tokens_with_categ_of_file_helper ph_with_cache
        file prefs hentities
 *)

  | FT.Config (FT.Jsonnet) ->
      let ph_with_cache = 
        { PH.jsonnet with parse = (parse_cache 
              (fun file -> Generic (PH.jsonnet.parse file))
              (function Generic (ast, toks) -> ast, toks | _ -> raise Impossible))} in
      tokens_with_categ_of_file_helper ph_with_cache
        file prefs hentities

  | FT.Config (FT.Yaml) ->
      let ph_with_cache = 
        { PH.yaml with parse = (parse_cache 
              (fun file -> Generic (PH.yaml.parse file))
              (function Generic (ast, toks) -> ast, toks | _ -> raise Impossible))} in
      tokens_with_categ_of_file_helper ph_with_cache
        file prefs hentities

  | FT.Config (FT.Dockerfile) ->
      let ph_with_cache = 
        { PH.dockerfile with parse = (parse_cache 
              (fun file -> Generic (PH.dockerfile.parse file))
              (function Generic (ast, toks) -> ast, toks | _ -> raise Impossible))} in
      tokens_with_categ_of_file_helper ph_with_cache
        file prefs hentities

  | FT.PL (FT.Lisp _) | FT.Config (FT.Sexp) ->
      let ph_with_cache = 
        { PH.lisp with parse = (parse_cache 
              (fun file -> Generic (PH.lisp.parse file))
              (function Generic (ast, toks) -> ast, toks | _ -> raise Impossible))} in
      tokens_with_categ_of_file_helper ph_with_cache
        file prefs hentities

  | FT.PL (FT.Script _) ->
      let ph_with_cache = 
        { PH.bash with parse = (parse_cache 
              (fun file -> Generic (PH.bash.parse file))
              (function Generic (ast, toks) -> ast, toks | _ -> raise Impossible))} in
      tokens_with_categ_of_file_helper ph_with_cache
        file prefs hentities

(*
  | FT.PL (FT.Csharp) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache 
         (fun file -> Csharp (Parse_csharp.parse file |> fst))
         (function Csharp (ast, toks) -> [ast, toks] | _ -> raise Impossible));
        highlight = (fun ~tag_hook prefs (ast, toks) -> 
          Highlight_csharp.visit_program ~tag_hook prefs (ast, toks));
        info_of_tok = Token_helpers_csharp.info_of_tok;
        }
        file prefs hentities

  | FT.PL (FT.Haskell _) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache 
         (fun file -> Hs (Parse_hs.parse file |> fst))
         (function Hs (ast, toks) -> [ast, toks] | _ -> raise Impossible));
        highlight = (fun ~tag_hook prefs (ast, toks) -> 
          Highlight_hs.visit_program ~tag_hook prefs (ast, toks));
        info_of_tok = Parser_hs.info_of_tok;
        }
        file prefs hentities

  | FT.PL (FT.Erlang) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache 
         (fun file -> Erlang (Parse_erlang.parse file |> fst))
         (function Erlang x -> [x] | _ -> raise Impossible));
        highlight = Highlight_erlang.visit_program;
        info_of_tok = Token_helpers_erlang.info_of_tok;
        }
        file prefs hentities
 *)
  | FT.PL (FT.Java) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache 
         (fun file -> Java (Parse_java.parse file))
          (function 
          | Java {Parsing_result. ast; tokens; _} -> (ast, tokens)
          | _ -> raise Impossible));
        highlight = Highlight_java.visit_toplevel;
        info_of_tok = Token_helpers_java.info_of_tok;
        }
        file prefs hentities

  | FT.PL (FT.Cplusplus _ | FT.C _ | FT.ObjectiveC _)
  (* TODO? for Protobuf we could now use the one in tree-sitter
   * TODO: does not work yet because got a Failure ("not a C/C++ file ...")
   *)  
  | FT.PL (FT.IDL (FT.Thrift | FT.Protobuf))
     ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache 
         (fun file -> 
          Common.save_excursion Flag_parsing.error_recovery true (fun () ->
           (* work by side effect on ast2 too *)
(* TODO? or Naming_AST.ml now better anyway?
           Check_variables_cpp.check_and_annotate_program
             ast;
*)
           Cpp (Parse_cpp.parse file)
         ))
         
         (function 
         | Cpp {Parsing_result. ast; tokens; _} -> (ast, tokens)
         | _ -> raise Impossible));
        highlight = Highlight_cpp.visit_toplevel;
        info_of_tok = Token_helpers_cpp.info_of_tok;
        }
        file prefs hentities

  | FT.PL (FT.Go) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache 
         (fun file -> Go (Parse_go.parse file))
          (function 
          | Go {Parsing_result. ast; tokens; _} -> (ast, tokens)
          | _ -> raise Impossible));
        highlight = Highlight_go.visit_program;
        info_of_tok = Token_helpers_go.info_of_tok;
        }
        file prefs hentities

  | FT.Text ("nw" | "tex" | "texi" | "web") ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache 
         (fun file -> Noweb (Parse_nw.parse file |> fst))
         (function Noweb x -> x | _ -> raise Impossible));
        highlight = Highlight_nw.visit_program;
        info_of_tok = Token_helpers_nw.info_of_tok;
        }
        file prefs hentities


  | FT.PL (FT.Web (FT.Js | FT.Coffee | FT.TypeScript)) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache
          (fun file -> 
            Common.save_excursion Flag_parsing.error_recovery true (fun () ->
              Js (Parse_js.parse file))
          )
         (function 
         | Js {Parsing_result. ast; tokens; _} -> ast, tokens
         | _ -> raise Impossible
         ));
        highlight = Highlight_js.visit_program;
(* TODO?
          let s = Token_helpers_js.str_of_tok tok in
          Ast_js.remove_quotes_if_present s
*)
        info_of_tok = Token_helpers_js.info_of_tok;
        }
        file prefs hentities

  | FT.PL (FT.Web (FT.Html)) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache 
          (fun file -> Html (Parse_html.parse file))
          (function 
          | Html (ast, toks) -> ast, toks
          | _ -> raise Impossible));
        highlight = Highlight_html.visit_toplevel;
        info_of_tok = Token_helpers_html.info_of_tok;
        }
        file prefs hentities

  | FT.Text ("org") ->
      let org = Org_mode.parse file in
      Org_mode.highlight org

  (* ugly, hardcoded, should instead look at the head of the file for a
   * # -*- org   indication.
   * very pad and code-overlay specific.
   *)
  | FT.Text ("txt") when Filename.basename file = "info.txt" ->
      let org = Org_mode.parse file in
      Org_mode.highlight org

  | _ -> failwith 
      "impossible: should be called only when file has good file_kind"
(*e: parsing2.ml *)

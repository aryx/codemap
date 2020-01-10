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
module PI = Parse_info
module HC = Highlight_code
module Db = Database_code
module Flag = Flag_visual

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
 *)
type ast = 
  (* functional *)
  | ML  of Parse_ml.program_and_tokens
  | Hs  of Parse_hs.program_and_tokens
  | Lisp of Parse_lisp.program_and_tokens
  | Erlang of Parse_erlang.program_and_tokens
  | Skip  of Parse_skip.program_and_tokens

  (* web *)
  | Html of Parse_html.program_and_tokens
  | Js  of Parse_js.program_and_tokens
  | Php of Parse_php.program_with_comments

  (* system *)
  | Cpp of Parse_cpp.toplevels_and_tokens
  | Rust of Parse_rust.program_and_tokens

  (* mainstream *)
  | Java of Parse_java.program_and_tokens
  | Csharp of Parse_csharp.program_and_tokens

  (* scripting *)
  | Python of Parse_python.program_and_tokens

  (* documentation *)
  | Noweb of Parse_nw.program_and_tokens
  (* less? | Org of Org_mode.org ? *)

let _hmemo_file = Hashtbl.create 101

(* with directories with many files, this is useful *)
let parse_cache parse_in extract file =
  Common.profile_code "View.parse_cache" (fun () ->
    let mtime = Common2.filemtime file in
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
  | _ when n = 1    -> UniqueUse
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
        Filename.basename e.Db.e_file =$= Filename.basename file &&
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
        then pr2_once (spf "multi def found for %s in %s" s file);
        categ

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type ('ast, 'token) for_helper = {
  parse: (Common.filename -> ('ast * 'token list) list);
  highlight_visit:(tag_hook:(Parse_info.t -> HC.category -> unit) ->
                   Highlight_code.highlighter_preferences ->
                   'ast * 'token list -> unit);
  info_of_tok:('token -> Parse_info.t);
}

let tokens_with_categ_of_file_helper 
  {parse; highlight_visit; info_of_tok} file prefs hentities =
  
  if !Flag.verbose_visual then pr2 (spf "Parsing: %s" file);
  let ast2 = parse file in

  if !Flag.verbose_visual then pr2 (spf "Highlighting: %s" file);
  (* todo: ast2 should not be a list, should just be (ast, toks)
   * but right now only a few parsers will satisfy this interface
   *)
  ast2 |> List.map (fun (ast, toks) ->

    let h = Hashtbl.create 101 in

    (* computing the token attributes *)
    highlight_visit ~tag_hook:(fun info categ -> Hashtbl.add h info categ)
      prefs (ast, toks);

    (* getting the text *)
    toks |> Common.map_filter (fun tok -> 
      let info = info_of_tok tok in
      let s = PI.str_of_info info in
      if not (PI.is_origintok info)
      then None
      else 
        let categ = Common2.hfind_option info h |> Common2.fmap (fun categ ->
          rewrite_categ_using_entities s categ file hentities
        ) in
        Some (s, categ,{ Common2.l = PI.line_of_info info; c = PI.col_of_info info; })
    )) |> List.flatten

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* coupling: right now if you add a language here, you need to whitelist it
 * also in Draw_microlevel.draw_contents2.
 *)
let tokens_with_categ_of_file file hentities = 
  let ftype = FT.file_type_of_file file in
  let prefs = Highlight_code.default_highlighter_preferences in
  
  match ftype with
  | FT.PL (FT.Web (FT.Php _)) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache (fun file ->
          Common.save_excursion Flag_parsing.error_recovery true (fun () ->
            let ((ast, toks), _stat) = Parse_php.parse file in
            (* todo: use database_light if given? we could so that
             * variables are better annotated.
             * note that database_light will be passed in
             * rewrite_categ_using_entities() at least.
             *)
            let find_entity = None in
            (* work by side effect on ast2 too *)
            (try 
            Check_variables_php.check_and_annotate_program
              find_entity
              ast
             with Cst_php.TodoNamespace _ | Common.Impossible -> ()
            );
            Php ((ast, toks))
          ))
         (function Php (ast, toks) -> [ast, toks] | _ -> raise Impossible));
         highlight_visit = (fun ~tag_hook prefs (ast, toks) ->
          Highlight_php.visit_program ~tag:tag_hook prefs hentities 
            (ast, toks)
         );
         info_of_tok = Token_helpers_php.info_of_tok;
        }
        file prefs hentities

  (* works also for Fsharp; at least the tokenizer *)
  | FT.PL (FT.ML _) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache (fun file -> 
           Common.save_excursion Flag_parsing.error_recovery true (fun()->
             ML (Parse_ml.parse file |> fst))
         )
         (function 
         | ML (astopt, toks) -> 
             let ast = astopt ||| [] in
             [ast, toks] 
         | _ -> raise Impossible));
        highlight_visit = (fun ~tag_hook prefs (ast, toks) -> 
          Highlight_ml.visit_program ~tag_hook prefs (ast, toks));
        info_of_tok = Token_helpers_ml.info_of_tok;
        }
        file prefs hentities

  | FT.PL (FT.Skip) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache (fun file -> 
           Common.save_excursion Flag_parsing.error_recovery true (fun()->
             Skip (Parse_skip.parse file |> fst))
         )
         (function 
         | Skip (astopt, toks) -> 
             [astopt, toks] 
         | _ -> raise Impossible));
        highlight_visit = (fun ~tag_hook prefs (ast, toks) -> 
          Highlight_skip.visit_program ~tag_hook prefs (ast, toks));
        info_of_tok = Token_helpers_skip.info_of_tok;
        }
        file prefs hentities

  | FT.PL (FT.Haskell _) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache 
         (fun file -> Hs (Parse_hs.parse file |> fst))
         (function Hs (ast, toks) -> [ast, toks] | _ -> raise Impossible));
        highlight_visit = (fun ~tag_hook prefs (ast, toks) -> 
          Highlight_hs.visit_program ~tag_hook prefs (ast, toks));
        info_of_tok = Parser_hs.info_of_tok;
        }
        file prefs hentities

  | FT.PL (FT.Python) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache (fun file -> 
           Common.save_excursion Flag_parsing.error_recovery true (fun()->
             Python (Parse_python.parse file |> fst))
         )
         (function 
         | Python (astopt, toks) -> 
             [astopt, toks] 
         | _ -> raise Impossible
         ));
        highlight_visit = (fun ~tag_hook prefs (ast, toks) -> 
          Highlight_python.visit_program ~tag_hook prefs (ast, toks));
        info_of_tok = Token_helpers_python.info_of_tok;
        }
        file prefs hentities

  | FT.PL (FT.Csharp) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache 
         (fun file -> Csharp (Parse_csharp.parse file |> fst))
         (function Csharp (ast, toks) -> [ast, toks] | _ -> raise Impossible));
        highlight_visit = (fun ~tag_hook prefs (ast, toks) -> 
          Highlight_csharp.visit_program ~tag_hook prefs (ast, toks));
        info_of_tok = Token_helpers_csharp.info_of_tok;
        }
        file prefs hentities

  | FT.PL (FT.Rust) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache 
         (fun file -> Rust (Parse_rust.parse file |> fst))
         (function Rust (ast, toks) -> [ast, toks] | _ -> raise Impossible));
        highlight_visit = (fun ~tag_hook prefs (ast, toks) -> 
          Highlight_rust.visit_program ~tag_hook prefs (ast, toks));
        info_of_tok = Token_helpers_rust.info_of_tok;
        }
        file prefs hentities

  | FT.PL (FT.Erlang) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache 
         (fun file -> Erlang (Parse_erlang.parse file |> fst))
         (function Erlang x -> [x] | _ -> raise Impossible));
        highlight_visit = Highlight_erlang.visit_program;
        info_of_tok = Token_helpers_erlang.info_of_tok;
        }
        file prefs hentities

  | FT.PL (FT.Java) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache 
         (fun file -> Java (Parse_java.parse file |> fst))
          (function 
          | Java (ast, toks) -> [Common2.some ast, (toks)] 
          | _ -> raise Impossible));
        highlight_visit = Highlight_java.visit_toplevel;
        info_of_tok = Token_helpers_java.info_of_tok;
        }
        file prefs hentities

  | FT.PL (FT.Lisp _) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache 
         (fun file -> Lisp (Parse_lisp.parse file |> fst))
         (function
         |  Lisp (ast, toks) -> [Common2.some ast, toks]
         | _ -> raise Impossible));
        highlight_visit = Highlight_lisp.visit_toplevel;
        info_of_tok = Parser_lisp.info_of_tok;
        }
        file prefs hentities

  | FT.Text ("nw" | "tex" | "texi" | "web") ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache 
         (fun file -> Noweb (Parse_nw.parse file |> fst))
         (function Noweb x -> [x] | _ -> raise Impossible));
        highlight_visit = Highlight_nw.visit_program;
        info_of_tok = Token_helpers_nw.info_of_tok;
        }
        file prefs hentities

  | FT.PL (FT.Cplusplus _ | FT.C _ | FT.Thrift | FT.ObjectiveC _) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache 
         (fun file -> 
          Common.save_excursion Flag_parsing.error_recovery true (fun () ->
           let (ast2, _stat) = Parse_cpp.parse file in
           let ast = Parse_cpp.program_of_program2 ast2 in
           (* work by side effect on ast2 too *)
           Check_variables_cpp.check_and_annotate_program
             ast;
           Cpp ast2
         ))
         (function Cpp x -> x | _ -> raise Impossible));
        highlight_visit = Highlight_cpp.visit_toplevel;
        info_of_tok = Token_helpers_cpp.info_of_tok;
        }
        file prefs hentities

  | FT.PL (FT.Web (FT.Js | FT.Coffee | FT.TypeScript)) ->
      tokens_with_categ_of_file_helper 
        { parse = (parse_cache
          (fun file -> 
            Common.save_excursion Flag_parsing.error_recovery true (fun () ->
              Js (Parse_js.parse file |> fst))
          )
         (function 
         | Js (astopt, toks) -> 
             let ast = astopt ||| [] in
             [ast, toks] 
         | _ -> raise Impossible
         ));
        highlight_visit = Highlight_js.visit_program;
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
          | Html (ast, toks) -> [ast, toks] 
          | _ -> raise Impossible));
        highlight_visit = Highlight_html.visit_toplevel;
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
  | FT.Text ("txt") when Common2.basename file =$= "info.txt" ->
      let org = Org_mode.parse file in
      Org_mode.highlight org

  | _ -> failwith 
      "impossible: should be called only when file has good file_kind"
(*e: parsing2.ml *)

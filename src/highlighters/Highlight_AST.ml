(* Yoann Padioleau
 *
 * Copyright (C) 2020-2024 Semgrep Inc.
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
open Common

open AST_generic
open Highlight_code
open Entity_code
module E = Entity_code
module G = AST_generic
module H = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Syntax highlighting of AST_generic for codemap (and now also for efuns).
 *
 * This code can also be abused to generate the light database
 * and the TAGS file (because codemap needs to know about
 * def and use of entities), but you should now prefer to
 * base such analysis on graph_code_xxx.ml instead of this file.
 *
 * history:
 *  - generalized from highlight_ml.ml and highlight_js.ml
 *  - extended for Highlight_scala.ml
 *  - extended for Rust to not just colorize identifiers but everything
*)

(*****************************************************************************)
(* Helpers when have global analysis information *)
(*****************************************************************************)

(* totally ocaml specific *)
let h_builtin_modules = Hashtbl_.hashset_of_list [
  "Pervasives"; "Common";
  "List"; "Hashtbl"; "Array"; "Stack";
  "String"; "Bytes"; "Str";
  "Sys"; "Unix"; "Gc";
  "Filename";
]

let fake_no_def2 = NoUse
let fake_no_use2 = (NoInfoPlace, UniqueDef, MultiUse)

let _info_of_name ((_s, info), _nameinfo) = info

let id_of_name (name : name) : ident =
  AST_generic_helpers.id_of_name name |> fst

(*****************************************************************************)
(* AST helpers *)
(*****************************************************************************)

let kind_of_body attrs x =
  let def2 = Def2 fake_no_def2 in
  match x.e with
  | Lambda _ -> Entity (Function, def2)

  (* ocaml specific *)
  | Call ({e = N (Id ((("ref", _)), _idinfo)); _}, _args) ->
      Entity (Global, def2)
  | Call ({e = N (IdQualified 
              { name_last = ("create", _), None;
                name_middle = Some (QDots [("Hashtbl", _), None]); _ }); _} ,
          _args) ->
      Entity (Global, def2)

  (* general cases *)
  | _ -> 
      let kind = 
        match () with
        |  _ when H.has_keyword_attr Const  attrs -> Constant
        | _ when H.has_keyword_attr Mutable attrs -> Global
        | _ -> Constant
      in 
      Entity (kind, def2)

(* todo: actually it can be a typedef alias to a function too
 * but this would require some analysis
*)
let kind_of_ty ty =
  let def2 = Def2 fake_no_def2 in
  match ty.t with
  | TyFun _ -> (FunctionDecl NoUse)

  (* ocaml specific *)
  | TyApply ({ t = TyN (Id (("ref", _), _)); _}, _) -> Entity (Global, def2)
  (* todo: should handle module aliases there too *)
  | TyApply ({ t = TyN (IdQualified {
        name_last = (("t", _), None);
        name_middle = Some (QDots [(("Hashtbl", _), None)]); 
        _ }); _}, _) ->
      Entity (Global, def2)
  | _ -> Entity (Constant, def2)

(* TODO: should not need if AST_generic was cleaner *)
let info_of_dotted_ident xs =
  match List.rev xs with
    | [] -> raise Impossible
    | (_s, info)::_ -> info   

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* try to better colorize identifiers, which can be many different things:
 * a field, a type, a function, a parameter, a local, a global, a generic,
 * etc.
 * update: now colorize everything.
*)
let visit_program (already_tagged, tag) ast =
  let tag ii categ =
    if Tok.is_fake ii
    then ()
    else tag ii categ
  in

  let tag_id (_s, ii) categ =
    (* so treat the most specific in the enclosing code and then
     * do not fear to write very general case patterns later because
     * the specific will have priority over the general
     * (e.g., a Method use vs a Field use)
    *)
    if not (Hashtbl.mem already_tagged ii)
    then begin 
        Hashtbl.replace already_tagged ii true;
        tag ii categ
    end
  in
  let tag_ids xs categ =
    xs |> List.iter (fun id -> tag_id id categ)
  in
  let _tag_if_not_tagged ii categ =
    if not (Hashtbl.mem already_tagged ii)
    then tag ii categ
  in

  (* ocaml specific *)
  let in_let = ref false in
  let in_try_with = ref false in

  let visitor =
    object
      inherit [_] AST_generic.iter_no_id_info as super

      method! visit_definition env x =
        match x with
        | ({ name = EN (Id (id, _)); attrs; _}, def) ->
            (match def with
             | Signature sig_ ->
                 tag_id id (kind_of_ty sig_.sig_type);
                 super#visit_definition env x

             | ModuleDef { mbody = body } ->
                 tag_id id (Entity (E.Module, Def2 fake_no_def2));
                 (match body with
                  | ModuleAlias name ->
                      let info = info_of_dotted_ident name in
                      tag info (Entity (Module, Use2 fake_no_use2));
                  | _ -> ()
                 );
                 super#visit_definition env x

             | TypeDef { tbody = G.Exception _ } ->
                 tag_id id (Entity (E.Exception, Def2 fake_no_def2));
                 super#visit_definition env x
             | TypeDef { tbody = kind } ->
                 tag_id id (Entity (E.Type, Def2 fake_no_def2));
                 (* todo: ty_params *)
                 (match kind with
                  | OrType xs ->
                      xs |> List.iter (function
                        | OrConstructor (id, _) ->
                            tag_id id (Entity (Constructor, Def2 fake_no_def2))
                        | _ -> ()
                      )
                  | AndType (_, xs, _) ->
                      xs |> List.iter (function
                        | F ({s=DefStmt({name=EN (Id (id, _)); _}, _);_})->
                            tag_id id (Entity (Field, (Def2 fake_no_def2)));
                        | _ ->  ()
                      );
                  | _ -> ()
                 );
                 super#visit_definition env x

             | VarDef { vinit = Some body; vtype = _; vtok = _ }  ->
                 (if not !in_let
                  then tag_id id (kind_of_body attrs body)
                  else tag_id id (Local Def)
                 );
                 Common.save_excursion in_let true (fun () ->
                   super#visit_definition env x
                 )

             | VarDef { vinit = None; vtype = _; vtok = _ }  ->
                  super#visit_definition env x

             | FuncDef _ ->
                 (if not !in_let
                  then tag_id id (Entity (Function, (Def2 NoUse)))
                  else tag_id id (Local Def)
                 );
                 Common.save_excursion in_let true (fun () ->
                   super#visit_definition env x
                 )
             | ClassDef _ ->
                 let kind = 
                      if H.has_keyword_attr G.RecordClass attrs
                      then Constructor
                      else Class
                 in
                 tag_id id (Entity (kind, (Def2 NoUse)));
                 super#visit_definition env x
             | FieldDefColon _ | MacroDef _ | UseOuterDecl _
             | OtherDef _ | EnumEntryDef _ ->
                  super#visit_definition env x

            )
        | _ -> super#visit_definition env x

      (* JS
               V.kprop = (fun (k,_) x ->
                 (match x with
                  | Field {fld_name = PN name; fld_body = Some (Fun _); _} ->
                      tag_name name (Entity (E.Method, (Def2 fake_no_def2)));
                  | Field {fld_name = PN name; _ } ->
                      tag_name name (Entity (E.Field, (Def2 fake_no_def2)));
                  | _ -> ()
                 );
                 k x
               );
      *)
      method! visit_function_definition env x =
          tag (snd x.fkind) Keyword;
          super#visit_function_definition env x

      method! visit_directive env x =
        (match x.d with
         | ImportAll (tk, DottedName xs, _) ->
              tag tk KeywordModule;
             (match List.rev xs with
             | id::xs ->
                tag_ids xs (Entity (Package, Use2 fake_no_use2));
               (* depend on language, Class in Scala, Module in OCaml *)
               (* let kind = Class in *)
               (* OCaml specific? *)
               (* tag_id id (Entity (kind, Use2 fake_no_use2)) *)
               tag_id id BadSmell
             | [] -> ()
             )
         | ImportFrom (tk, DottedName xs, _ys) ->
              tag tk KeywordModule;
              tag_ids xs (Entity (Package, Use2 fake_no_use2));
              (* depend on language, Class in Scala, Module in OCaml *)
              let _kind = Class in
              (* TODO now import_from_kind list type *)
(*              ys |> List.iter (fun (id, opt) ->
              (match opt with
              | None ->
                tag_id id (Entity (kind, Def2 fake_no_def2))
              | Some (id2, _) ->
                tag_id id (Entity (kind, Use2 fake_no_use2));
                tag_id id2 (Entity (kind, Def2 fake_no_def2))
             ));
*)       
             ()
              
         | G.Package (tk, xs) ->
            tag tk KeywordModule;
            tag_ids xs (Entity (Package, Def2 fake_no_def2))
         | _ -> ()
        );
        super#visit_directive env x

      method! visit_name env x =
        (match x with
        | IdQualified { name_middle = Some (QDots xs); _} ->
           tag_ids (List.map fst xs) (Entity (Module, Use2 fake_no_use2))
        | _ -> ()
        );
        super#visit_name env x

      method! visit_parameter env x =
        (match x with
         | ParamPattern (PatId (id, _idinfo)) ->
             tag_id id (Parameter Def);
             (* less: let kpattern do its job? *)
         | ParamPattern _ -> ()
         | Param p | ParamRest (_, p) | ParamHashSplat (_, p) | ParamReceiver p ->
             (match p.pname with
              | Some id ->
                  tag_id id (Parameter Def);
              | None -> ()
             )
         | ParamEllipsis _ | OtherParam _ -> ()
        );
        super#visit_parameter env x
(*
    V.kargument = (fun (k, _) x ->
      (match x with
      | ArgImplicitTildeExpr (_, name) ->
        let info = Ast.info_of_name name in
        (* todo: could be a Parameter use, need scope analysis *)
        tag info (Local Use)
      | _ -> ()
      );
      k x
    );

      | LetPattern (pat, _tok, body) ->
          (match pat with
          | PatTyped (_, PatVar name, _, _ty, _) ->
              let info = Ast.info_of_name name in
              if not !in_let
              then tag info (kind_of_body body)
              else tag info (Local (Def))
          | _ -> ()
          );
          Common.save_excursion in_let true (fun () ->
            k x
          )
    );
*)

      method! visit_stmt env x =
        match x.s with
        | Try (tk, _e (*, tok_with*), _match_cases, _else_cases, _finally) ->
            tag tk KeywordExn;
            Common.save_excursion in_try_with true (fun () ->
              super#visit_stmt env x
            )
        | If (tk, _, _, _) | While (tk, _, _) | Switch (tk, _, _) ->
            tag tk KeywordConditional;
            super#visit_stmt env x
        | For (tk, header, _) ->
            tag tk KeywordLoop;
            (match header with
            | ForEach (_, tk, _) -> tag tk KeywordLoop
            | ForClassic _ | MultiForEach _ | ForEllipsis _ -> () 
            );
            super#visit_stmt env x
        | Return (tk, _, _) ->
            tag tk Keyword;
            super#visit_stmt env x
        | Break (tk, _, _) | Continue (tk, _, _) ->
            tag tk Keyword;
            super#visit_stmt env x
        | _ -> super#visit_stmt env x

      method! visit_literal _env x =
        match x with
        | Bool (_, tk) -> tag tk Boolean
        | Int (_, tk) | Float (_, tk) | Imag (_, tk) | Ratio (_, tk) ->
            tag tk Number
        | Char (_, tk) -> tag tk String
        | String (l, (_, tk), r) -> 
              tag l String;
              tag tk String;
              tag r String
        | Regexp ((l, (_, tk), r), _opt) -> 
              tag l Regexp;
              tag tk Regexp;
              tag r Regexp
        | Atom (_, (_, tk)) -> tag tk Atom
        | Unit tk | Null tk | Undefined tk -> tag tk Null

      method! visit_expr env x =
        match x.e with
        (* 
              | Apply (Id (name, {contents = Global _ | NotResolved}), _) ->
                 tag_name name (Entity (E.Function, (Use2 fake_no_use2)));
              | Apply (Id (_name, {contents = Local | Param}), _) ->
                 (* todo: tag_name name PointerCall; *)
                 ()
        *)

        | N (Id (id, idinfo)) ->
            (* TODO could be a func passed to a higher
             *  order function, as in List.map snd, or even x |> Common.sort
            *)
            (* could have been tagged as a function name in the rule below *)
            let categ = 
              match !(idinfo.id_resolved) with
              | Some (kind, _sid) ->
                 (match kind with
                 | G.LocalVar -> (Local Use)
                 | G.Parameter -> (Parameter Use)
                 | G.Global -> Entity (Global, Use2 fake_no_use2)
                 | G.EnclosedVar -> Entity (Global, Use2 fake_no_use2)
                 | G.ImportedEntity _ | G.GlobalName _ -> Entity (Global, Use2 fake_no_use2)
                 | G.ImportedModule _ -> Entity (Module, Use2 fake_no_use2)
                 | G.TypeName -> Entity (Type, Use2 fake_no_use2)
                 | G.Macro -> Entity (Macro, Use2 fake_no_use2)
                 | G.EnumConstant -> Entity (Constructor, Use2 fake_no_use2)
                 )
              | None -> Normal
            in
            tag_id id categ;
            super#visit_expr env x

        | Special (kind, info) ->
            (match kind with
             | Eval -> tag info BadSmell
             | _ -> tag info Builtin
            );
            super#visit_expr env x

        (* pad specific *)
        | Call ({ e = N (Id ((("=~", _)), _idinfo)); _},
                (_, [_arg1; Arg ({ e = L (G.String (_, (_, info), _)); _})], _)) ->
            tag info Regexp;
            super#visit_expr env x
        (* ocaml specific *)
        | Call ({ e = N (Id ((("ref", info)), _idinfo)); _}, _args) ->
            tag info UseOfRef;
            super#visit_expr env x


        | Call ({ e = DotAccess (_, _, (FN (Id (id, _)))); _}, _) ->
            tag_id id (Entity (Method, (Use2 fake_no_use2)));
            super#visit_expr env x

        | Call ({ e = N (Id (id, _idinfo)); _}, _args) ->
            tag_id id (Entity (Function, (Use2 fake_no_use2)));
            super#visit_expr env x
        | Call ({e = N (IdQualified 
                    { name_last = (id, None);
                      name_middle = qu; _}); _}, _args)->
            (match qu with
             | Some (QDots [(s2, info2), None]) when Hashtbl.mem h_builtin_modules s2->
                 tag info2 BuiltinCommentColor;
                 tag_id id Builtin;
             | _ ->
                 tag_id id (Entity (Function, (Use2 fake_no_use2)));
            );
            super#visit_expr env x

        (* disambiguate "with" which can be used for match, try, or record *)
(* TODO now a stmt
        | MatchPattern (_e1, (*tok_with,*) _match_cases) ->
            (*tag tok_with (KeywordConditional); *)
            super#visit_expr env x
*)

        (* JS TODO
                    | Apply (ObjAccess (_, _, PN name), _) ->
                        tag_name name (Entity (E.Method, (Use2 fake_no_use2)));
                    | Fun (_, Some name) ->
                        tag_name name (Entity (E.Function, (Use2 fake_no_use2)));
        *)
        | DotAccess (_e, tok, 
             (FN (Id (id, _) | IdQualified {name_last = (id, _); _}))) ->
            (match Tok.content_of_tok tok with
             (* ocaml specific *)
             | "#" -> tag_id id (Entity (Method, (Use2 fake_no_use2)))

             | _ -> tag_id id (Entity (Field, (Use2 fake_no_use2)))
            );
            super#visit_expr env x
        | G.Constructor (name, _eopt) ->
            let id = id_of_name name in
            tag_id id (Entity (Constructor,(Use2 fake_no_use2)));
            super#visit_expr env x

        | Record (_, xs, _) ->
            xs |> List.iter (fun x ->
              match x with
              | F ({s=DefStmt ({ name = EN name; _}, _);_})->
                  let id = id_of_name name in
                  tag_id id (Entity (Field, (Use2 fake_no_use2)));
              | _ -> ()
            );
            super#visit_expr env x
        (* coupling: with how record with qualified name in ml_to_generic.ml *)
(* TODO encoded differently now
        | OtherExpr (OE_RecordFieldName, (Di name)::_) ->
            let info = info_of_dotted_ident name in
            tag info (Entity (Field, (Use2 fake_no_use2)));
            super#visit_expr env x
*)

        | Comprehension (_, (_, (_e, xs), _)) ->
            xs |> List.iter (function
              | CompFor (tfor, _, tin, _) ->
                   tag tfor KeywordLoop;
                   tag tin KeywordLoop;
              | CompIf (tif, _) ->
                    tag tif KeywordConditional
            );
            super#visit_expr env x
        | _ -> super#visit_expr env x

      method! visit_pattern env x =
        (match x with
         | PatConstructor (name, _popt) ->
             let id = id_of_name name in
             if !in_try_with
             then tag_id id (KeywordExn)
             else tag_id id (ConstructorMatch fake_no_use2)
         | PatId (id, _idinfo) ->
             tag_id id (Parameter Def)
         | PatAs (_p1, (id, _idinfo)) ->
             tag_id id (Parameter Def)
         | PatRecord (_, xs, _) ->
             xs |> List.iter (fun (name, _pat) ->
               let info = info_of_dotted_ident name in
               tag info (Entity (Field, (Use2 fake_no_use2)));
             )
         | _ -> ()
        );
        super#visit_pattern env x

      method! visit_type_ env t =
        (match t.t with
         | TyN (name) ->
             let id = id_of_name name in
             tag_id id (Entity (Type, (Use2 fake_no_use2)))
         | TyApply ({t = TyN (name); _}, _ty_args) ->
             let id = id_of_name name in
             (* different color for higher-order types *)
             tag_id id TypeVoid;
             (* todo: ty_args *)
         | TyVar id ->
             tag_id id TypeVoid;
         | _ -> ()
        );
        super#visit_type_ env t

      method! visit_type_parameter env x =
        (match x with
        | TP { tp_id; _ } ->
           tag_id tp_id (Entity (Type, (Def2 fake_no_def2)))
        | _ -> ()
        );
        super#visit_type_parameter env x
      
      method! visit_attribute env x =
        match x with
        | KeywordAttr (kind, tk) ->
           (match kind with
           | Public | Private | Protected -> tag tk KeywordObject
           | _else_ -> tag tk Keyword
           );
           super#visit_attribute env x
        | NamedAttr (tk, _, _) ->
           tag tk Attribute;
           super#visit_attribute env x
        | OtherAttribute _ ->
           super#visit_attribute env x

    end
  in
  visitor#visit_any () (Pr ast);
  ()

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* TODO: go through the tokens too, especially the infos not in the CST
 * which should be spaces or comments to color specially.
 *)
let visit_for_highlight ~tag_hook _prefs _file (ast, tokens) =
  let already_tagged = Hashtbl.create 101 in
  visit_program (already_tagged, tag_hook) ast;
  tokens |> List.iter (fun (tk, origin) ->
    match origin with
    | Parse_languages.Extra -> tag_hook tk Comment
    | Parse_languages.InCST ->
      let str = Tok.content_of_tok tk in
      (match str with
      | "[" | "]" -> tag_hook tk Punctuation
      | "." -> tag_hook tk Punctuation
      | s when s =~ "^[;:,(){}<>=]+$" -> 
        tag_hook tk Punctuation
      (* we could guard with language. Note that 'else'
       * is not in the generic AST, hence this special case
       *)
      | "else" -> tag_hook tk KeywordConditional
      | _else_ -> ()
      )
  );
  ()

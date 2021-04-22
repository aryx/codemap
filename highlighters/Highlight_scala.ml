(* Yoann Padioleau
 *
 * Copyright (C) 2021 R2C
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

open Highlight_code
open Entity_code
module PI = Parse_info
module T = Parser_scala
module FT = File_type

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Syntax highlighting of Scala code for codemap (and now also for efuns).
 *
*)

(* will color in red identifiers which were not tagged by the AST phase *)
(* let debug_missing_tag = ref false *)

(*****************************************************************************)
(* Helpers when have global analysis information *)
(*****************************************************************************)

(* pad-specific: see my ~/.emacs *)
(*
let h_pervasives_pad = Common.hashset_of_list [
  "pr2";"pr";"pr2_gen";
  "sprintf";"i_to_s";
  "pp2";"spf";
  "log";"log2";"log3"
]

let h_builtin_modules = Common.hashset_of_list [
  "Pervasives"; "Common";
  "List"; "Hashtbl"; "Array"; "Stack";
  "String"; "Bytes"; "Str";
  "Sys"; "Unix"; "Gc";
  "Filename";
]

let h_builtin_bool = Common.hashset_of_list [
  "not";
  "exists"; "forall";
]
*)

let no_def = NoUse
let no_use = (NoInfoPlace, UniqueDef, MultiUse)

(* Set to true when want to debug the AST-based tagger.
 * Less useful now that we do tag_if_not_tagged Error for TLowerIdent.
*)
let disable_token_phase2 = false

(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

(* The idea of the code below is to visit the program either through its
 * AST or its list of tokens. The tokens are easier for tagging keywords,
 * number and basic entities. The AST is better for tagging idents
 * to figure out what kind of ident it is.
*)
let visit_program
    ?(lexer_based_tagger=true)
    ~tag_hook _prefs  _file (_ast, toks) =

  let already_tagged = Hashtbl.create 101 in
  let tag = (fun ii categ ->
    tag_hook ii categ;
    Hashtbl.add already_tagged ii true
  )
  in
  let tag_if_not_tagged ii categ =
    if not (Hashtbl.mem already_tagged ii)
    then tag ii categ
  in
  let tag_if_not_tagged_bis ii categ =
    if not (Hashtbl.mem already_tagged ii) && lexer_based_tagger
    then tag ii categ
  in

  (* -------------------------------------------------------------------- *)
  (* AST phase 1 *)
  (* -------------------------------------------------------------------- *)
  (* Now using the AST_generic instead of Ast_ml to factorize code between
   * all the AST-based code highlighters.
  *)
  (*
  let gen = Ml_to_generic.program ast in
  (* TODO Naming_AST.resolve Lang.OCaml gen; *)
  Highlight_AST.visit_program
    (already_tagged, tag)
    gen;
  *)
  (* -------------------------------------------------------------------- *)
  (* toks phase 1 (sequence of tokens) *)
  (* -------------------------------------------------------------------- *)
  (* note: all Space are filtered in xs so it should be easier to
   * write rules (but regular comments are kept as well as newlines).
  *)
  let rec aux_toks xs =
    match xs with
    | [] -> ()

    (* pad-specific: *)
    |   T.Comment(ii)
        ::T.Nl _ii2
        ::T.Comment(ii3)
        ::T.Nl ii4
        ::T.Comment(ii5)
        ::xs ->

        let s = PI.str_of_info ii in
        let s5 =  PI.str_of_info ii5 in
        (match () with
         | _ when s =~ ".*\\*\\*\\*\\*" && s5 =~ ".*\\*\\*\\*\\*" ->
             tag ii CommentEstet;
             tag ii5 CommentEstet;
             tag ii3 CommentSection1
         | _ when s =~ ".*------" && s5 =~ ".*------" ->
             tag ii CommentEstet;
             tag ii5 CommentEstet;
             tag ii3 CommentSection2
         | _ when s =~ ".*####" && s5 =~ ".*####" ->
             tag ii CommentEstet;
             tag ii5 CommentEstet;
             tag ii3 CommentSection0
         | _ ->
             ()
        );
        aux_toks (T.Comment ii3::T.Nl ii4::T.Comment ii5::xs)

    |   T.Comment(ii)::xs when (PI.str_of_info ii) =~ "(\\*[ ]*coupling:" ->
        tag ii CommentImportance3;
        aux_toks xs


    (* When we get a parse error, the AST does not contain the definitions, but
     * we can still try to tag certain things. Here is a
     * poor's man semantic tagger. We try to infer if an ident is a func,
     * or class, or module based on the few tokens around.
     *
     * This may look ridiculous to do such semantic tagging using tokens
     * instead of the full AST but some files may parse with
     * the default parser because of some weird extensions so having
     * a solid token-based tagger is still useful as a last resort.
    *)
    | (T.Kclass _ | T.Kobject _ | T.Ktrait _)::T.ID_UPPER(_s, ii)::xs ->
        tag_if_not_tagged_bis ii (Entity (Type, Def2 no_def));
        aux_toks xs;
    | T.Kpackage _::T.ID_LOWER(_s, ii)::xs ->
        let ent = (Entity (Package, Def2 no_def)) in
        tag_if_not_tagged_bis ii ent ;
        tag_path_and_aux_toks ent xs;

    | (T.Kval _)::T.ID_LOWER(_s, ii)::xs ->
        tag_if_not_tagged_bis ii (Entity (Constant, Def2 no_def));
        aux_toks xs;
    | (T.Kvar _)::T.ID_LOWER(_s, ii)::xs ->
        tag_if_not_tagged_bis ii (Entity (Global, Def2 no_def));
        aux_toks xs;
    (* could be a method, need more context *)
    | (T.Kdef _)::T.ID_LOWER(_s, ii)::xs ->
        tag_if_not_tagged_bis ii (Entity (Function, Def2 no_def));
        aux_toks xs;

    | (T.DOT _)::T.ID_LOWER(_s, ii)::after::xs ->
        (match after with
        | T.EQ _ | T.OP _ | T.PLUS _ | T.MINUS _ | T.STAR _
        | T.RPAREN _ | T.RBRACKET _ | T.RBRACE _ -> 
          tag_if_not_tagged_bis ii (Entity (Field, Use2 no_use));
        | T.LPAREN _ ->
          tag_if_not_tagged_bis ii (Entity (Method, Use2 no_use));
        | _ -> ()
        );
        aux_toks (after::xs);
    | T.ID_LOWER (_, ii)::T.LPAREN _::xs ->
        tag_if_not_tagged_bis ii (Entity (Function, Use2 no_use));
        aux_toks xs

(*
    | T.Tlet(ii)::T.TLowerIdent(_s, ii3)::xs
      when PI.col_of_info ii = 0 ->

        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then tag ii3 (Entity (Function, (Def2 NoUse)));
        aux_toks xs;


    | T.Ttype(ii)::T.TLowerIdent(_s, ii3)::xs
      when PI.col_of_info ii = 0 ->

        if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
        then tag ii3 (Entity (Type, Def2 NoUse));
        aux_toks xs;

        (* module defs *)

    (* bad smell, use of ref *)

    | T.TBang _ii1::T.TLowerIdent(_s2, ii2)::xs ->
        tag ii2 (UseOfRef);
        aux_toks xs

    |    T.TLowerIdent(_, ii1)
         ::(T.TAssign ii2 | T.TAssignMutable ii2)
         ::xs ->
        tag ii1 (UseOfRef);
        tag ii2 (UseOfRef);
        aux_toks xs

    (* module use, and function call! *)

    | T.TUpperIdent(_s, ii)::T.TDot _ii2::T.TUpperIdent(_s2, _ii3)::xs ->
        tag ii (Entity (Module, Use2 fake_no_use2));
        aux_toks xs;

    | T.TUpperIdent(s, ii)::T.TDot _ii2::T.TLowerIdent(_s2, ii3)::xs ->

        (* see my .emacs *)
        if Hashtbl.mem h_builtin_modules s then begin
          tag ii BuiltinCommentColor;
          if not (Hashtbl.mem already_tagged ii3) && lexer_based_tagger
          then tag ii3 Builtin;
        end else begin
          tag ii (Entity (Module, Use2 fake_no_use2));
          (* tag ii3 (Function Use); *)
        end;
        aux_toks xs;

        (* labels *)
        (* can be a def or use, no way to know
            | T.TTilde ii1::T.TLowerIdent (_s, ii2)::xs ->
                (* TODO when parser, can also have Use *)
                tag ii1 (Parameter Def);
                tag ii2 (Parameter Def);
                aux_toks xs
        *)

    (* attributes *)
    | T.TBracketAtAt _::T.TLowerIdent (_, ii1)::
      T.TDot _::T.TLowerIdent (_, ii2)::xs ->
        tag ii1 Attribute;
        tag ii2 Attribute;
        aux_toks xs

    | T.TBracketAtAt _::T.TLowerIdent (_, ii)::xs ->
        tag ii Attribute;
        aux_toks xs
  *)
    | _x::xs ->
        aux_toks xs
  and tag_path_and_aux_toks _ent xs =
    aux_toks xs
  in
  let toks' = toks |> Common.exclude (function
    | T.Space _ -> true
    | _ -> false
  )
  in
  aux_toks toks';

  (* -------------------------------------------------------------------- *)
  (* toks phase 2 (individual tokens) *)
  (* -------------------------------------------------------------------- *)

  if not disable_token_phase2 then
    toks |> List.iter (fun tok ->
      match tok with
      (* specials *)

      | T.Comment ii -> tag_if_not_tagged ii Comment
(*
          if not (Hashtbl.mem already_tagged ii)
          then
            (* a little bit syncweb specific *)
            let s = PI.str_of_info ii in
            (match s with
             (* yep, s e x are the syncweb markers *)
             | _ when s =~ "(\\*[sex]:"  -> tag ii CommentSyncweb
             (* normally then use of *** or ### or --- should be enough,
              * but in some files like ocamlyacc files the preceding
              * heuristic fail in which case it's useful to have those
              * rules. Moreover ocamldoc use something similar
             *)
             | _ when s =~ "(\\*1 "  -> tag ii CommentSection1
             | _ when s =~ "(\\*2 "  -> tag ii CommentSection2
             | _ when s =~ "(\\*3 "  -> tag ii CommentSection3

             (* ocamldoc *)
             | _ when s =~ "(\\*\\* *{ *1"  -> tag ii CommentSection1
             | _ when s =~ "(\\*\\* *{ *2"  -> tag ii CommentSection2
             | _ when s =~ "(\\*\\* *{ *3"  -> tag ii CommentSection3
             | _ -> tag ii Comment
            )
*)
      | T.Nl _ii | T.Space _ii -> ()
      | T.Unknown ii -> tag ii Error
      | T.EOF _ii-> ()

      (* values *)
      | T.Knull ii -> tag ii Null
      | T.BooleanLiteral (_, ii) -> tag ii Boolean
      | T.IntegerLiteral (_,ii) | T.FloatingPointLiteral (_,ii)  -> 
            tag ii Number
      | T.CharacterLiteral (_s, ii) -> tag ii String
      | T.StringLiteral (_s,ii) ->
          (* can have been tagged as a regexp? *)
          tag_if_not_tagged ii String
      | T.T_INTERPOLATED_START (_, ii) | T.T_INTERPOLATED_END ii
      | T.T_INTERPOLATED_STRING (_, ii) ->
          tag ii String
      | T.T_DOLLAR_LBRACE ii -> tag ii Punctuation

      (* keywords *)

      | T.Kvar ii | T.Kval ii | T.Kdef ii -> tag ii Keyword
      | T.Ktype ii -> tag ii Keyword

      | T.Kif ii | T.Kelse ii -> tag ii KeywordConditional
      | T.Kreturn ii -> tag ii Keyword

      (* TODO: should also colorize it's with *)
      | T.Kmatch ii | T.Kcase ii -> tag ii KeywordConditional

      | T.Ktry ii | T.Kcatch ii | T.Kfinally ii | T.Kthrow ii  ->
        tag ii KeywordExn

      | T.Kfor ii | T.KforSome ii | T.Kdo ii | T.Kwhile ii | T.Kyield ii -> 
        tag ii KeywordLoop

      | T.Kpackage ii | T.Kimport ii -> tag ii KeywordModule

      | T.Kclass ii  | T.Ktrait ii | T.Kobject ii
      | T.Kextends ii | T.Kwith ii
      | T.Knew ii  | T.Kthis ii | T.Ksuper ii
      | T.Koverride ii | T.Kfinal ii | T.Ksealed ii | T.Kabstract ii
      | T.Kprotected ii | T.Kprivate ii
        -> tag ii KeywordObject

      | T.Kimplicit ii -> tag ii UseOfRef
      | T.Klazy ii -> tag ii Keyword

      (* Punctuation *)

      | T.EQ ii
      | T.DOT ii
      | T.SEMI ii | T.COMMA ii | T.COLON ii

      | T.PIPE ii
      | T.UNDERSCORE ii
      | T.TILDE ii
      | T.BANG ii 
      | T.SHARP ii 


      | T.LBRACKET ii | T.RBRACKET ii
      | T.LBRACE ii | T.RBRACE ii
      | T.LPAREN ii | T.RPAREN ii
       -> tag ii Punctuation

      | T.AT ii -> tag ii Attribute

      (* Semgrep *)
      | T.LDots ii | T.RDots ii | T.Ellipsis ii -> tag ii NotParsed

      (* Operators *)

      | T.PLUS ii | T.MINUS ii 
      | T.STAR ii
          -> tag ii Operator

      | T.MORECOLON ii 
      | T.LESSPERCENT ii
      | T.LESSMINUS ii
      | T.LESSCOLON ii
      | T.EQMORE ii
        ->
          tag ii Operator

      (* Identifiers *)
      | T.SymbolLiteral (_, ii) -> tag ii Atom

      | T.ID_LOWER (_, ii) -> 
            tag_if_not_tagged ii Normal
      | T.ID_UPPER (_, ii) -> 
            tag_if_not_tagged ii (Entity (Type, Use2 no_use))
      | T.ID_BACKQUOTED (_, ii) -> 
            tag_if_not_tagged ii Normal
      | T.ID_DOLLAR (_, ii) -> 
            tag ii Normal
      | T.OP (_, ii) -> 
            tag_if_not_tagged ii Operator
      
(*
      | T.TLowerIdent (s, ii)->
          (match s with
           | _ when Hashtbl.mem h_pervasives_pad s ->
               tag ii BuiltinCommentColor
           | _ when Hashtbl.mem h_builtin_bool s ->
               tag ii BuiltinBoolean
           (* all the identifiers should have been tagged by now. *)
           | _ when not is_lex_or_yacc_file && not !debug_missing_tag ->
               tag_if_not_tagged ii Error
           | _ -> ()
          )

*)

    )

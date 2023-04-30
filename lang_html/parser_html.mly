/* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
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
 */
%{

(*************************************************************************)
(* Prelude *)
(*************************************************************************)

(*
 * This file is mainly used to define the tokens. We could have
 * moved the token definitions directly in lexer_html.mll, as it was
 * done originally in ocamlnet/netstring, but we now also use this
 * file to defines a basic grammar to parse strict html.
 * Moreover it is more symetric with what we do in the other
 * lang_xxx/ directories.
 * Finally it's a good exercise to define a simple grammar using the
 * tokens; it is as a side effect documenting some of those tokens.
 *)
%}

/*(*************************************************************************)*/
/*(* Tokens *)*/
/*(*************************************************************************)*/

%token <Tok.t> TComment  /*(* <!-- ... --> *)*/
%token <Tok.t> TDoctype  /*(* <! ... > *)*/
%token <Tok.t> TPi       /*(* <? ... ?> or >  *)*/

%token <Tok.t * string>   Lelement
%token <Tok.t * string>   Lelementend
%token <Tok.t> Relement  /*(* > *)*/
%token <Tok.t> Relement_empty   /*(* />, for XML compat *)*/
%token <Tok.t * string> Cdata
%token <Tok.t * string> CdataSpecial
%token <Tok.t * string> Space
%token <Tok.t * string> Name
%token <Tok.t> Eq
%token <Tok.t * string> Literal
%token <Tok.t> Other

/*(*-----------------------------------------*)*/
%token <Tok.t> EOF

/*(*************************************************************************)*/
/*(* Rules type declaration *)*/
/*(*************************************************************************)*/

%start main
%type <unit> main

%%

/*(* assume comments and doctype are filtered out and pi are forbidden *)*/
main:
 | html_list EOF { }

html:
 | Lelement attr_list Relement_empty { }
 | Lelement attr_list Relement html_list Lelementend Relement { }
 | Cdata { }

attr:
 | Name Eq value { }

value:
 | Literal { }

/*(*************************************************************************)*/
/*(* xxx_list, xxx_opt *)*/
/*(*************************************************************************)*/

html_list:
 | html_list html { $1 @ [$2] }
 | /*(*empty*)*/ { [] }

attr_list:
 | attr_list attr { $1 @ [$2] }
 | /*(*empty*)*/ { [] }

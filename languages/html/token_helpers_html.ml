(* Yoann Padioleau
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
 *)
open Parser_html

(*****************************************************************************)
(* Visitors *)
(*****************************************************************************)

let visitor_info_of_tok f = function
  | TComment ii -> TComment (f ii)
  | TDoctype ii -> TDoctype (f ii)
  | TPi ii -> TPi (f ii)
  | Relement ii -> Relement (f ii)
  | Relement_empty ii -> Relement_empty (f ii)
  | Eq ii -> Eq (f ii)
  | Other ii -> Other (f ii)
  | Lelement (ii, s) -> Lelement (f ii, s)
  | Lelementend (ii, s) -> Lelementend (f ii, s)
  | Cdata (ii, s) -> Cdata (f ii, s)
  | CdataSpecial (ii, s) -> CdataSpecial (f ii, s)
  | Space (ii, s) -> Space (f ii, s)
  | Name (ii, s) -> Name (f ii, s)
  | Literal (ii, s) -> Literal (f ii, s)
  | EOF ii -> EOF (f ii)

let info_of_tok tok =
  let res = ref None in
  visitor_info_of_tok
    (fun ii ->
      res := Some ii;
      ii)
    tok
  |> ignore;
  Common2_.some !res

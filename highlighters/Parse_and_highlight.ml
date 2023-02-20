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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type ('ast, 'token) t = {
  parse: (Common.filename -> ('ast * 'token list) list);
  highlight:(tag_hook:(Parse_info.t -> HC.category -> unit) ->
                   Highlight_code.highlighter_preferences ->
                   Fpath.t ->
                   'ast * 'token list -> unit);
  info_of_tok:('token -> Parse_info.t);
}

(*****************************************************************************)
(* helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)


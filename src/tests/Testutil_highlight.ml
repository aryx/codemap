(*
   Shared helpers for highlighting tests.
*)
open Highlight_code
module E = Entity_code

type tokens = (string * Highlight_code.category option * Pos.linecol) list

let highlight_file path : tokens =
  let hentities = Hashtbl.create 0 in
  Parsing.tokens_with_categ_of_file path hentities

(* Find all categories for tokens matching the given string *)
let categs_of_string (tokens : tokens) s =
  tokens |> List_.filter_map (fun (s2, categ, _pos) ->
    if s2 = s then categ else None
  )

let has_entity kind categs =
  List.exists (function
    | Entity (k, _) -> k = kind
    | _ -> false) categs

let has_categ expected categs =
  List.exists (fun c -> c = expected) categs

let check_categ ~msg (tokens : tokens) str expected =
  let categs = categs_of_string tokens str in
  if not (has_categ expected categs) then
    Alcotest.failf "%s: expected %s for %S, got [%s]"
      msg
      (show_category expected)
      str
      (categs |> List_.map show_category |> String.concat "; ")

let check_entity ~msg (tokens : tokens) str kind =
  let categs = categs_of_string tokens str in
  if not (has_entity kind categs) then
    Alcotest.failf "%s: expected Entity %s for %S, got [%s]"
      msg
      (E.show_kind kind)
      str
      (categs |> List_.map show_category |> String.concat "; ")

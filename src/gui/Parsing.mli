(*s: Parsing.mli *)

(* internally memoize the parsing part in _hmemo_file *)
val tokens_with_categ_of_file:
  string (* filename *) ->
  (string, Database_code.entity) Hashtbl.t ->
  (string * Highlight_code.category option * Pos.linecol) list

(* helpers *)
val use_arity_of_use_count: 
  int -> Highlight_code.use_arity

(*e: Parsing.mli *)

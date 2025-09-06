
val color_of_source_archi: 
  Archi_code.source_archi -> Simple_color.emacs_color

val color_of_webpl_type:
  FType.webpl_type -> Simple_color.emacs_color

val anamorphic_diviser_of_file:
  root:string ->
  string (* filename *) -> float

(* default treemap *)
val code_treemap: 
  filter_file: (Fpath.t -> bool) ->
  string (* path *) list ->
  (string, string (* filename *) * int) Treemap.treemap

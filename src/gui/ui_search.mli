(*s: ui_search.mli *)

val dialog_search_def: 
  Model.model Async.t -> string option

val run_grep_query:
  root:string -> string -> (string * Model.line) list

val run_tbgs_query:
  root:string -> string -> (string * Model.line) list

(*e: ui_search.mli *)

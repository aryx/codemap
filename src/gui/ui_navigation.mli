(*s: ui_navigation.mli *)

val go_back: 
  Model.world -> unit

val go_dirs_or_file:
  ?current_grep_query:(string (* filename *), Model.line) Hashtbl.t option ->
  Model.world -> string (* filename *) list -> unit

(*e: ui_navigation.mli *)

(*s: ui_navigation.mli *)

val go_back: 
  Model2.world -> unit

val go_dirs_or_file:
  ?current_grep_query:(string (* filename *), Model2.line) Hashtbl.t option ->
  Model2.world -> string (* filename *) list -> unit

(*e: ui_navigation.mli *)

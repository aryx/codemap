(*s: Completion.mli *)
(*s: signature [[my_entry_completion_eff]] *)

val my_entry_completion_eff :
  callback_selected:
   (GEdit.entry -> string -> string -> Database_code.entity -> bool) ->
  callback_changed:(string -> unit) -> 
  (unit -> Big_grep.index) ->
  GEdit.entry
(*e: signature [[my_entry_completion_eff]] *)

(*s: signature [[build_completion_defs_index]] *)
val build_completion_defs_index : 
  Database_code.entity list -> Big_grep.index
(*e: signature [[build_completion_defs_index]] *)
(*e: Completion.mli *)

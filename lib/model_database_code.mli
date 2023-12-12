(*s: model_database_code.mli *)

(*s: hentities sig *)
val hentities :
  string (* filename *) -> Database_code.database option -> 
  (string, Database_code.entity) Hashtbl.t
(*e: hentities sig *)

(*s: [[hfiles_and_top_entities]] sig *)
val hfiles_and_top_entities :
  string (* filename *) -> Database_code.database option -> 
  (string (* filename *), Database_code.entity list) Hashtbl.t
(*e: [[hfiles_and_top_entities]] sig *)

(*s: [[all_entities]] sig *)
(* Will generate extra entities for files, dirs, and also generate
 * an extra entity when have a fullname that is not empty
 *)
val all_entities :
  root:string (* filename *) -> string (* filename *) list -> Database_code.database option->
  Database_code.entity list
(*e: [[all_entities]] sig *)

(*s: [[actual_root_of_db]] sig *)
val actual_root_of_db : 
  root:string (* filename *) -> Database_code.database -> string
(*e: [[actual_root_of_db]] sig *)

(*s: [[readable_to_absolute_filename_under_root]] sig *)
val readable_to_absolute_filename_under_root :
  root:string (* filename *) -> string -> string
(*e: [[readable_to_absolute_filename_under_root]] sig *)

(*e: model_database_code.mli *)

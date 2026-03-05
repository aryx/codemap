(*
   Shared helpers for highlighting tests.
*)

type tokens = (string * Highlight_code.category option * Pos.linecol) list

val highlight_file : string -> tokens

val categs_of_string : tokens -> string -> Highlight_code.category list
val has_entity : Entity_code.kind -> Highlight_code.category list -> bool
val has_categ : Highlight_code.category -> Highlight_code.category list -> bool

val check_categ :
  msg:string -> tokens -> string -> Highlight_code.category -> unit
val check_entity :
  msg:string -> tokens -> string -> Entity_code.kind -> unit

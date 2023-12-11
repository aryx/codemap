type org_line =
  | Header of int * string (* full string, including starting stars *)
  | Comment of string
  | Other of string

type org = org_line list

val parse : string (* filename *) -> org

val highlight :
  org -> (string * Highlight_code.category option * Pos.linecol) list

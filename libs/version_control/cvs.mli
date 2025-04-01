(* could also use git-cvs to convert to git, but not always practical,
 * for instance would need get full CVS root for freebsd *)

val annotate : 
  ?basedir:string -> string (* filename *) -> Lib_vcs.line_annotation array
val date_file_creation: 
  ?basedir:string -> string (* filename *) -> Common2_.date_dmy

val annotate_raw : 
  ?basedir:string -> string (* filename *) -> string array

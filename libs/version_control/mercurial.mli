(* filename below is assumed to be the path of the file relative to basedir *)

(* operations on a singular file *)

val annotate : 
  ?basedir:string -> string (* filename *) -> Lib_vcs.line_annotation array
val date_file_creation: 
  ?basedir:string -> string (* filename *) -> Common2_.date_dmy
val annotate_raw : 
  ?basedir:string -> string (* filename *) -> string array

(* repository operations *)

val grep:
  basedir:string -> string -> string (* filename *) list
(* returns a temporary file containing the content of filename at versionid *)
val show:
  basedir:string -> string (* filename *) -> Lib_vcs.versionid -> Fpath.t

val files_involved_in_diff:
  basedir:string -> Lib_vcs.versionid -> 
  (Lib_vcs.file_commit_status * string (* filename *)) list

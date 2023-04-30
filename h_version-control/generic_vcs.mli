
class type vcs = object
method basedir: string

method grep: string -> Common.filename list
method show: Common.filename -> Lib_vcs.versionid -> Common.filename
method files_involved_in_diff: Lib_vcs.versionid -> 
  (Lib_vcs.file_commit_status * Common.filename) list
end 

val git: basedir:Common.filename -> vcs
val hg: basedir:Common.filename -> vcs

(* infer from basedir *)
val mk_vcs: basedir:Common.filename -> vcs


class type vcs = object
method basedir: string

method grep: string -> string (* filename *) list
method show: string (* filename *) -> Lib_vcs.versionid -> Fpath.t
method files_involved_in_diff: Lib_vcs.versionid -> 
                               (Lib_vcs.file_commit_status * string (* filename *)) list
end 

val git: basedir:string -> vcs
val hg: basedir:string -> vcs

(* infer from basedir *)
val mk_vcs: basedir:string -> vcs


val gen_age_layer:
  < Cap.readdir; .. > ->
  (* ?verbose:bool -> *)
  line_granularity: bool ->
  skip_revs:Lib_vcs.versionid list ->
  Common2.path -> output:string (* filename *) -> unit

val gen_nbauthors_layer: 
  < Cap.readdir; .. > ->
  (* ?verbose:bool -> *)
  skip_revs:Lib_vcs.versionid list ->
  string (* path *) -> output:string (* filename *) -> unit

val actions : < Cap.readdir; .. > -> Arg_.cmdline_actions

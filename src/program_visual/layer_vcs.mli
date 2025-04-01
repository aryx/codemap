
val gen_age_layer:
  (* ?verbose:bool -> *)
  line_granularity: bool ->
  skip_revs:Lib_vcs.versionid list ->
  Common2_.path -> output:string (* filename *) -> unit

val gen_nbauthors_layer: 
  (* ?verbose:bool -> *)
  skip_revs:Lib_vcs.versionid list ->
  string (* path *) -> output:string (* filename *) -> unit

val actions : unit -> Arg_.cmdline_actions

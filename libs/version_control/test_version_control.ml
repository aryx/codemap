(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_git dir = 
  let xs = Git_.commits ~basedir:dir () in
  xs |> List.iter UCommon.pr2_gen


(* ------------------------------------------------------------------------ *)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-test_git", "   <dir>", 
  Arg_.mk_action_1_arg test_git;

  "-test_git2", "   <dir> <str>", 
  Arg_.mk_action_2_arg (fun basedir str ->
    let vid = 
      (* Git.commit_of_relative_time ~basedir str  *)
      Lib_vcs.VersionId str
    in
    UCommon.pr2_gen vid;
    let patch = Git_.commit_patch ~basedir vid in
    UCommon.pr2_gen patch;
  );
  "-test_git3", "<dir>", 
  Arg_.mk_action_1_arg (fun basedir ->

    (* pr2 (Git_.parent_path_with_dotgit basedir); *)

    let old_id = Git_.commit_of_relative_time ~basedir "2 weeks ago" in
    let recent_id = Git_.commit_of_relative_time ~basedir "1 week ago" in
    let commits = Git_.commits_between_commitids ~basedir ~old_id ~recent_id in
    UCommon.pr2_gen old_id;
    UCommon.pr2_gen recent_id;
    Common2_.pr2_xxxxxxxxxxxxxxxxx ();
    commits |> List.iter UCommon.pr2_gen;
  )
]

open Fpath_.Operators
  let xs = String_.split ~sep:"/" file in
  xxs |> List.rev |> List_.find_some (fun xs ->
    let dir = "/" ^ String.concat "/" xs in
      assert(Filename_.filesuffix file = ext);
  xs |> List_.exclude (fun s -> 
    let xs = UCmd.cmd_to_list cmd in
      xs |> List_.map_filter (fun s -> 
  let xs = UCmd.cmd_to_list cmd in
    xs |> List_.map_filter (fun s -> 
  let xs = UCmd.cmd_to_list cmd in
  let xs = UCmd.cmd_to_list cmd in
  let xs = UCmd.cmd_to_list cmd in
  UCmd.cmd_to_list cmd
(*
*)
  let tmpfile = UTmp.new_temp_file "git_show" ".cat" in
  let cmd = (spf "git show %s:%s > %s" str_commit file !!tmpfile) in
  let xs = UCmd.cmd_to_list cmd in
  let xs = UCmd.cmd_to_list cmd in
  let xs = UCmd.cmd_to_list cmd in
  let xs = UCmd.cmd_to_list cmd in
  let xs = UCmd.cmd_to_list cmd in
  let xs = UCmd.cmd_to_list cmd in
  let xs = UCmd.cmd_to_list cmd in
  commits |> List_.index_list_1 |> List.iter (fun (vid, cnt) ->
  let basedir = Unix.realpath repo in
  commits |> (* Console.progress (fun k -> *) List.filter (fun (id, _x) ->
    (* k (); *)
    let xs = UCmd.cmd_to_list cmd in
  )
  (* TODO: use UTmp *)
  UFile.Legacy.with_open_outfile tmpfile (fun (xpr, _chan) ->
      UCommon.pr2_gen (id, s);
      xpr (spf "%s %s\n" (Lib_vcs.s_of_versionid id) s);
  file |> UFile.Legacy.cat |> List.map (fun s ->
  let tmpfile = UTmp.new_temp_file "git" ".patch" in
  UFile.write_file ~file:tmpfile s;
  let cmd = (goto_dir basedir ^ "git apply " ^ !!tmpfile ^ " 2>&1") in
  let xs = UCmd.cmd_to_list cmd in
  xs |> List.iter UCommon.pr2;
    lines_to_remove |> List_.map_filter (fun i ->
  let hblame = Hashtbl_.hashset_of_list toblame in
    annots |> Array.to_list |> List_.map_filter (fun x ->
  List_.take_safe 2 (counts @ counts')
    lines_to_remove |> List_.map_filter (fun i -> 
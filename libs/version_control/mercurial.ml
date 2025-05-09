(* Yoann Padioleau
 * 
 * Copyright (C) 2009, 2013 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common
open Fpath_.Operators
open Lib_vcs 
module Log = Log_vcs.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * less: could use a converter like maybe a git-hg if it exists?
 * 
 * history: 
 *  - done for aComment to study the history of repo in under mercurial.
 *  - extended for cmf --deadcode
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* less: factorize with Git.exec_cmd *)
let exec_cmd ~basedir s =
  let cmd = Lib_vcs.goto_dir basedir^ s in
  Log.info (fun m -> m "executing: %s" s);
  let ret = Sys.command cmd in
  if (ret <> 0) 
  then failwith ("pb with command: " ^ s)

(*****************************************************************************)
(* Commands *)
(*****************************************************************************)
(* ex: 
  stevel 7785c3469156 Thu Aug 30 00:13:38 2007 -0400: /*
*)

let annotate_regexp = 
  "^" ^ "[ \t]*\\([^ \t]+\\)[ \t]+" ^
    "\\([A-Za-z0-9]+\\)[ \t]+" ^
    "[A-Za-z]+" ^ "[ \t]+" ^
    "\\([A-Za-z]+\\)" ^ "[ \t]+" ^ (* month *)
    "\\([0-9]+\\)" ^ "[ \t]" ^ (* day *)
    "[0-9]+" ^ ":" ^
    "[0-9]+" ^ ":" ^
    "[0-9]+" ^ "[ \t]" ^ 
    "\\([0-9]+\\)" ^ "[ \t]" ^ "-" ^ (* year *)
    ".*" (* rest of line *)

let annotate2 ?(basedir="") filename = 
  (* 
     can add -f to follow rename and copy, in that case maybe add to add
      it to hg log too
     can add -u for user
  *)
  let cmd = (goto_dir basedir ^ "hg annotate -u -f -c -d "^filename^" 2>&1") in
  (* pr2 cmd; *)
  let xs = UCmd.cmd_to_list cmd in
  (*let ys = Common.cat (Common.filename_of_db (basedir,filename)) in*)

  let annots = 
    xs |> List_.filter_map (fun s -> 
      if s =~ annotate_regexp 
      then 
        let (author, commitid, month_str, day, year) = Common.matched5 s in
        Some (VersionId commitid, 
              Author author,
              Common2_.mk_date_dmy 
                (s_to_i day) 
                (Common2_.int_of_month (Common2_.month_of_string month_str))
                (s_to_i year))
      else begin 
        Log.warn (fun m -> m "hg annotate wrong line: %s" s);
        None
      end
    ) 
  in
  (* files lines are 1_based, so add this dummy 0 entry *)
  Array.of_list (dummy_annotation::annots)

let annotate ?basedir a = 
  Profiling.profile_code "Hg.annotate" (fun () -> annotate2 ?basedir a)

(* ------------------------------------------------------------------------ *)

let annotate_raw ?(basedir="") filename = 
  let cmd = (goto_dir basedir ^ "hg annotate -u -f -c -d "^filename^" 2>&1") in
  let xs = UCmd.cmd_to_list cmd in

  let annots = 
    xs |> List_.filter_map (fun s -> 
      if s =~ annotate_regexp 
      then 
        Some s
      else begin 
        (* pr2 ("hg annotate wrong line: " ^ s); *)
        None
      end
    ) 
  in
  Array.of_list (""::annots)



(* ------------------------------------------------------------------------ *)
(* ex:
date:        Thu Aug 30 00:13:38 2007 -0400
*)

let date_regexp = 
  "date:[ \t]+" ^
    "[A-Za-z]+" ^ "[ \t]+" ^
    "\\([A-Za-z]+\\)" ^ "[ \t]+" ^ (* month *)
    "\\([0-9]+\\)" ^ "[ \t]" ^ (* day *)
    "[0-9]+" ^ ":" ^
    "[0-9]+" ^ ":" ^
    "[0-9]+" ^ "[ \t]" ^ 
    "\\([0-9]+\\)" ^ "[ \t]" ^ "-" ^ (* year *)
    ".*" (* rest of line *)

let date_file_creation2 ?(basedir="") file = 

  let cmd = (goto_dir basedir ^ 
             "hg log -f "^file^" 2>&1")
  in
  let xs = UCmd.cmd_to_list cmd in
  let xs = List.rev xs in   (* could also hg log ... | tac *)
  
  xs |> List_.find_some (fun s -> 
    if s =~ date_regexp
    then 
      let (month_str, day, year) = Common.matched3 s in
      Some (Common2_.mk_date_dmy 
               (s_to_i day) 
               (Common2_.int_of_month (Common2_.month_of_string month_str))
               (s_to_i year))
    else None
  )


let date_file_creation ?basedir a = 
  Profiling.profile_code "Hg.date_file" (fun() -> date_file_creation2 ?basedir a)

(*****************************************************************************)
(* Repository operations *)
(*****************************************************************************)

let grep ~basedir str =
  let cmd = (goto_dir basedir ^
            (spf "hg locate -0 | xargs -0 grep --files-with-matches %s" str)) 
  in
  UCmd.cmd_to_list cmd
(*
  let (xs, status) = Common2_.cmd_to_list_and_status cmd in
  (* According to grep man page, non-zero exit code is expected when
   * there are no matches.
   * According to xargs man page, it returns 123 if one of his subcommand
   * returns something between 1 and 125
   *)
  match xs, status with
  | [], Unix.WEXITED n when n > 0 -> []
  | xs, Unix.WEXITED 0 
  | xs, Unix.WEXITED 123
    -> xs
  | _ -> 
    raise (UCmd.CmdError (status, (spf "CMD = %s, RESULT = %s" cmd
                                (Dumper.dump (status, xs)))))
*)
 

let show ~basedir file commitid =
  let tmpfile = UTmp.new_temp_file ~prefix:"hg_cat" ~suffix:".cat" () in
  let str_commit = Lib_vcs.s_of_versionid commitid in
  let cmd = spf "hg cat -r '%s' %s > %s" str_commit file !!tmpfile in
  exec_cmd ~basedir cmd;
  tmpfile

let files_involved_in_diff ~basedir commitid =
  let str_commit = Lib_vcs.s_of_versionid commitid in
  let cmd = goto_dir basedir ^
    spf "hg status --change '%s'" str_commit in
  let xs = UCmd.cmd_to_list cmd in
  xs |> List.map Lib_vcs.parse_file_status


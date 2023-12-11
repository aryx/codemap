
open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(*
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let verbose = ref false

(* action mode *)
let action = ref ""

let version = "0.3"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action _xs =
  raise Todo

(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)



(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
 Test_version_control.actions () @
 []

let options () = 
  [
    "-verbose", Arg.Set verbose, 
    " ";
  ] @
  Arg_.options_of_actions action (all_actions()) @
  Common2.cmdline_flags_devel () @
  Common2.cmdline_flags_verbose () @
  Common2.cmdline_flags_other () @
  [
  "-version",   Arg.Unit (fun () -> 
    pr2 (spf "vcs version: %s" version);
    exit 0;
  ), 
    "  guess what";

  (* this can not be factorized in Common *)
  "-date",   Arg.Unit (fun () -> 
    pr2 "version: $Date: 2008/10/26 00:44:57 $";
    raise (Common2.UnixExit 0)
    ), 
  "   guess what";
  ] @
  []

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 
  let usage_msg = 
    "Usage: " ^ Filename.basename Sys.argv.(0) ^ 
      " [options] <file or dir> " ^ "\n" ^ "Options are:"
  in
  (* does side effect on many global flags *)
  let args = Arg_.parse_options (options()) usage_msg Sys.argv in

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Profiling.profile_code "Main total" (fun () -> 

    (match args with
   
    (* --------------------------------------------------------- *)
    (* actions, useful to debug subpart *)
    (* --------------------------------------------------------- *)
    | xs when List.mem !action (Arg_.action_list (all_actions())) -> 
        Arg_.do_action !action xs (all_actions())

    | _ when not (String_.empty !action) -> 
        failwith ("unrecognized action or wrong params: " ^ !action)

    (* --------------------------------------------------------- *)
    (* main entry *)
    (* --------------------------------------------------------- *)
    | x::xs -> 
        main_action (x::xs)

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] -> 
        Arg_.usage usage_msg (options()); 
        failwith "too few arguments"
    )
  )

(*****************************************************************************)
let _ =
  UCommon.main_boilerplate (fun () -> 
      main ();
  )


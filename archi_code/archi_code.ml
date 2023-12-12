(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Categorizing a source file according to recurring architecture "aspects"
 * (really a directory structure) of a project. We often have some tests/,
 * some commons/ library, some include/, etc.
 *
 * A file may belong to multiple categories at once.
 *
 * Right now the "aspects" are slightly modeled according to my
 * own code and facebook flib code.
 *
 * This is used by codemap to colorize files. This is also used
 * mainly for its AutoGenerated category in pfff -test_loc to
 * not count auto generated code in the LOC of a project. This
 * can also be used in the deadcode detector to not count auto
 * generated files (e.g. visitor_xxx.ml) as real users of an entity.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* coupling: if add category, dont forget to extend the source_archi_list
 * below
 *)
type source_archi =
  | Main
  | Init
  | Interface
  (* I put Test and Logging together because if some dirs do not have some
   * unit tests, but have some code to logs his action, then it's quite
   * similar. Such code should be more robust and it's good to see it
   * visually.
   *)
  | Test
  | Logging
  | Core
  | Utils (* utils base common *)
  | Constants
  | GetSet (* mutators, accessors *)
  | Configuration (* settings *)
  | Building (* makefiles *)
  | Data (* big files *)
  | Doc
  | Ui (* ui render display *)
  | Storage (* storage db  *)
  | Parsing (* scanner, parser *)
  | Security
  | I18n
    (* todo?
     * Memory (e.g. malloc, buffer), Fonts (font, charset)
     * IO (e.g. keyboard, mouse)
     * Strings (e.g. regex
     *)
  | Architecture (* e.g. x86 *)
  | OS (* e.g. win32, macos, unix *)
  | Network (* e.g. protocols ssh, ftp *)
  | Ffi
  | ThirdParty (* external *)
  | Legacy (* legacy, deprecated *)
  | AutoGenerated
  | BoilerPlate
  (* a project often contains itself some infrastructure to run tests or
   * benchmarks.
   *)
  | Unittester
  | Profiler
  | MiniLite
  | Intern
  | Script
  | Regular
(* with tarzan *)

let source_archi_list =
  [
    Main;
    Init;
    Interface;
    Test;
    Logging;
    Core;
    Utils;
    Configuration;
    Building;
    Doc;
    Data;
    Constants;
    GetSet;
    Ui;
    Storage;
    Parsing;
    Security;
    I18n;
    Architecture;
    OS;
    Network;
    Script;
    ThirdParty;
    Legacy;
    Ffi;
    AutoGenerated;
    BoilerPlate;
    Unittester;
    Profiler;
    MiniLite;
    Intern;
    Regular;
  ]

type source_kind = Header | Source

(*****************************************************************************)
(* String of *)
(*****************************************************************************)

(* ocamltarzan generated *)
let s_of_source_archi = function
  | Init -> "Init"
  | Main -> "Main"
  | Interface -> "Interface"
  | AutoGenerated -> "AutoGenerated"
  | BoilerPlate -> "BoilerPlate"
  | Test -> "Test"
  | Logging -> "Logging"
  | Core -> "Core"
  | Utils -> "Utils"
  | Constants -> "Constants"
  | Script -> "Script"
  | Ffi -> "Ffi"
  | Configuration -> "Configuration"
  | Building -> "Building"
  | GetSet -> "GetSet"
  | Ui -> "Ui"
  | Storage -> "Storage"
  | Parsing -> "Parsing"
  | ThirdParty -> "ThirdParty"
  | Legacy -> "Legacy"
  | Unittester -> "Unittester"
  | Profiler -> "Profiler"
  | Intern -> "Intern"
  | Regular -> "Regular"
  | Doc -> "Doc"
  | Data -> "Data"
  | MiniLite -> "MiniLite"
  | Security -> "Security"
  | I18n -> "I18n"
  | Architecture -> "Architecture"
  | OS -> "OS"
  | Network -> "Network"

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(* TODO move this elsewhere *)
let find_duplicate_dirname dir =
  let h = Hashtbl.create 101 in
  let dups = Common2.hash_with_default (fun () -> 0) in

  let rec aux path =
    let subdirs = Common2.readdir_to_dir_list path |> List.sort compare in

    subdirs
    |> List.iter (fun dir ->
           let path = Filename.concat path dir in

           if Hashtbl.mem h dir then (
             UCommon.pr2
               (spf "duplicate dir for %s already there: %s" dir
                  (Hashtbl.find h dir));
             dups#update dir (fun old -> old + 1))
           else Hashtbl.add h dir path;
           aux path)
  in
  aux dir;
  UCommon.pr2 "duplicate are:";
  dups#to_list |> Assoc.sort_by_val_highfirst
  |> List.iter (fun (dir, cnt) -> UCommon.pr2 (spf " %s: %d" dir cnt));
  ()

(*****************************************************************************)
(* actions *)
(*****************************************************************************)

(*
let actions () = [
  "-test_dup_dir", "<dir>",
  Common.mk_action_1_arg (find_duplicate_dirname);
]
*)

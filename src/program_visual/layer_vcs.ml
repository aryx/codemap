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
module FT = File_type

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* 
 * todo? not sure how to transform those strings below in types so that more
 * checking is done at compile-time. We could define some AuthorMoreThan20
 * constructors but we will need to write some boilerplate code
 * to transform those constructors into strings (which are used
 * for the "legend" menu in codemap and the pfff-web UI).
 * Moreover there will be no guarentees that functions like
 * property_of_nb_authors covers the whole spectrum of constructors.
 * One good thing we would get is that in the layer generation
 * we could not generate property that do not exist (right now
 * it's easy to make a typo in the string and the compiler will not
 * complain).
 *)


let properties_nb_authors = [
  "authors > 40", "MediumPurple";
  "authors > 20", "red3";
  "authors > 10", "red1";
  "authors > 5", "orange";
  "authors = 5", "yellow";
  "authors = 4", "YellowGreen";
  "authors = 3", "green";
  "authors = 2", "aquamarine3";
  "authors = 1", "cyan";

  (* empty files *)
  "authors = 0", "white";
]

let properties_age = [
  "age > 5 years", "blue";
  "age > 3 years", "DeepSkyBlue1";
  "age > 1 year",  "cyan";
  "age > 6 months", "aquamarine3";
  "age > 3 months", "green";
  "age > 2 months", "YellowGreen";
  "age > 1 month", "yellow";
  "age > 2 weeks", "orange";
  "age > 1 week", "red1";
  "age last week", "red3";

  (* empty files *)
  "no info", "white";
]

let property_of_nb_authors n =
  match n with
  | _ when n > 40 -> "authors > 40"
  | _ when n > 20 -> "authors > 20"
  | _ when n > 10 -> "authors > 10"
  | _ when n > 5 -> "authors > 5"
  | _ when n <= 5 -> spf "authors = %d" n
  | _ -> raise Impossible

let property_of_age (Common2_.Days n) =
  match n with
  | _ when n > 5 * 365 -> "age > 5 years"
  | _ when n > 3 * 365 -> "age > 3 years"
  | _ when n > 1 * 365 -> "age > 1 year"
  | _ when n > 6 * 30 -> "age > 6 months"
  | _ when n > 3 * 30 -> "age > 3 months"
  | _ when n > 2 * 30 -> "age > 2 months"
  | _ when n > 1 * 30 -> "age > 1 month"
  | _ when n > 2 * 7 -> "age > 2 weeks"
  | _ when n > 1 * 7 -> "age > 1 week"
  | _ -> "age last week"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let filter_files xs =
  xs |> List.filter (fun f -> 
    (* filter the cached annotation files generated by Git.annotate below *)
    not (f =~ ".*.git_annot$") &&
    (match FT.file_type_of_file (Fpath.v f) with
    | FT.PL _ | FT.Text _ -> true
    | _ -> false
    )
  )
  
(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let gen_nbauthors_layer (*?(verbose=false)*) ~skip_revs dir ~output =
  let dir = Unix.realpath dir in
  let hskip_revs = Hashtbl_.hashset_of_list skip_revs in

  let caps = Cap.readdir_UNSAFE() in
  let files = UFile.Legacy.files_of_dirs_or_files_no_vcs_nofilter caps [dir] in
  let files = filter_files files in

  let layer = { Layer_code.
     title = "Number of authors";
     description = "Use information from git blame";           
     files = files |> (*Console.progress ~show:verbose (fun k -> *)
      List.map (fun file ->
        (*k();*)
        let readable_file = (Filename_.readable ~root:(dir) (file)) in
        
        let annots = Git_.annotate 
          ~basedir:dir ~use_cache:true 
          ~use_dash_C:false (* too slow *)
          readable_file
        in
        let nbauthors = 
          annots |> Array.to_list 
          (* don't count the first entry which is the annotation for line 0
           * which is a dummy value. See git.ml
           *)
          |> List.tl 
          |> List_.filter_map (fun (version, Lib_vcs.Author s, _data) ->
            if Hashtbl.mem hskip_revs version
            then None
            else Some s
          )
          |> Common2.uniq
          |> List.length
        in
        let property = property_of_nb_authors nbauthors in

        readable_file,
        { Layer_code.
          (* don't display anything at the line microlevel *)
          micro_level = [];

          macro_level = [property, 1.];
        }
      );
      kinds = properties_nb_authors;
  }
  in
  UCommon.pr2 ("generating layer in " ^ output);
  Layer_code.save_layer layer output


let gen_age_layer (*?(verbose=false)*) ~line_granularity ~skip_revs dir ~output =
  let dir = Unix.realpath dir in
  let hskip_revs = Hashtbl_.hashset_of_list skip_revs in

  let caps = Cap.readdir_UNSAFE () in
  let files = UFile.Legacy.files_of_dirs_or_files_no_vcs_nofilter caps [dir] in
  (* filter the cached annotation files generated by Git.annotate below *)
  let files = filter_files files in

  let layer = { Layer_code.
     title = "Age of code";
     description = "Use information from git blame";
     files = files |> (*Console.progress ~show:verbose (fun k -> *)
      List.map (fun file ->
        (*k();*)
        let readable_file = (Filename_.readable ~root:(dir) (file)) in
        
        let annots = Git_.annotate 
          ~basedir:dir ~use_cache:true 
          ~use_dash_C:false (* too slow *)
          readable_file
        in

        let annots = 
          annots |> Array.to_list 
           (* don't count the first entry which is the annotation for
            * line 0 which is a dummy value. See git.ml
            *)
           |> List.tl 
           |> List_.exclude (fun (version, Lib_vcs.Author _, _date_dmy) ->
             Hashtbl.mem hskip_revs version
           )
        in
        let now_dmy = 
          Common2_.today () 
          |> Common2_.floattime_to_unixtime |> Common2_.unixtime_to_dmy 
        in

        let max_age = 
          match annots with
          | [] -> "no info"
          | xs ->
           (* could also decide to use the average date of the file instead *)
           let max_date_dmy =
            xs
            |> List.map (fun (_version, Lib_vcs.Author _, date_dmy) -> date_dmy)
            |> Common2_.maximum_dmy
           in
           let age_in_days = 
             Common2_.rough_days_between_dates max_date_dmy now_dmy
           in
           UCommon.pr2_gen max_date_dmy;
           UCommon.pr2_gen age_in_days;

           property_of_age age_in_days
        in

        readable_file,
        { Layer_code.
          micro_level = 
            if line_granularity then
            annots |> List_.index_list_1 |> List.map
              (fun ((_version, Lib_vcs.Author _, date_dmy), i) -> 
                let age_in_days = 
                  Common2_.rough_days_between_dates date_dmy now_dmy
                in
                i, property_of_age age_in_days
              )
            else [];

          macro_level = [max_age, 1.];
        }
      );
      kinds = properties_age;
  }
  in
  UCommon.pr2 ("generating layer in " ^ output);
  Layer_code.save_layer layer output


(*****************************************************************************)
(* Actions *)
(*****************************************************************************)

let actions () = [
  "-gen_nbauthors_layer", " <git dir> <layerfile>",
  Arg_.mk_action_2_arg (fun dir output -> 
    gen_nbauthors_layer ~skip_revs:[] dir ~output);
  "-gen_age_layer", " <git dir> <layerfile>",
  Arg_.mk_action_2_arg (fun dir output -> 
    gen_age_layer ~line_granularity:false ~skip_revs:[] dir ~output);
  "-gen_age_lines_layer", " <git dir> <layerfile>",
  Arg_.mk_action_2_arg (fun dir output -> 
    gen_age_layer ~line_granularity:true ~skip_revs:[] dir ~output);
]

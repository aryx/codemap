(*s: Main.ml *)
(*
 * Please imagine a long and boring gnu-style copyright notice 
 * appearing just here.
 *)
open Common
open Fpath_.Operators
module Flag = Flag_visual
module FT = File_type

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This is the main entry point of codemap, a semantic source code visualizer
 * using treemaps and code thumbnails. The focus here is code understanding
 * not editing, so for instance even if features like autocompletion are
 * great for editing, they are not really helpful for understanding an existing
 * codebase. What can help is completion to help navigate and go from one
 * place to another, and this is one of the feature of this tool.
 * 
 * requirements:
 *  - get a bird's eye view of all the code (hence treemaps)
 *  - get a bird's eye view of a file (hence code thumbnails)
 *  - better syntax highlighting than Emacs, using real parsers so
 *    we can colorize differently identifiers (a function vs a field vs
 *    a constant)
 *  - important code should be bigger. Just like in google maps
 *    the important roads are more visible. So need some sort of
 *    global semantic analysis.
 *  - show the data (the source code), but also show the relations
 *    (hence codegraph integration)
 *  - look at the code through different views (hence layers)
 *  - SEMI deep semantic analysis, control flow + data flow so can answer
 *    flow questions such as where a callback is called (hence datalog)
 * 
 * history:
 *  - saw Aspect Browser while working on aspects as an intern at IRISA
 *  - work on Poffs and idea of visualizing the same code through 
 *    different views
 *  - talked about mixing sgrep/spatch with code visualization,
 *    highlighting with a certain color different architecture aspects
 *    of the Linux kernel (influenced by work on aspect browser)
 *  - talked about fancy code visualizer while at cleanmake with YY,
 *    Spiros, etc.
 *  - saw SeeSoft code visualizer while doing some bibliographic work
 *  - saw code thumbnails by MSR, and Rob Deline
 *  - saw treemap of Linux kernel by Fekete => idea of mixing
 *    tree-map+code-thumbnails+seesoft = codemap
 *  - saw talk at CC'09 about improving javadoc by putting in bigger fonts
 *    really often used API functions => idea of light db and semantic
 *    visual feedback
 *  - read hierarchical edge bundling paper and its d3 implementation to 
 *    visualize on top of a treemap the call graph
 * 
 * related work:
 *  - racket IDE (was called DrScheme before), had arrows long time ago
 *    between occurences of a variable and its definition
 *  - http://peaker.github.io/lamdu/, but focused more on AST pretty printing
 *  - light table, interesting visualization slice but now focused more
 *    on live programming a la Bret Victor
 *  - http://www.kickstarter.com/projects/296054304/zeta-code, mostly focused
 *    on code relations, so related more to codegraph
 *  - textmate, nice fuzzy file find, very quick to go to a place
 *  - sublime, has thumbnails, but people don't really care about it
 *  - http://www.hello2morrow.com/products/sotoarc ?
 *  - http://scg.unibe.ch/codemap ?
 *  - http://scg.unibe.ch/wiki/projects/rbcrawler, class blueprint, very nice
 *    when exploring tangled object code abusing inheritance
 *  - moose http://youtu.be/yvXm9LC17vk at 14min
 *  - http://redotheweb.com/CodeFlower/ ?
 *  - code swarm, visualize git history, focused on people more than code
 *    https://code.google.com/p/gource/ 
 *    http://artzub.com/ghv/#repo=d3&climit=100&user=mbostock
 *  - http://www.codetrails.com/ctrlflow, smarter completion by infering
 *    importance of method (like I do, by #times this entity is globally used)
 *  - codesonar, very nice interactive zoomable call graph visualizer
 *    https://www.youtube.com/watch?v=EqDhtRoorGU also very deep semantic analysis
 *    with control flow and data flow
 *  - CodeCompass https://github.com/Ericsson/CodeCompass/
 *  - Sourcetrail, recently open sourced
 *    https://www.sourcetrail.com/blog/open_source/
 * 
 * features of IDE we do want (e.g. see the list at http://xamarin.com/studio):
 *  - smart syntax highlighting (we do even more: semantic highlighting!)
 *  - go to definition (=~ TAGS, light db and search bar completion provides it)
 *  - code navigation (directory, files, also "hypertext" go to def/uses)
 *  - find uses (funcs, classes, TODO tricky for methods in dynamic languages)
 *  - code tooltip, hover on use of an entity to display information about
 *    it (#uses, TODO: type/args, comments, code, age, methods, etc)
 *  - unified "fuzzy" search (files, entities, TODO but also content),
 *    SEMI project-wide fuzzy search is really useful for filenames but also content!
 *  - debugger? it helps understand code so a coverage layer or TODO live
 *    coverage tracing would be nice (as in tracegl),
 *    also a way to see the actual concrete values of variables/parameters
 *    inline would be nice too (as in intellij 14) (always-on visualization!)
 *  - source control? can extract age, number of authors, churn information in
 *    layers
 *  - SEMI dataflow from here, dataflow to here (intellij)
 * 
 * features of IDE we care less about:
 *  - folding/outline? thumbnails make this less important
 *  - auto completion? One nice thing of autocomplete is that
 *    it proposes all the possible methods of an object, the overriden
 *    as well as not overriden parent methods. We don't need autocomplete
 *    but we want the ability to understand a class by TODO "inlining" parent 
 *    methods that are relevant to understand the local code of the class
 *    (e.g. the short command of Eiffel)
 *  - code snippet? This is similar to auto completion, it's good for writing
 *    new code, but not that useful to understand existing code.
 *  - refactoring? no (but some spatch integration could be nice)
 *  - UI designer? no
 *  - deploy assistant, cloud assistant? no
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(*s: main flags *)
(* on macOS lion, X11 resizes the window to a smaller size so
 * there is no point in starting with a big screen_size :(
 *)
let screen_size = ref 1
let legend = ref false

(* you can also put this in your ~/gtkrc-2.0
 *  gtk-icon-theme-name = "Tango"
 *  gtk-theme-name = "Murrine-Gray"
 *  gtk-font-name = "DejaVu Sans 16"
 * or not put anything and use the default Gtk theme.
 *)

(* if not specified, codemap will try to use files in the current directory *)
let graph_file = ref (None: string (* filename *) option)
let db_file    = ref (None: string (* filename *) option)
let layer_file = ref (None: string (* filename *) option)
let layer_dir  = ref (None: string (* filename *) option)

(* See also Gui.synchronous_actions *)
let test_mode = ref (None: string option)
(*e: main flags *)

(* see filters below, which filter files we are interested in *)
let filter = ref (fun _file -> true)
(* less: a config file: GtkMain.Rc.add_default_file "/.../pfff_browser.rc"; *)

(* for -debug, -verbose, -quiet *)
let logs_level = ref (Some Logs.Warning)

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* File targeting *)
(*****************************************************************************)

let filters = [
  (* pad-specific: semgrep related files *)
  "semgrep", (fun file ->
    match FT.file_type_of_file file with
    | FT.PL (FT.OCaml _ | FT.Python | FT.Web (FT.Js | FT.TypeScript))
    | FT.PL (FT.IDL _)
    | FT.PL (FT.Script _)
    | FT.Config (FT.Makefile | FT.Dockerfile | FT.Jsonnet | FT.Yaml | FT.Sexp) -> 
            true
    | _ -> false
  );
  (* pad-specific: *)
  "xix", (fun file ->
    match FT.file_type_of_file file with
    | FT.PL ((FT.OCaml _) | (FT.C _ | FT.Asm)) | FT.Config FT.Makefile -> true
    | _ -> false
  );

  "ocaml", (fun file ->
    match File_type.file_type_of_file file with
    | FT.PL (FT.OCaml _) | FT.Config (FT.Makefile)  -> true
    | _ -> false
  );
  "mli", (fun file ->
    match File_type.file_type_of_file file with
    | FT.PL (FT.OCaml "mli") | FT.Config (FT.Makefile)   -> 
      not (!!file =~ ".*/commons/")
    | _ -> false
  );
  "nw", (fun file -> 
    match FT.file_type_of_file file with
    | FT.Text "nw" -> true | _ -> false
  );
  "doc", (fun file -> 
    match FT.file_type_of_file file with
    | FT.Text _ -> true | _ -> false
  );

  (* other languages *)
  "php", (fun file ->
    match File_type.file_type_of_file file with
    | FT.PL (FT.Web (FT.Php _)) -> true  | _ -> false
  );
  "js", (fun file ->
    match File_type.file_type_of_file file with
    | FT.PL (FT.Web (FT.Js)) -> true  | _ -> false
  );
  "config", (fun file ->
    match File_type.file_type_of_file file with
    | FT.Config (FT.Yaml | FT.Json) -> true  | _ -> false
  );

  "cpp", (let x = ref false in (fun file ->
    Common2_.once x (fun () -> 
      (* TODO: also add possible pfff_macros.h when there *)
      Parse_cpp.init_defs !Flag_parsing_cpp.macros_h
    );
    match FT.file_type_of_file file with
    | FT.PL (FT.C _ | FT.Cplusplus _) -> true 
    | FT.PL FT.Asm -> true
    | _ -> false
  ));

  (* general categories *)
  "pl", (fun file ->
    match File_type.file_type_of_file file with
    | FT.PL _ -> true  | _ -> false
  );
]

let mk_filter_file (root : Fpath.t) : (Fpath.t -> bool) =
  let gitignore_filter =
    Gitignore_filter.create
      ~gitignore_filenames:[
      Gitignore.{source_kind = "gitignore"; filename = ".gitignore"; format = Gitignore };
      Gitignore.{source_kind = "codemapignore"; filename = ".codemapignore"; format = Gitignore };
      ]
    ~project_root:root ()
  in
  (fun file ->
     !filter file &&
     let ppath =
        match Ppath.in_project ~root:(Rfpath.of_fpath_exn root) (Rfpath.of_fpath_exn file) with
        | Ok ppath -> ppath
        | Error err ->
              failwith (spf "could not find project path for %s with root = %s (errot = %s)"
                !!file !!root err)
     in
     let (status, _events) =
       Gitignore_filter.select gitignore_filter ppath
     in
     status =*= Gitignore.Not_ignored
    )

(*****************************************************************************)
(* Model helpers *)
(*****************************************************************************)

(*s: [[treemap_generator]] *)
(* this is called each time we go in a new directory (or set of dirs) *)
let treemap_generator ~filter_file = 
 fun paths ->
  let treemap = Treemap_pl.code_treemap ~filter_file paths in
  let algo = Treemap.Ordered Treemap.PivotByMiddle in
  let big_borders = !Flag.boost_label_size in
  let rects = Treemap.render_treemap ~algo ~big_borders treemap in
  Logs.debug (fun m -> m "%d rectangles to draw" (List.length rects));
  rects
(*e: [[treemap_generator]] *)

(*s: function [[build_model]] *)
(* this is currently called in the background *)
let build_model root dbfile_opt graphfile_opt =   

  let db_opt = dbfile_opt |> Option.map Database_code.load_database in
  let caps = Cap.readdir_UNSAFE() in
  (* TODO: use List_files *)
  let files = 
    UFile.Legacy.files_of_dirs_or_files_no_vcs_nofilter caps [root] 
    |> List.filter (fun file -> !filter (Fpath.v file))
  in
  let hentities = Model_database_code.hentities root db_opt in
  let all_entities = Model_database_code.all_entities ~root files db_opt in
  let big_grep_idx = Completion.build_completion_defs_index all_entities in

  let g_opt = graphfile_opt |> Option.map Graph_code.load in
  let hfile_deps_of_node, hentities_of_file =
    match g_opt with
    | None -> Hashtbl.create 0, Hashtbl.create 0
    | Some g ->
      let a = Model_graph_code.build_filedeps_of_dir_or_file g in
      let b = Model_graph_code.build_entities_of_file g in
      let b = Model_graph_code.add_headers_files_entities_of_file root b in
      a, Hashtbl_.hash_of_list b
  in
  
  let model = { Model.
        root = root;
        db = db_opt;
        hentities; big_grep_idx;
        g =  g_opt;
        hfile_deps_of_node; hentities_of_file;
  }
  in
  model
[@@profiling]
(*e: function [[build_model]] *)

(* could also to parse all json files and filter the one which do not parse *)
let layers_in_dir dir =
  Common2_.readdir_to_file_list dir |> List_.filter_map (fun file ->
    if file =~ "layer.*json"
    then Some (Filename.concat dir file)
    else None
  )

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

(*s: function [[main_action]] *)
let main_action xs = 
  (* this used to be done by linking with gtkInit.cmo, but better like this *)
  let _locale = GtkMain.Main.init () in

  (* TODO: remove, accept just a single root and rely on .codemapignore
   * for advanced customization? or nice to accept multiple so
   * can compare size of different projects together easily?
   * TODO: use Fpath_.common_parent at least
   *)
  let root = Common2_.common_prefix_of_files_or_dirs xs in
  Logs.info (fun m -> m "Using root = %s" root);

  let filter_file = mk_filter_file (Fpath.v root) in

  let async_model = Async.async_make () in

  let layers = 
    match !layer_file, !layer_dir, xs with
    | Some file, _, _ -> 
        [Layer_code.load_layer file]
    | None, Some dir, _ | None, None, [dir] ->
        layers_in_dir dir |> List.map Layer_code.load_layer
    | _ -> []
  in
  let layers_with_index = 
    Layer_code.build_index_of_layers ~root 
      (match !layer_file, layers with 
      | Some _, [layer] -> [layer, true]
      | _ -> layers |> List.map (fun x -> x, false)
      )
  in

  let db_file = 
    match !db_file, xs with
    | Some file, _ -> Some file
    | None, [dir] ->
      let candidates = [
          Filename.concat dir Database_code.default_db_name;
          Filename.concat dir Database_code.default_db_name ^ ".json";
      ] in
      (try 
        Some (candidates |> List.find (fun file -> Sys.file_exists file))
      with Not_found -> None
      )
      | _ -> None
  in
  db_file |> Option.iter (fun db -> 
    Logs.info (fun m -> m "Using pfff light db: %s" db)
  );
  let graph_file : Fpath.t option = 
    match !graph_file, xs with
    | Some file, _ -> Some (Fpath.v file)
    | None, [dir] ->
      let candidates = [
          Filename.concat dir !!Graph_code.default_filename;
      ] in
      (try 
         let file = candidates |> List.find (fun file -> Sys.file_exists file) in
         Some (Fpath.v file)
      with Not_found -> None
      )
    | _ -> None
  in
  graph_file |> Option.iter (fun db -> 
    Logs.info (fun m -> m "Using graphcode: %s" !!db)
  );

  let treemap_func = treemap_generator ~filter_file in
  let dw = Model.init_drawing  treemap_func layers_with_index xs root in

  (* This can require lots of stack. Make sure to have ulimit -s 40000 *)
  Thread.create (fun () ->
    (* heavy computation are not *fairly* scheduled apparently by the OCaml
     * runtime, so let's do the heavy computation in another process
     * and here just have the thread waiting for it to be done.
     * This thread used to cause some Bus error on MacOS but now that
     * we use invoke and do the job in another process things seem better :)
     *)
    let job () = build_model root db_file graph_file in
    let res = Parallel.invoke job () () in
    Async.async_set res async_model;
  ) ()
  |> ignore;
 
  let w = { Model.
    dw;
    dw_stack = ref [dw];
    model = async_model;
    treemap_func;
    current_node = None;
    current_node_selected = None;
    current_entity = None;
    settings = { Model.
      (* todo: too fuzzy for now *)
      draw_summary = false;
      draw_searched_rectangles = true;
    };
    root_orig = root;
  }
  in

  View.mk_gui  ~screen_size:!screen_size ~legend:!legend !test_mode w
(*e: function [[main_action]] *)
  
(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

(* related work: http://cloc.sourceforge.net/ but have skip list
 * and archi_code_lexer.mll which lower the important of some files?
 *)
let test_loc print_top30 xs =
  let xs = xs |> List.map Unix.realpath in
  let root = Common2_.common_prefix_of_files_or_dirs xs in

  let filter_file = mk_filter_file (Fpath.v root) in
  let treemap = Treemap_pl.code_treemap ~filter_file xs in

  let res = ref [] in
  let rec aux tree =
    match tree with
    | Common2_.Node (_dir, xs) ->
        List.iter aux xs
    | Common2_.Leaf (leaf, _) ->
        let file = leaf.Treemap.label in
        let size = leaf.Treemap.size in
        let unix_size = (Common2_.unix_stat_eff file).Unix.st_size in
        if unix_size > 0
        then begin
          let multiplier = (float_of_int size /. float_of_int unix_size) in
          let multiplier = min multiplier 1.0 in
          let loc = Common2_.nblines_with_wc file in
          Stack_.push (((Filename_.readable ~root:(root) (file))), 
                       (float_of_int loc *. multiplier)) res;
        end
  in
  aux treemap;
  let total = !res |> List.map snd |> List.map int_of_float  |> Common2_.sum in
  UCommon.pr2 (spf "LOC = %d (%d files)" total (List.length !res));
  if print_top30 then begin
    let topx = 30 in
    UCommon.pr2 (spf "Top %d:" topx);
    !res |> Assoc.sort_by_val_highfirst |> List_.take_safe topx 
    |>  List.iter (fun (file, f) ->
      UCommon.pr2 (spf "%-40s: %d" file (int_of_float f))
    )
  end


let test_treemap_dirs () =
  let paths = 
    ["commons/common.ml"; "h_visualization"; "code_graph"] 
    |> List.map Unix.realpath in
  let paths = List.sort String.compare paths in
  let tree = 
    paths |> Treemap.tree_of_dirs_or_files
      ~filter_dir:Lib_vcs.filter_vcs_dir
      ~filter_file:(fun file -> !!file =~ ".*\\.ml")
      ~file_hook:(fun _file -> 10)
  in
  UCommon.pr2_gen tree


(* update: try to put ocamlgtk related tests in widgets/test_widgets.ml, not
 * here. Here it's for ... well it's for nothing I think because it's not 
 * really easy to test a gui.
 *)

(*s: [[visual_commitid]]() action *)
let test_visual_commitid id =
  let files = UCmd.cmd_to_list
    (spf "git show --pretty=\"format:\" --name-only %s"
        id) 
    (* not sure why git adds an extra empty line at the beginning but we
     * have to filter it
     *)
    |> List_.exclude String_.empty
  in
  UCommon.pr2_gen files;
  main_action files
(*e: [[visual_commitid]]() action *)

let width = 500
let height = 500

let test_draw cr =
  (* [0,0][1,1] world scaled to a width x height screen *)
  Cairo.scale cr (float_of_int width) (float_of_int height);

  Cairo.set_source_rgba cr 0.5 0.5 0.5   0.5;
  Cairo.set_line_width cr 0.001;

  Cairo.move_to cr 0.5 0.5;
  Cairo.line_to cr 0.6 0.6;
  Cairo.stroke cr;

  Cairo.select_font_face cr "serif" ~weight:Cairo.Bold;
  Cairo.set_font_size cr 0.1;
  Cairo.move_to cr 0.1 0.1;
  Cairo.show_text cr "THIS IS SOME TEXT";
  Cairo.move_to cr 0.1 0.2;
  Cairo.show_text cr "THIS IS SOME TEXT";
  Cairo.set_font_size cr 0.05;
  Cairo.move_to cr 0.1 0.3;
  Cairo.show_text cr "THIS IS SOME TEXT";

  Cairo.set_source_rgb cr 0.1 0.1 0.1;
  Cairo.move_to cr 0.1 0.1;
  Cairo.line_to cr 0.1 0.2;
  Cairo.stroke cr;

  let start = ref 0.0 in

  for _i = 0 to 3 do
    let end_ = !start +. 0.5 in
    Cairo.arc cr 0.5 0.5 ~r:0.3 ~a1:!start ~a2:end_;
    Cairo.stroke cr;
    start := end_;
  done;

  ()

let test_cairo () =
  let _locale = GtkMain.Main.init () in
  let w = GWindow.window ~title:"test" () in
  (w#connect#destroy GMain.quit) |> ignore;
  let px = GDraw.pixmap ~width ~height ~window:w () in
  px#set_foreground `WHITE;
  px#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
  let cr = Cairo_gtk.create px#pixmap in
  test_draw cr;
  (GMisc.pixmap px ~packing:w#add ()) |> ignore;
  w#show ();
  GMain.main()
  
(*---------------------------------------------------------------------------*)
(* the command line flags *)
(*---------------------------------------------------------------------------*)
let extra_actions () = [
 (*s: actions *)
   "-test_loc", " ",
   Arg_.mk_action_n_arg (test_loc true);
   "-test_loc2", " ",
   Arg_.mk_action_n_arg (test_loc false);
   "-test_cairo", " ",
   Arg_.mk_action_0_arg (test_cairo);
   "-test_commitid", " <id>",
   Arg_.mk_action_1_arg (test_visual_commitid);
   "-test_treemap_dirs", " <id>",
   Arg_.mk_action_0_arg (test_treemap_dirs);
 (*e: actions *)
]
 
(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
 extra_actions()@
 []

let options () = ([ 
  (*s: options *)
    "-screen_size", Arg.Set_int screen_size,
    " <int> (1 = small, 2 = big)";
    "-legend", Arg.Set legend,
    " display the legend";
    "-no_legend", Arg.Clear legend,
    " do not display the legend";

    "-symlinks", Arg.Unit (fun () -> Treemap.follow_symlinks := true;),
    " follow symlinks";
    "-no_symlinks", Arg.Unit (fun () -> Treemap.follow_symlinks := false),
    " do not follow symlinks";

    "-with_graph", Arg.String (fun s -> graph_file := Some s),
    " <graph_file> dependency semantic information";
    "-with_db", Arg.String (fun s -> db_file := Some s),
    " <db_file> generic semantic information";
    "-with_layer", Arg.String (fun s -> layer_file := Some s),
    " <layer_file>";
    "-with_layers", Arg.String (fun s -> layer_dir := Some s),
    " <dir_with_layers>";
    "-filter", Arg.String (fun s -> filter := List.assoc s filters;), 
     spf " filter certain files (available = %s)" 
      (filters |> List.map fst |> String.concat ", ");
    "-extra_filter", Arg.String (fun s -> Flag.extra_filter := Some s),
    " ";

    "-ft", Arg.Set_float Flag.threshold_draw_content_font_size_real,
    " <float> threshold to draw content";
    "-nblines_file", Arg.Set_float Flag.threshold_draw_content_nblines,
    " <float>";
    "-boost_lbl", Arg.Set Flag.boost_label_size,
    " boost size of labels";
    "-emacs_client", Arg.Set_string Editor_connection.emacsclient_path,
    " <path>";
    "-efuns_client", Arg.Set_string Editor_connection.efunsclient_path,
    " <path>";

  (*-------------------------------------------------------------------------*)
  (* debugging helpers *)
  (*-------------------------------------------------------------------------*)

    "-verbose", Arg.Unit (fun () -> logs_level := Some Logs.Info),
    " ";
    "-debug", Arg.Unit (fun () -> logs_level := Some Logs.Debug),
    " ";
    "-quiet", Arg.Unit (fun () -> logs_level := None),
    " ";
    "-debug_handlers", Arg.Set Gui.synchronous_actions,
    " ";
    "-disable_fonts", Arg.Set Flag.disable_fonts,
    " ";

    "-test", Arg.String (fun s -> test_mode := Some s),
    " <str> execute an internal script";
  (*e: options *)
  ] @
  Arg_.options_of_actions action (all_actions()) @
  Common2_.cmdline_flags_devel () @
  [
  "-version",   Arg.Unit (fun () -> 
    UCommon.pr2 (spf "CodeMap version: %s" "TODO: version codemap");
    exit 0;
  ), 
    " guess what";
  ]) |> Arg.align

(*****************************************************************************)
(* The main entry point *)
(*****************************************************************************)
let main () = 

  let usage_msg = 
    spf "Usage: %s [options] <file or dir> \nDoc: %s\nOptions:"
      (Filename.basename Sys.argv.(0))
      "https://github.com/facebook/pfff/wiki/Codemap"
  in
  (* alt: use cmdliner and parse --debug, --info ... *)
  let args = Arg_.parse_options (options()) usage_msg Sys.argv in

  Logs_.setup ~level:!logs_level ();
  Logs.info (fun m -> m "Starting logging");
    
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
    | (x::xs) -> 
        main_action (x::xs)

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] -> Arg.usage (options()) usage_msg; 
    );
  )

(*****************************************************************************)
let _ = 
  UCommon.main_boilerplate (fun () ->
    main ()
  )
(*e: Main.ml *)

(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
 * Copyright (C) 2024 Semgrep Inc.
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
module E = Entity_code
module G = Graphe
module Log = Log_graph_code.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A "code database" data structure.
 *
 * A program can be seen as a hierarchy of entities
 * (directory/package/module/file/class/function/method/field/...)
 * linked to each other through different mechanisms
 * (import/reference/extend/implement/instantiate/call/access/...).
 * This module is the basis for 'codegraph', a tool to help
 * visualize code dependencies or code relationships
 * See https://github.com/aryx/codegraph for more info.
 *
 * This module provides one of the core data structure of codegraph:
 * an (hyper)graph of all the entities in a program linked
 * either via a 'has-a' relation, which represent the
 * hierarchies (in the sense of containment, not inheritance), or
 * 'use-a', which represent the dependencies
 * (the other core data structure of codegraph is in
 * Dependencies_matrix_code.ml).
 *
 * history: Is this yet another code database in pfff at Facebook? For PHP
 * we already have database_php.ml, tags_php.ml, database_light_php.ml,
 * and now even a Prolog database, ... that's a lot of code database.
 * They all have things in common, but by focusing here on one thing,
 * by just having a single graph, it's then
 * easier to reason and implement certain features.
 * I could have probably done the DSM (dependency structure matrix) using
 * database_php.ml, but it was not made for that. Here the graph is
 * the core and simplest data structure that is needed.
 *
 * This graph also unifies many things. For instance there is no
 * special code to handle directories or files, they are
 * just considered regular entities like module or classes
 * and can have sub-entities. Moreover like database_light.ml,
 * this file is language independent so one can have one tool
 * that can handle ML, PHP, C++, etc.
 *
 * history and related work:
 *  - "CIA, the C Information Abstraction System" (1990)
 *    https://www.proquest.com/docview/195583113
 *  - database_code_php.ml, database_light_php.ml, tags_php.ml (2010)
 *    and a few other lang-specific "code databases" I did at Facebook
 *  - Graph_code (this module) and codegraph (the tool around it) (2012)
 *    inspired by CIA and https://www.ndepend.com/ dependency structure matrix
 *  - Grok by Steve Yegge at Google
 *    http://www.youtube.com/watch?v=KTJs-0EInW8 (2012)
 *    update: he later joined sourcegraph (in 2023)
 *  - Kythe at Google (an open source descendent of Grok) (2018?)
 *    https://kythe.io/ which probably defines its own code database data
 *    structure
 *  - Glean at Facebook (2020?)
 *    (not sure if was influenced by my own prolog database at Facebook)
 *    https://glean.software/docs/introduction/
 *  - Stackgraph by github, inspired by Scope graph by Elco Visser (2021?)
 *  - LSIF by sourcegraph and Microsoft (inspired by LSP) (2022?)
 *    https://microsoft.github.io/language-server-protocol/specifications/lsif/0.4.0/specification/
 *  - SCIP by sourcegraph, an evolution of LSIF (2023?)
 *    which even have its formal protobuf spec:
 *    https://github.com/sourcegraph/scip/blob/main/scip.proto
 *
 * TODO:
 *  - We probably want to imitate more SCIP. It looks like a pretty
 *    good spec, very complete, and at least export to SCIP or
 *    import from SCIP.
 *
 *  - how to handle duplicate entities (e.g. we can have two different
 *    files with the same module name, or two functions with the same
 *    name but one in a library and the other in a script).
 *    prepend a ___number suffix?
 *    Or just have one node with multiple parents :) But having
 *    multiple parents would not solve the problem because then
 *    an edge will increment unrelated cells in the DSM.
 *
 *  - change API to allow by default to automatically create nodes
 *    when create edges with unexisting nodes? After all graphviz
 *    allow to specify graphs like this, which shorten graph
 *    description significantly. Can still have a
 *    add_edge_throw_exn_if_not_present for the cases where we
 *    want extra security.
 *
 *  - maybe I can generate the light database from this graph_code.ml
 *    (I already do a bit for prolog with graph_code_prolog.ml)
 *
 *  - opti: faster implem of parent? have a lock_graph() that forbid any
 *    further modifications on Has but then provide optimized operations
 *    like parent the precompute or memoize the parent relation
 *
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type node = entity_name * E.kind

(* TODO: at some point we might want a 'string list' here, to better represent
 * entities like "Package1.Subpackage.FooClass.BarMethod" ?
 * TODO: or even better, switch to the SCIP "symbol" syntax
 * see https://github.com/sourcegraph/scip/blob/v0.3.3/scip.proto#L147
 * with symbol (entity_name) like "com/example/MyClass#myMethod(+1)."
 *)
and entity_name = string

(* TODO: SCIP supports more precise edges with its "Occurence" and "SymbolRole"
 * at https://github.com/sourcegraph/scip/blob/v0.3.3/scip.proto#L500
 *)
type edge =
  (* a package Has subpackages, a subpackage Has classes, a class Has members,
   * etc *)
  | Has
  (* A class Use(extends) another class, a method Use(calls) another method,
   * etc.
   * todo? refine by having different cases? Use of `Call|`Extend|...?
   * I didn't feel the need yet, because it's easy to know if it's
   * a Call or Extend by looking at the src and dst of the edge.
   * But it could be useful for instance for field access to know
   * weather it's a read or write access! Instead of having a variant
   * here one could also have an edgeinfo.
   *)
  | Use

type nodeinfo = {
  pos : Tok.location;
  props : E.property list;
  (* would be better to have a more structured form than string at some point *)
  typ : string option;
  (* This is the SCIP symbol, a unique identifier assigned to each name in
     a SCIP-indexed project: https://github.com/sourcegraph/scip
     Example: "scip-typescript npm example-ts 1.0.0 `index.ts`/bar()."
  *)
  scip_symbol : string option;
  range : (Pos.linecol * Pos.linecol) option;
}

(* could also have a pos: and props: here *)
type edgeinfo = { write : bool; read : bool }

(*
 * We use an imperative, directed, without intermediate node-index, graph.
 *
 * We use two different graphs because we need an efficient way to
 * go up in the hierarchy to increment cells in the dependency matrix
 * so it's better to separate the two usages.
 *
 * note: file information are in readable path format in Dir and File
 * nodes (and should also be in readable format in the nodeinfo).
 *)
type t = {
  (* Actually the Has graph should really be a tree, but we need convenient
   * access to the children or parent of a node, which are provided
   * by the graph API so let's reuse that.
   *)
  has : node G.graph;
  (* The source and target should be enough information to understand
   * the kind of Use. For instance a class referencing another class
   * has to be an 'extends'. A class referencing an Interface has to
   * be an 'implements'.
   *)
  use : node G.graph;
  nodeinfo : (node, nodeinfo) Hashtbl.t;
  edgeinfo : (node * node * edge, edgeinfo) Hashtbl.t;
}

type error = NodeAlreadyPresent of node

exception Error of error

(* coupling: see print_statistics below *)
type statistics = {
  parse_errors : Fpath.t list ref;
  (* could be Parse_info.token_location*)
  lookup_fail : (Tok.t * node) list ref;
  method_calls : (Tok.t * resolved) list ref;
  field_access : (Tok.t * resolved) list ref;
  unresolved_class_access : Tok.t list ref;
  unresolved_calls : Tok.t list ref;
}

and resolved = bool

let empty_statistics () =
  {
    parse_errors = ref [];
    lookup_fail = ref [];
    method_calls = ref [];
    unresolved_calls = ref [];
    unresolved_class_access = ref [];
    field_access = ref [];
  }

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)
let root = (".", E.Dir)
let pb = ("PB", E.Dir)
let not_found = ("NOT_FOUND", E.Dir)
let dupe = ("DUPE", E.Dir)
let _stdlib = ("STDLIB", E.Dir)

(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)

let string_of_node (s, kind) = E.string_of_entity_kind kind ^ ": " ^ s

let string_of_error = function
  | NodeAlreadyPresent n -> "Node already present: " ^ string_of_node n

let display_with_gv g =
  (* TODO? use different colors for the different kind of edges? *)
  G.display_with_gv g.has

(*****************************************************************************)
(* Graph construction *)
(*****************************************************************************)
let create () =
  {
    has = G.create ();
    use = G.create ();
    nodeinfo = Hashtbl.create 101;
    edgeinfo = Hashtbl.create 101;
  }

let add_node n g =
  if G.has_node n g.has then (
    Log.warn (fun m -> m "already present %s" (Dumper.dump n));
    raise (Error (NodeAlreadyPresent n)));
  if G.has_node n g.use then (
    Log.warn (fun m -> m "already present %s" (Dumper.dump n));
    raise (Error (NodeAlreadyPresent n)));

  G.add_vertex_if_not_present n g.has;
  G.add_vertex_if_not_present n g.use;
  ()
[@@profiling]

let add_edge (n1, n2) e g =
  match e with
  | Has -> G.add_edge n1 n2 g.has
  | Use -> G.add_edge n1 n2 g.use
[@@profiling]

let remove_edge (n1, n2) e g =
  match e with
  | Has -> G.remove_edge n1 n2 g.has
  | Use -> G.remove_edge n1 n2 g.use

let add_nodeinfo n info g =
  if not (G.has_node n g.has) then failwith "unknown node";

  Hashtbl.replace g.nodeinfo n info

let add_edgeinfo (n1, n2) e info g = Hashtbl.replace g.edgeinfo (n1, n2, e) info

(*****************************************************************************)
(* IO *)
(*****************************************************************************)
(* todo: what when have a .opti? cache_computation will shortcut us *)
let version = 5

let save g file =
  Log.info (fun m -> m "saving %s" !!file);
  (* see ocamlgraph FAQ *)
  UMarshal_.write_value (g, !Graph.Blocks.cpt_vertex, version) file

let load file =
  Log.info (fun m -> m "loading %s" !!file);
  let g, serialized_cpt_vertex, version2 = UMarshal_.get_value file in
  if version <> version2 then
    failwith (spf "your marshalled file has an old version, delete it");
  Graph.Blocks.after_unserialization serialized_cpt_vertex;
  g

let default_filename = Fpath.v "graph_code.marshall"

(*****************************************************************************)
(* Iteration *)
(*****************************************************************************)
let iter_use_edges f g = G.iter_edges f g.use
let iter_has_edges f g = G.iter_edges f g.has
let iter_nodes f g = G.iter_nodes f g.has

let all_use_edges g =
  let res = ref [] in
  G.iter_edges (fun n1 n2 -> Stack_.push (n1, n2) res) g.use;
  !res

let all_has_edges g =
  let res = ref [] in
  G.iter_edges (fun n1 n2 -> Stack_.push (n1, n2) res) g.has;
  !res

let all_nodes g =
  let res = ref [] in
  G.iter_nodes (fun n -> Stack_.push n res) g.has;
  !res

(*****************************************************************************)
(* Graph access *)
(*****************************************************************************)

let has_node n g = G.has_node n g.has

let has_edge (n1, n2) e g =
  match e with
  | Has -> G.has_edge n1 n2 g.has
  | Use -> G.has_edge n1 n2 g.use

let pred n e g =
  match e with
  | Has -> G.pred n g.has
  | Use -> G.pred n g.use
[@@profiling]

let succ n e g =
  match e with
  | Has -> G.succ n g.has
  | Use -> G.succ n g.use

(* the default implementation of a graph in ocamlgraph is good to
 * get the successor but not good at all for the predecessors
 * so if you need to use pred many times, use this precomputation
 * function.
 *)
let mk_eff_use_pred g =
  (* we use its find_all property *)
  let h = Hashtbl.create 101 in

  g
  |> iter_nodes (fun n1 ->
         let uses = succ n1 Use g in
         uses |> List.iter (fun n2 -> Hashtbl_.push h n2 n1));
  fun n -> Hashtbl_.get_stack h n

let parent n g =
  match G.pred n g.has with
  | [ x ] -> x
  | [] -> failwith "parnet: node has no parent"
  | _ -> failwith "parent: pred returned multiple parents"
[@@profiling]

let parents n g = G.pred n g.has [@@profiling]
let children n g = G.succ n g.has

let rec node_and_all_children n g =
  let xs = G.succ n g.has in
  if List_.null xs then [ n ]
  else
    n :: (xs |> List_.map (fun n -> node_and_all_children n g) |> List_.flatten)

let nb_nodes g = G.nb_nodes g.has
let nb_use_edges g = G.nb_edges g.use
let nodeinfo n g = Hashtbl.find g.nodeinfo n

let nodeinfo_opt n g =
  try Some (nodeinfo n g) with
  | Not_found -> None

let edgeinfo_opt (n1, n2) e g =
  try Some (Hashtbl.find g.edgeinfo (n1, n2, e)) with
  | Not_found -> None

(* todo? assert it's a readable path? graph_code_php.ml is using readable
 * path now but the other might not yet or it can be sometimes convenient
 * also to have absolute path here, so not sure if can assert anything.
 *)
let file_of_node (n : node) (g : t) : Fpath.t =
  try
    let info = nodeinfo n g in
    info.pos.pos.file
  with
  | Not_found -> (
      match n with
      | str, E.File -> Fpath.v str
      | _ ->
          raise Not_found
          (* todo: BAD no? *)
          (* spf "NOT_FOUND_FILE (for node %s)" (string_of_node n) *))

let privacy_of_node n g =
  let info = nodeinfo n g in
  let props = info.props in
  props
  |> List_.find_some (function
       | E.Privacy x -> Some x
       | _ -> None)

(* see also Graph_code_class_analysis.class_method_of_string *)
let shortname_of_node (s, _kind) =
  let xs = String_.split ~sep:"[.]" s in
  let s = Common2.list_last xs in
  (* undo what was in gensym, otherwise codemap for instance will not
   * recognize the entity as one hovers on its name in a file. *)
  let s = if s =~ "\\(.*\\)__[0-9]+" then Common.matched1 s else s in
  let s =
    (* see graph_code_clang.ml handling of struct/typedef/unions *)
    if s =~ "^[STU]__\\(.*\\)" then
      (* assert (kind =*= E.Type);, hmm have constructor like T__AT *)
      Common.matched1 s
    else s
  in
  s

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let create_intermediate_directories_if_not_present g dir =
  let dirs = Common2.inits_of_relative_dir dir in

  let rec aux current xs =
    match xs with
    | [] -> ()
    | x :: xs ->
        let entity = (x, E.Dir) in
        if has_node entity g then aux entity xs
        else (
          g |> add_node entity;
          g |> add_edge (current, entity) Has;
          aux entity xs)
  in
  aux root dirs

let create_initial_hierarchy g =
  g |> add_node root;
  g |> add_node pb;
  g |> add_node not_found;
  g |> add_node dupe;
  (*  g +> add_node stdlib;*)
  g |> add_edge (root, pb) Has;
  g |> add_edge (pb, dupe) Has;
  g |> add_edge (pb, not_found) Has;
  (*  g +> add_edge (root, stdlib) Has;*)
  ()

let remove_empty_nodes g xs =
  let use_pred = mk_eff_use_pred g in
  xs
  |> List.iter (fun n ->
         if succ n Use g =*= [] && use_pred n =*= [] then
           (* less: could also remove the node? but slow? removing the edge
            * should be enough for what we want (avoid clutter in codegraph)
            *)
           remove_edge (parent n g, n) Has g)

let basename_to_readable_disambiguator (xs : string list) ~root =
  (* nosemgrep: no-filename-readable *)
  let xs = xs |> List_.map (fun x -> Filename_.readable ~root (x)) in
  let h = Hashtbl.create 101 in
  xs
  |> List.iter (fun file ->
         Hashtbl_.push h (Fpath.basename (Fpath.v file)) (file));
  fun file -> Hashtbl_.get_stack h file

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

let group_edges_by_files_edges xs g =
  xs
  |> Common2.group_by_mapped_key (fun (n1, n2) ->
         (file_of_node n1 g, file_of_node n2 g))
  |> List_.map (fun (x, deps) -> (List.length deps, (x, deps)))
  |> Assoc.sort_by_key_highfirst |> List_.map snd

(*****************************************************************************)
(* Graph algorithms *)
(*****************************************************************************)
let strongly_connected_components_use_graph g =
  let scc, hscc = G.strongly_connected_components g.use in
  (scc, hscc)

let top_down_numbering g =
  let scc, hscc = G.strongly_connected_components g.use in
  let g2 = G.strongly_connected_components_condensation g.use (scc, hscc) in
  let hdepth = G.depth_nodes g2 in

  let hres = Hashtbl.create 101 in
  hdepth
  |> Hashtbl.iter (fun k v ->
         let nodes_at_k = scc.(k) in
         nodes_at_k |> List.iter (fun n -> Hashtbl.add hres n v));
  hres

let bottom_up_numbering g =
  let scc, hscc = G.strongly_connected_components g.use in
  let g2 = G.strongly_connected_components_condensation g.use (scc, hscc) in
  let g3 = G.mirror g2 in
  let hdepth = G.depth_nodes g3 in

  let hres = Hashtbl.create 101 in
  hdepth
  |> Hashtbl.iter (fun k v ->
         let nodes_at_k = scc.(k) in
         nodes_at_k |> List.iter (fun n -> Hashtbl.add hres n v));
  hres

(*****************************************************************************)
(* Loading graph code from dot file *)
(*****************************************************************************)
(* assumes a "path/to/file.x" -> "path/to/file2.x" format *)
let graph_of_dotfile dotfile =
  let xs = UFile.cat dotfile in
  let deps =
    xs
    |> List_.filter_map (fun s ->
           if s =~ "^\"\\(.*\\)\" -> \"\\(.*\\)\"$" then
             let src, dst = Common.matched2 s in
             Some (src, dst)
           else (
             Log.info (fun m -> m "ignoring line: %s" s);
             None))
  in
  let g = create () in
  create_initial_hierarchy g;
  (* step1: defs *)
  deps
  |> List.iter (fun (src, dst) ->
         let srcdir = Filename.dirname src in
         let dstdir = Filename.dirname dst in
         try
           create_intermediate_directories_if_not_present g srcdir;
           create_intermediate_directories_if_not_present g dstdir;
           if not (has_node (src, E.File) g) then (
             g |> add_node (src, E.File);
             g |> add_edge ((srcdir, E.Dir), (src, E.File)) Has);
           if not (has_node (dst, E.File) g) then (
             g |> add_node (dst, E.File);
             g |> add_edge ((dstdir, E.Dir), (dst, E.File)) Has)
         with
         | Assert_failure _ ->
             Log.err (fun m -> m "assert failure: %s" (Dumper.dump (src, dst))));
  (* step2: use *)
  deps
  |> List.iter (fun (src, dst) ->
         let src_node = (src, E.File) in
         let dst_node = (dst, E.File) in

         g |> add_edge (src_node, dst_node) Use);
  g

(*****************************************************************************)
(* Statistics *)
(*****************************************************************************)
let string_of_statistics stats g =
  Buffer_.with_buffer_to_string (fun buf ->
      let prf fmt = Printf.bprintf buf fmt in
      prf "nb nodes = %d, nb edges = %d" (nb_nodes g) (nb_use_edges g);
      prf "parse errors = %d" (!(stats.parse_errors) |> List.length);
      prf "lookup fail = %d" (!(stats.lookup_fail) |> List.length);
      prf "unresolved method calls = %d"
        (!(stats.method_calls)
        |> List.filter (fun (_, x) -> not x)
        |> List.length);
      prf "(resolved method calls = %d)"
        (!(stats.method_calls) |> List.filter (fun (_, x) -> x) |> List.length);
      prf "unresolved field access = %d"
        (!(stats.field_access)
        |> List.filter (fun (_, x) -> not x)
        |> List.length);
      prf "(resolved field access) = %d)"
        (!(stats.field_access) |> List.filter (fun (_, x) -> x) |> List.length);
      prf "unresolved class access = %d"
        (!(stats.unresolved_class_access) |> List.length);
      prf "unresolved calls = %d" (!(stats.unresolved_calls) |> List.length))

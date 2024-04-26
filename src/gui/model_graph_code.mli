(*s: model_graph_code.mli *)
val build_filedeps_of_dir_or_file: 
  Graph_code.t ->
  (Graph_code.node, string (* filename *) list * string (* filename *) list) Hashtbl.t

(* the nodes are sorted by line numbers *)
val build_entities_of_file:
  Graph_code.t ->
  (string (* filename *), Graph_code.node list) Assoc.t

val add_headers_files_entities_of_file:
  string (* filename *) (* a dir *) -> 
  (string (* filename *), Graph_code.node list) Assoc.t ->
  (string (* filename *), Graph_code.node list) Assoc.t

val node_of_entity: 
  Database_code.entity -> Graph_code.t -> Graph_code.node option
(*e: model_graph_code.mli *)

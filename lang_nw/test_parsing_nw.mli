(* Print the set of tokens in a .nw file *)
val test_tokens_nw : string (* filename *) -> unit

(* This makes accessible the different test_xxx functions above from
 * the command line, e.g. '$ pfff -parse_nw foo.nw will call the
 * test_parse_nw function.
 *)
val actions : unit -> Arg_.cmdline_actions

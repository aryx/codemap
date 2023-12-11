(* Print the set of tokens in a .html file *)
val test_tokens_html : string (* filename *) -> unit

(* This makes accessible the different test_xxx functions above from
 * the command line, e.g. '$ pfff -parse_html foo.html will call the
 * test_parse_html function.
 *)
val actions : unit -> Arg_.cmdline_actions

let main (_caps : Cap.all_caps) : unit =
  Testo.interpret_argv ~project_name:"codemap" (fun _env ->
      List_.flatten [
        Unit_highlight_ocaml.tests;
        Unit_highlight_haskell.tests;
        Unit_highlight_go.tests;
        Unit_highlight_python.tests;
        Unit_highlight_java.tests;
        Unit_highlight_cpp.tests;
        Unit_highlight_js.tests;
        Unit_highlight_scala.tests;
        Unit_highlight_rust.tests;
        Unit_highlight_zig.tests;
        Unit_highlight_markdown.tests;
        Unit_highlight_yaml.tests;
      ])

let () = Cap.main (fun all_caps -> main all_caps)

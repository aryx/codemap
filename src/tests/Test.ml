let main (_caps : Cap.all_caps) : unit =
  Testo.interpret_argv ~project_name:"codemap" (fun _env ->
      List_.flatten [
        Unit_highlight_haskell.tests;
        Unit_highlight_ocaml.tests;
      ])

let () = Cap.main (fun all_caps -> main all_caps)

let main (_caps : Cap.all_caps) : unit =
  Testo.interpret_argv ~project_name:"codemap" (fun _env ->
      Unit_highlight_haskell.tests)

let () = Cap.main (fun all_caps -> main all_caps)

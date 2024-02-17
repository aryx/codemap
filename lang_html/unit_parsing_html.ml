open Ast_html

let t = Testo.create

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let tests =
  Testo.categorize "parsing_html"
    [
      t "lexing regular code" (fun () -> ());
      t "parsing regular code" (fun () -> ());
      t "html tree correctness"
        (fun () ->
          let s = "<div>a</div><div>b</div>" in
          let ast = Parse_html.html_tree_of_string s in
          match ast with
          | Element
              ( Tag ("__root__", _),
                [],
                [
                  Element (Tag ("div", _), [], _);
                  Element (Tag ("div", _), [], _);
                ] ) ->
              ()
          | _ ->
              Alcotest.failf "wrong ast for %s, got %s" s
                ((*Export_html.ml_pattern_string_of_html_tree*)
                 Dumper.dump ast) );
    ]

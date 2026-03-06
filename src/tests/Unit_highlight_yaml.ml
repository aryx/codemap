(*
   Unit tests for YAML highlighting in codemap.
*)
open Highlight_code
open Testutil_highlight

let t = Testo.create

let tests_path = "tests/yaml"

let highlight file =
  highlight_file (Filename.concat tests_path file)

let test_simple () =
  let tokens = highlight "simple.yaml" in

  (* keys are highlighted as fields *)
  check_entity ~msg:"key name" tokens "name" Entity_code.Field;
  check_entity ~msg:"key nested" tokens "nested" Entity_code.Field;

  (* string values *)
  check_categ ~msg:"string value hello" tokens "hello" String;
  check_categ ~msg:"string value foo" tokens "foo" String;

  (* numbers *)
  check_categ ~msg:"number value" tokens "1.0" Number;

  (* no ghost parens *)
  let has_paren = List.exists (fun (s, _, _) -> s = "(" || s = ")") tokens in
  Alcotest.(check bool) "no ghost parens" false has_paren;

  ()

let tests =
  Testo.categorize "Highlight_yaml"
    [
      t "simple.yaml" test_simple;
    ]

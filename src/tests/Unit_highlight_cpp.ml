(*
   Unit tests for C++ highlighting in codemap.
*)
open Highlight_code
open Testutil_highlight
module E = Entity_code

let t = Testo.create
let tests_path = "tests/cpp"
let highlight file = highlight_file (Filename.concat tests_path file)

let test_simple () =
  let tokens = highlight "simple.cpp" in

  (* preprocessor *)
  check_categ ~msg:"include directive" tokens "#include <iostream>" Include;

  (* keywords *)
  check_categ ~msg:"enum keyword" tokens "enum" Keyword;
  check_categ ~msg:"return keyword" tokens "return" Keyword;
  check_categ ~msg:"if keyword" tokens "if" KeywordConditional;

  (* function definition *)
  check_entity ~msg:"factorial is a function def" tokens "factorial" E.Function;
  check_entity ~msg:"main is a function def" tokens "main" E.Function;

  (* built-in type *)
  check_categ ~msg:"int is TypeInt" tokens "int" TypeInt;

  (* literals *)
  check_categ ~msg:"number literal" tokens "0" Number;
  check_categ ~msg:"number literal 42" tokens "42" Number;
  check_categ ~msg:"string literal" tokens "\"hello\"" String;
  check_categ ~msg:"boolean literal" tokens "true" Boolean;

  ()

let tests =
  Testo.categorize "Highlight_cpp"
    [
      t "simple.cpp" test_simple;
    ]

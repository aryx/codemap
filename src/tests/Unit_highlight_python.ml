(*
   Unit tests for Python highlighting in codemap.
*)
open Highlight_code
open Testutil_highlight
module E = Entity_code

let t = Testo.create
let tests_path = "tests/python"
let highlight file = highlight_file (Filename.concat tests_path file)

let test_simple () =
  let tokens = highlight "simple.py" in

  (* keywords *)
  check_categ ~msg:"import keyword" tokens "import" KeywordModule;
  check_categ ~msg:"class keyword" tokens "class" KeywordObject;
  check_categ ~msg:"def keyword" tokens "def" Keyword;
  check_categ ~msg:"return keyword" tokens "return" Keyword;
  check_categ ~msg:"if keyword" tokens "if" KeywordConditional;
  check_categ ~msg:"else keyword" tokens "else" KeywordConditional;

  (* class definition *)
  check_entity ~msg:"Color is a class def" tokens "Color" E.Class;

  (* class constants *)
  check_entity ~msg:"RED is a constant" tokens "RED" E.Constant;
  check_entity ~msg:"GREEN is a constant" tokens "GREEN" E.Constant;

  (* function definition and use *)
  check_entity ~msg:"factorial is a function" tokens "factorial" E.Function;
  check_entity ~msg:"print is a function" tokens "print" E.Function;

  (* parameter *)
  let n_categs = categs_of_string tokens "n" in
  Alcotest.(check bool) "n has Parameter Def"
    true (has_categ (Parameter Def) n_categs);
  Alcotest.(check bool) "n has Parameter Use"
    true (has_categ (Parameter Use) n_categs);

  (* literals *)
  check_categ ~msg:"string literal" tokens "\"hello\"" String;
  check_categ ~msg:"number literal" tokens "42" Number;
  check_categ ~msg:"boolean literal" tokens "True" Boolean;

  ()

let tests =
  Testo.categorize "Highlight_python"
    [
      t "simple.py" test_simple;
    ]

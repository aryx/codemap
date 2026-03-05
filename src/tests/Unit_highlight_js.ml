(*
   Unit tests for JavaScript highlighting in codemap.
*)
open Highlight_code
open Testutil_highlight
module E = Entity_code

let t = Testo.create
let tests_path = "tests/javascript"
let highlight file = highlight_file (Filename.concat tests_path file)

let test_simple () =
  let tokens = highlight "simple.js" in

  (* keywords *)
  check_categ ~msg:"import keyword" tokens "import" KeywordModule;
  check_categ ~msg:"from keyword" tokens "from" KeywordModule;
  check_categ ~msg:"class keyword" tokens "class" KeywordObject;
  check_categ ~msg:"constructor keyword" tokens "constructor" KeywordObject;
  check_categ ~msg:"this keyword" tokens "this" KeywordObject;
  check_categ ~msg:"function keyword" tokens "function" Keyword;
  check_categ ~msg:"const keyword" tokens "const" Keyword;
  check_categ ~msg:"return keyword" tokens "return" Keyword;
  check_categ ~msg:"if keyword" tokens "if" KeywordConditional;

  (* class definition *)
  check_entity ~msg:"Color is a class def" tokens "Color" E.Class;

  (* function definition and use *)
  check_entity ~msg:"factorial is a function" tokens "factorial" E.Function;

  (* parameter *)
  let n_categs = categs_of_string tokens "n" in
  Alcotest.(check bool) "n has Parameter Def"
    true (has_categ (Parameter Def) n_categs);
  Alcotest.(check bool) "n has Parameter Use"
    true (has_categ (Parameter Use) n_categs);

  (* field access *)
  check_entity ~msg:"name is a field" tokens "name" E.Field;

  (* method call *)
  check_entity ~msg:"log is a method" tokens "log" E.Method;

  (* constants *)
  check_entity ~msg:"x is a constant" tokens "x" E.Constant;

  (* literals *)
  check_categ ~msg:"string literal" tokens "'fs'" String;
  check_categ ~msg:"number literal" tokens "0" Number;
  check_categ ~msg:"number literal 42" tokens "42" Number;
  check_categ ~msg:"boolean literal" tokens "true" Boolean;

  ()

let tests =
  Testo.categorize "Highlight_js"
    [
      t "simple.js" test_simple;
    ]

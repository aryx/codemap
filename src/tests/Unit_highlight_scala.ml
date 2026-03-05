(*
   Unit tests for Scala highlighting in codemap.
*)
open Highlight_code
open Testutil_highlight
module E = Entity_code

let t = Testo.create
let tests_path = "tests/scala"
let highlight file = highlight_file (Filename.concat tests_path file)

let test_simple () =
  let tokens = highlight "simple.scala" in

  (* keywords *)
  check_categ ~msg:"package keyword" tokens "package" KeywordModule;
  check_categ ~msg:"import keyword" tokens "import" KeywordModule;
  check_categ ~msg:"sealed keyword" tokens "sealed" KeywordObject;
  check_categ ~msg:"trait keyword" tokens "trait" KeywordObject;
  check_categ ~msg:"object keyword" tokens "object" KeywordObject;
  check_categ ~msg:"extends keyword" tokens "extends" KeywordObject;
  check_categ ~msg:"def keyword" tokens "def" Keyword;
  check_categ ~msg:"val keyword" tokens "val" Keyword;
  check_categ ~msg:"if keyword" tokens "if" KeywordConditional;

  (* package def *)
  check_entity ~msg:"test is a package def" tokens "test" E.Package;

  (* class/object definition *)
  check_entity ~msg:"Color is a class def" tokens "Color" E.Class;
  check_entity ~msg:"Simple is a class def" tokens "Simple" E.Class;

  (* constructors *)
  check_entity ~msg:"Red is a constructor" tokens "Red" E.Constructor;
  check_entity ~msg:"Green is a constructor" tokens "Green" E.Constructor;
  check_entity ~msg:"Blue is a constructor" tokens "Blue" E.Constructor;

  (* function definition and use *)
  check_entity ~msg:"factorial is a function" tokens "factorial" E.Function;

  (* parameter *)
  let n_categs = categs_of_string tokens "n" in
  Alcotest.(check bool) "n has Parameter Def"
    true (has_categ (Parameter Def) n_categs);

  (* type *)
  check_entity ~msg:"Int is a type" tokens "Int" E.Type;

  (* literals *)
  check_categ ~msg:"string literal" tokens "\"hello\"" String;
  check_categ ~msg:"number literal" tokens "42" Number;
  check_categ ~msg:"boolean literal" tokens "true" Boolean;

  ()

let tests =
  Testo.categorize "Highlight_scala"
    [
      t "simple.scala" test_simple;
    ]

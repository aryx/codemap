(*
   Unit tests for Go highlighting in codemap.
*)
open Highlight_code
open Testutil_highlight
module E = Entity_code

let t = Testo.create
let tests_path = "tests/go"
let highlight file = highlight_file (Filename.concat tests_path file)

let test_simple () =
  let tokens = highlight "simple.go" in

  (* keywords *)
  check_categ ~msg:"package keyword" tokens "package" KeywordModule;
  check_categ ~msg:"import keyword" tokens "import" KeywordModule;
  check_categ ~msg:"func keyword" tokens "func" Keyword;
  check_categ ~msg:"type keyword" tokens "type" Keyword;
  check_categ ~msg:"const keyword" tokens "const" Keyword;
  check_categ ~msg:"return keyword" tokens "return" Keyword;
  check_categ ~msg:"if keyword" tokens "if" KeywordConditional;

  (* built-in type *)
  check_entity ~msg:"int is a type" tokens "int" E.Type;

  (* literals *)
  check_categ ~msg:"number literal" tokens "0" Number;
  check_categ ~msg:"string literal" tokens "\"fmt\"" String;

  ()

let tests =
  Testo.categorize "Highlight_go"
    [
      t "simple.go" test_simple;
    ]

(*
   Unit tests for Java highlighting in codemap.
*)
open Highlight_code
open Testutil_highlight
module E = Entity_code

let t = Testo.create
let tests_path = "tests/java"
let highlight file = highlight_file (Filename.concat tests_path file)

let test_simple () =
  let tokens = highlight "Simple.java" in

  (* keywords *)
  check_categ ~msg:"package keyword" tokens "package" KeywordModule;
  check_categ ~msg:"import keyword" tokens "import" KeywordModule;
  check_categ ~msg:"public keyword" tokens "public" KeywordObject;
  check_categ ~msg:"class keyword" tokens "class" KeywordObject;
  check_categ ~msg:"static keyword" tokens "static" Keyword;
  check_categ ~msg:"return keyword" tokens "return" Keyword;
  check_categ ~msg:"if keyword" tokens "if" KeywordConditional;

  (* built-in type *)
  check_categ ~msg:"int is TypeInt" tokens "int" TypeInt;

  (* literals *)
  check_categ ~msg:"number literal" tokens "0" Number;
  check_categ ~msg:"number literal 42" tokens "42" Number;
  check_categ ~msg:"string literal" tokens "\"hello\"" String;
  check_categ ~msg:"boolean literal" tokens "true" Boolean;

  ()

let tests =
  Testo.categorize "Highlight_java"
    [
      t "Simple.java" test_simple;
    ]

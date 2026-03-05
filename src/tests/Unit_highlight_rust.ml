(*
   Unit tests for Rust highlighting in codemap.
*)
open Highlight_code
open Testutil_highlight
module E = Entity_code

let t = Testo.create
let tests_path = "tests/rust"
let highlight file = highlight_file (Filename.concat tests_path file)

let test_simple () =
  let tokens = highlight "simple.rs" in

  (* keywords *)
  check_categ ~msg:"use keyword" tokens "use" KeywordModule;
  check_categ ~msg:"fn keyword" tokens "fn" Keyword;
  check_categ ~msg:"return keyword" tokens "return" Keyword;
  check_categ ~msg:"if keyword" tokens "if" KeywordConditional;

  (* type definition *)
  check_entity ~msg:"Color is a type def" tokens "Color" E.Type;

  (* function definition and use *)
  check_entity ~msg:"factorial is a function" tokens "factorial" E.Function;
  check_entity ~msg:"main is a function" tokens "main" E.Function;

  (* parameter *)
  check_categ ~msg:"n is a parameter" tokens "n" (Parameter Def);

  (* type *)
  check_entity ~msg:"u64 is a type" tokens "u64" E.Type;

  (* literals *)
  check_categ ~msg:"number literal" tokens "0" Number;
  check_categ ~msg:"number literal 42" tokens "42" Number;
  check_categ ~msg:"string content" tokens "hello" String;
  check_categ ~msg:"boolean literal" tokens "true" Boolean;

  ()

let tests =
  Testo.categorize "Highlight_rust"
    [
      t "simple.rs" test_simple;
    ]

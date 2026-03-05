(*
   Unit tests for Zig highlighting in codemap.
*)
open Highlight_code
open Testutil_highlight
module E = Entity_code

let t = Testo.create
let tests_path = "tests/zig"
let highlight file = highlight_file (Filename.concat tests_path file)

let test_simple () =
  let tokens = highlight "simple.zig" in

  (* keywords *)
  check_categ ~msg:"const keyword" tokens "const" Keyword;
  check_categ ~msg:"fn keyword" tokens "fn" Keyword;
  check_categ ~msg:"return keyword" tokens "return" Keyword;
  check_categ ~msg:"pub keyword" tokens "pub" Keyword;
  check_categ ~msg:"if keyword" tokens "if" KeywordConditional;
  check_categ ~msg:"enum keyword" tokens "enum" KeywordObject;
  check_categ ~msg:"struct keyword" tokens "struct" KeywordObject;

  (* function definition *)
  check_entity ~msg:"factorial is a function" tokens "factorial" E.Function;
  check_entity ~msg:"main is a function" tokens "main" E.Function;

  (* literals *)
  check_categ ~msg:"number literal" tokens "0" Number;
  check_categ ~msg:"number literal 42" tokens "42" Number;
  check_categ ~msg:"string content" tokens "\"hello\"" String;
  check_categ ~msg:"boolean literal" tokens "true" Boolean;

  ()

let tests =
  Testo.categorize "Highlight_zig"
    [
      t "simple.zig" test_simple;
    ]

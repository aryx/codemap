(*
   Unit tests for Haskell highlighting in codemap.
*)
open Highlight_code
open Testutil_highlight
module E = Entity_code

let t = Testo.create

let tests_path = "tests/haskell"

let highlight file =
  highlight_file (Filename.concat tests_path file)

let test_simple () =
  let tokens = highlight "simple.hs" in

  (* function definition: type signature produces FunctionDecl *)
  let factorial_categs = categs_of_string tokens "factorial" in
  Alcotest.(check bool) "factorial has FunctionDecl"
    true
    (has_categ (FunctionDecl NoUse) factorial_categs);

  (* recursive call is tagged as function use *)
  Alcotest.(check bool) "factorial has Function use"
    true (has_entity E.Function factorial_categs);

  (* type names in signatures *)
  check_entity ~msg:"Int is a type" tokens "Int" E.Type;

  (* data constructors *)
  check_entity ~msg:"Red is a constructor" tokens "Red" E.Constructor;
  check_entity ~msg:"Green is a constructor" tokens "Green" E.Constructor;
  check_entity ~msg:"Blue is a constructor" tokens "Blue" E.Constructor;

  (* type definition name *)
  check_entity ~msg:"Color is a type def" tokens "Color" E.Type;

  (* type alias *)
  check_entity ~msg:"Name is a type def" tokens "Name" E.Type;

  (* string literal (token includes quotes) *)
  check_categ ~msg:"string literal" tokens "\"hello\"" String;

  (* number literal *)
  check_categ ~msg:"number literal" tokens "42" Number;
  check_categ ~msg:"number literal 0" tokens "0" Number;
  check_categ ~msg:"number literal 1" tokens "1" Number;

  (* parameter *)
  let n_categs = categs_of_string tokens "n" in
  Alcotest.(check bool) "n has Parameter Def"
    true (has_categ (Parameter Def) n_categs);
  Alcotest.(check bool) "n has Parameter Use"
    true (has_categ (Parameter Use) n_categs);

  (* import keyword *)
  check_categ ~msg:"import keyword" tokens "import" KeywordModule;

  (* Haskell-specific keywords *)
  check_categ ~msg:"where keyword" tokens "where" KeywordModule;
  check_categ ~msg:"module keyword" tokens "module" KeywordModule;
  check_categ ~msg:"data keyword" tokens "data" Keyword;
  check_categ ~msg:"type keyword" tokens "type" Keyword;
  check_categ ~msg:"do keyword" tokens "do" Keyword;
  check_categ ~msg:"let keyword" tokens "let" Keyword;

  ()

let tests =
  Testo.categorize "Highlight_haskell"
    [
      t "simple.hs" test_simple;
    ]

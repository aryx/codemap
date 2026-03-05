(*
   Unit tests for OCaml highlighting in codemap.
*)
open Highlight_code
module E = Entity_code

let t = Testo.create

let tests_path = "tests/ocaml"

let highlight file =
  let hentities = Hashtbl.create 0 in
  Parsing.tokens_with_categ_of_file
    (Filename.concat tests_path file) hentities

let categs_of_string tokens s =
  tokens |> List_.filter_map (fun (s2, categ, _pos) ->
    if s2 = s then categ else None
  )

let has_entity kind categs =
  List.exists (function
    | Entity (k, _) -> k = kind
    | _ -> false) categs

let has_categ expected categs =
  List.exists (fun c -> c = expected) categs

let check_categ ~msg tokens str expected =
  let categs = categs_of_string tokens str in
  if not (has_categ expected categs) then
    Alcotest.failf "%s: expected %s for %S, got [%s]"
      msg
      (show_category expected)
      str
      (categs |> List_.map show_category |> String.concat "; ")

let check_entity ~msg tokens str kind =
  let categs = categs_of_string tokens str in
  if not (has_entity kind categs) then
    Alcotest.failf "%s: expected Entity %s for %S, got [%s]"
      msg
      (E.show_kind kind)
      str
      (categs |> List_.map show_category |> String.concat "; ")

let test_simple () =
  let tokens = highlight "simple.ml" in

  (* module definition *)
  check_categ ~msg:"module keyword" tokens "module" KeywordModule;
  check_categ ~msg:"struct keyword" tokens "struct" KeywordModule;
  check_entity ~msg:"Foo is a module def" tokens "Foo" E.Module;

  (* type definition and constructors *)
  check_entity ~msg:"color is a type def" tokens "color" E.Type;
  check_entity ~msg:"Red is a constructor" tokens "Red" E.Constructor;
  check_entity ~msg:"Green is a constructor" tokens "Green" E.Constructor;
  check_entity ~msg:"Blue is a constructor" tokens "Blue" E.Constructor;

  (* function definition and use *)
  check_entity ~msg:"factorial def is Function" tokens "factorial" E.Function;
  let factorial_categs = categs_of_string tokens "factorial" in
  Alcotest.(check bool) "factorial has Function use (recursive call)"
    true (List.length (List.filter (function
      | Entity (E.Function, Use2 _) -> true
      | _ -> false) factorial_categs) >= 1);

  (* parameter *)
  let n_categs = categs_of_string tokens "n" in
  Alcotest.(check bool) "n has Parameter Def"
    true (has_categ (Parameter Def) n_categs);
  Alcotest.(check bool) "n has Parameter Use"
    true (has_categ (Parameter Use) n_categs);

  (* let keyword *)
  check_categ ~msg:"let keyword" tokens "let" Keyword;

  (* conditionals *)
  check_categ ~msg:"if keyword" tokens "if" KeywordConditional;
  check_categ ~msg:"then keyword" tokens "then" KeywordConditional;
  check_categ ~msg:"else keyword" tokens "else" KeywordConditional;

  (* constants *)
  check_entity ~msg:"msg is a constant" tokens "msg" E.Constant;
  check_entity ~msg:"x is a constant" tokens "x" E.Constant;

  (* literals *)
  check_categ ~msg:"string literal" tokens "\"hello\"" String;
  check_categ ~msg:"number literal" tokens "42" Number;
  check_categ ~msg:"boolean literal" tokens "true" Boolean;

  (* function call *)
  check_entity ~msg:"print_int is a function use" tokens "print_int" E.Function;

  ()

let tests =
  Testo.categorize "Highlight_ocaml"
    [
      t "simple.ml" test_simple;
    ]

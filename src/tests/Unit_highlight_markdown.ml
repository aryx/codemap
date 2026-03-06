(*
   Unit tests for Markdown highlighting in codemap.
*)
open Highlight_code
open Testutil_highlight

let t = Testo.create

let tests_path = "tests/markdown"

let highlight file =
  highlight_file (Filename.concat tests_path file)

let test_simple () =
  let tokens = highlight "simple.md" in

  (* heading markers *)
  check_categ ~msg:"h1 marker" tokens "# " CommentSection0;
  check_categ ~msg:"h2 marker" tokens "## " CommentSection1;
  check_categ ~msg:"h3 marker" tokens "### " CommentSection2;

  (* heading text (inline tokens from the AST) *)
  check_categ ~msg:"h1 text" tokens "Main heading" CommentSection0;
  check_categ ~msg:"h2 text" tokens "Second heading" CommentSection1;

  (* bold *)
  check_categ ~msg:"bold open" tokens "**" Punctuation;
  check_categ ~msg:"bold text" tokens "bold text" EmbededHtml;

  (* italic *)
  check_categ ~msg:"italic open" tokens "*" Punctuation;
  check_categ ~msg:"italic text" tokens "italic text" EmbededStyle;

  (* inline code *)
  check_categ ~msg:"backtick" tokens "`" Punctuation;
  check_categ ~msg:"inline code" tokens "inline code" EmbededCode;

  (* link *)
  check_categ ~msg:"link url" tokens "http://example.com" EmbededUrl;

  (* image *)
  check_categ ~msg:"image alt" tokens "alt text" String;
  check_categ ~msg:"image url" tokens "image.png" EmbededUrl;
  check_categ ~msg:"bang" tokens "!" Punctuation;

  (* list markers *)
  check_categ ~msg:"unordered list marker" tokens "- " Punctuation;
  check_categ ~msg:"ordered list marker" tokens "1. " Punctuation;

  (* fenced code block *)
  check_categ ~msg:"fenced code fence" tokens "```" Keyword;

  (* blockquote marker *)
  check_categ ~msg:"blockquote" tokens "> " CommentImportance1;

  (* thematic break *)
  check_categ ~msg:"thematic break" tokens "---" CommentEstet;

  (* indented code block *)
  check_categ ~msg:"indented code" tokens
    "indented code block\n" EmbededCode;

  ()

let tests =
  Testo.categorize "Highlight_markdown"
    [
      t "simple.md" test_simple;
    ]

val is_eof : Lexer_nw.token -> bool
val is_comment : Lexer_nw.token -> bool
val token_kind_of_tok : Lexer_nw.token -> Lib_ast_fuzzy.token_kind
val info_of_tok : Lexer_nw.token -> Tok.t

val visitor_info_of_tok :
  (Tok.t -> Tok.t) -> Lexer_nw.token -> Lexer_nw.token

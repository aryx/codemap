(* the token list contains also the comment-tokens *)
type program_and_tokens = Ast_nw.program * Lexer_nw.token list

(* This is the main function *)
val parse : string (* filename *) -> program_and_tokens * Parsing_stat.t
val parse_fuzzy : string (* filename *) -> Ast_fuzzy.tree list * Lexer_nw.token list

(* internal *)
val tokens : Parsing_helpers.input_source -> Lexer_nw.token list

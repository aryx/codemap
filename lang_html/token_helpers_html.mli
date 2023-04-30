val info_of_tok : Parser_html.token -> Tok.t

val visitor_info_of_tok :
  (Tok.t -> Tok.t) -> Parser_html.token -> Parser_html.token

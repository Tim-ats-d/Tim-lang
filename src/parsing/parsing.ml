let of_position { Lexing.pos_lnum; pos_bol; pos_cnum; _ } ~msg =
  Printf.sprintf "Line:%i, Character:%i\nError: %s" pos_lnum
    (pos_cnum - pos_bol) msg

let parse lexbuf =
  try Ok (Parser.program Lexer.token lexbuf) with
  | Lexer.Syntax_error (msg, { lex_curr_p; _ }) ->
      Result.error @@ of_position lex_curr_p ~msg
  | Parser.Error ->
      Result.error @@ of_position lexbuf.lex_curr_p ~msg:"syntax error"

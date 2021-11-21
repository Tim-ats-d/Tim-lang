let from_lexbuf lexbuf =
  match Parsing.parse lexbuf with
  | Ok ast -> Ast.Eval.eval_program ast
  | Error err -> prerr_endline err

let from_str str = Lexing.from_string str |> from_lexbuf

let from_file filename =
  let lexbuf = Lexing.from_channel (open_in filename) in
  Lexing.set_filename lexbuf filename;
  from_lexbuf lexbuf

let from_lexbuf lexbuf =
  match Parsing.parse lexbuf with
  | Ok ast_opt -> (
      match ast_opt with None -> () | Some ast -> Ast.Eval.eval_program ast)
  | Error err -> prerr_endline err

let from_str str = from_lexbuf (Lexing.from_string str)

let from_file filename =
  let lexbuf = Lexing.from_channel (open_in filename) in
  Lexing.set_filename lexbuf filename;
  from_lexbuf lexbuf

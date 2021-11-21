{
  open Parser

  let string_acc: Buffer.t = Buffer.create 64

  exception Syntax_error of Lexing.lexbuf
}

let digit = ['0'-'9']

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']

let alpha = (lowercase|uppercase)
let alphanum = (alpha|digit)

let newline = '\n' | "\r\n"
let whitespace = [' ' '\t']+

let comment = '#' [^ '\r' '\n']* (newline)
let ident = (lowercase) ('_'|lowercase|digit)*
let env_var = (uppercase|digit|'_')*
let flag = ('-'|"--") (alphanum)

rule token = parse
  | comment { token lexbuf }
  | '=' { EQ }
  | '(' { LPARENT }
  | ')' { RPARENT }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | ',' { COMMA }
  | "int" { TINT }
  | "string" { TSTRING }
  | "cmd" { TCMD }
  | "list" { TLIST }
  | "let" { LET }
  | "tell" { TELL }
  | "with" { WITH }
  | "as" { AS }
  | "to" { TO }
  | "for" { FOR }
  | "in" { IN }
  | "do" { DO }
  | "and" { AND }
  | "end" { END }
  | '"' { read_string lexbuf }
  | flag { FLAG (Lexing.lexeme lexbuf) }
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | digit+ { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | env_var { ENV_VAR (Lexing.lexeme lexbuf) }
  | whitespace { token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | eof { EOF }
  | _ { raise (Syntax_error lexbuf) }

and read_string = parse
  | '"' { let c = Buffer.contents string_acc in Buffer.clear string_acc; STRING c }
  | '\\' '"' { Buffer.add_char string_acc '"'; read_string lexbuf }
  | [^ '"'] { Buffer.add_string string_acc (Lexing.lexeme lexbuf); read_string lexbuf }
  | eof { failwith "string literal is never terminated" }
  | _ { failwith ("Character not allowed in string literal: '" ^ Lexing.lexeme lexbuf ^ "'") }

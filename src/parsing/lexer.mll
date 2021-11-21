{
  open Parser

  let buf = Buffer.create 64

  exception Syntax_error of string * Lexing.lexbuf
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
  | comment    { Lexing.new_line lexbuf; token lexbuf }
  | '('        { LPARENT }
  | ')'        { RPARENT }
  | '['        { LBRACKET }
  | ']'        { RBRACKET }
  | ','        { COMMA }
  | '='        { EQ }
  | "int"      { TINT }
  | "string"   { TSTRING }
  | "cmd"      { TCMD }
  | "list"     { TLIST }
  | "let"      { LET }
  | "tell"     { TELL }
  | "with"     { WITH }
  | "as"       { AS }
  | "to"       { TO }
  | "for"      { FOR }
  | "in"       { IN }
  | "do"       { DO }
  | "and"      { AND }
  | "end"      { END }
  | '"'        { read_string lexbuf }
  | flag       { FLAG (Lexing.lexeme lexbuf) }
  | ident      { IDENT (Lexing.lexeme lexbuf) }
  | digit+     { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | env_var    { ENV_VAR (Lexing.lexeme lexbuf) }
  | whitespace { token lexbuf }
  | newline    { Lexing.new_line lexbuf; token lexbuf }
  | eof        { EOF }
  | _          { raise (Syntax_error ("lexing error", lexbuf)) }

and read_string = parse
  | '"'      { let c = Buffer.contents buf in
               Buffer.clear buf; STRING c }
  | '\\' '"' { Buffer.add_char buf '"'; read_string lexbuf }
  | [^ '"']  { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string lexbuf }
  | eof      { let msg = "string literal is never terminated" in
               raise (Syntax_error (msg, lexbuf)) }
  | _        { let msg = Printf.sprintf "Character not allowed in string literal: '%s'" @@ Lexing.lexeme lexbuf in
               raise (Syntax_error (msg, lexbuf)) }

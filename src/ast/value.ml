type t = Val_int of int | Val_string of string | Val_cmd of Ast_types.Cmd.t

let cmd ~name ~args = Val_cmd { name; args }

let int i = Val_int i

let string s = Val_string s

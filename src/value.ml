type t = Val_cmd of Ast.Cmd.t | Val_int of int

let cmd ~name ~args = Val_cmd { name; args }

let int i = Val_int i

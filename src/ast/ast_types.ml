module Cmd = struct
  type t = { name : string; args : string list }

  let add_arg cmd arg = { cmd with args = arg :: cmd.args }
end

type program = stmt list

and stmt =
  | Assign of { name : string; value : expr }
  | Expr of expr
  | With of { env_vars : (expr * expr) array; body : stmt }

and expr =
  | Name of string
  | String of string
  | Cmd of Cmd.t
  | Tell of { cmd : expr; args : expr list }
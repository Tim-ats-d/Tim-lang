module Cmd = struct
  type t = { name : string; args : string list }

  let add_arg cmd arg = { cmd with args = arg :: cmd.args }
end

type program = stmt list

and stmt =
  | Assign of { name : string; value : expr }
  | Expr of expr
  | With of { env_vars : (expr * expr) array; body : stmt }
  | For of { name : string; iter : expr; body : stmt list }

and expr =
  | Name of string
  | String of string
  | Int of int
  | List of expr Seq.t
  | Cmd of Cmd.t
  | Cast of { target : expr; t : Type.t }
  | Tell of { cmd : expr; args : expr list }

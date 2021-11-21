type program = stmt list

and stmt = Assign of { name : string; value : expr } | Expr of expr

and expr =
  | Unit
  | Bool of bool
  | Int of int
  | String of string
  | Name of string
  | Ellipsis
  | Cmd of { name : string; args : string list }
  | List of expr Seq.t
  | Cast of { from : expr; to_ : Type.t }
  | IfThenElse of { cond : expr; body : expr; orelse : expr }
  | Tell of { cmd : expr; args : expr list }
  | With of { vars : (expr * expr) array; body : stmt }
  | For of { name : string; iter : expr; body : stmt list }

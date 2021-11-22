type program = stmt list

and stmt = Assign of { name : string; value : expr } | Expr of expr

and expr =
  | Unit
  | Bool of bool
  | Int of int
  | String of string
  | Name of string
  | Cmd of { name : string; args : string list }
  | List of expr Seq.t
  | Cast of { from : expr; to_ : Type.t }
  | IfThenElse of { cond : expr; body : expr; orelse : expr }
  | With of { vars : (expr * expr) array; body : stmt }
  | For of { name : string; iter : expr; body : stmt list }
  | ListCompr of { name : string; iter : expr; cond : expr; body : expr }
  | Tell of { cmd : expr; args : expr list }

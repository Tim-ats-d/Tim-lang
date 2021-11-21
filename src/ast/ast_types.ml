type program = stmt list

and stmt =
  | Assign of { name : string; value : expr }
  | Expr of expr
  | With of { vars : (expr * expr) array; body : stmt }
  | For of { name : string; iter : expr; body : stmt list }

and expr =
  | Name of string
  | String of string
  | Int of int
  | List of expr Seq.t
  | Cmd of { name : string; args : string list }
  | Cast of { from : expr; to_ : Type.t }
  | Tell of { cmd : expr; args : expr list }

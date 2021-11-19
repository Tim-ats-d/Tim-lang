module Arg = struct
  type t = Anon of string | Option of string * string | Unit of string

  let to_string =
    let open Printf in
    function
    | Anon a -> a
    | Option (name, value) -> sprintf "-%s=%s" name value
    | Unit u -> sprintf "-%s" u
end

module Cmd = struct
  type t = { name : string; args : Arg.t list }

  let to_string { name; args } =
    let args = List.map Arg.to_string args |> String.concat " " in
    Printf.sprintf "%s %s" name args

  let add_arg cmd arg = { cmd with args = arg :: cmd.args }
end

type program = stmt list

and stmt =
  | Assign of string * expr
  | Expr of expr
  | Add of { target : string; arg : Arg.t }

and expr =
  | Name of string
  | Cmd of Cmd.t
  | Tell of { cmd : expr; args : string list; env_vars : Utils.Unix.Env.t }

type t =
  | VUnit
  | VBool of bool
  | VInt of int
  | VString of string
  | VCmd of { name : string; args : string list }
  | VList of t Seq.t

let bool b = VBool b

let int i = VInt i

let string s = VString s

let cmd ~name ~args = VCmd { name; args }

let list l = VList l

let to_bool = function VBool b -> b | _ -> failwith "must be a bool"

let to_seq = function VList seq -> seq | _ -> failwith "must be a list"

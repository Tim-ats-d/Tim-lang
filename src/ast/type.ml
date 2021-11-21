open Value

type t = Unit | Bool | Int | String | Ellipsis | Cmd | List

let of_value = function
  | VUnit -> Unit
  | VBool _ -> Bool
  | VInt _ -> Int
  | VString _ -> String
  | VEllipsis -> Ellipsis
  | VCmd _ -> Cmd
  | VList _ -> List

let to_string = function
  | Unit -> "Unit"
  | Bool -> "Bool"
  | Int -> "Int"
  | String -> "String"
  | Ellipsis -> "Ellipsis"
  | Cmd -> "Cmd"
  | List -> "List"

let cast ~from ~to_ =
  match (from, to_) with
  | (VInt _ as v), Int -> v
  | VInt i, String -> string_of_int i |> Value.string
  | (VString _ as v), String -> v
  | VString s, Int -> int_of_string s |> Value.int
  | VString s, List ->
      String.to_seq s
      |> Seq.map (fun c -> VString (String.make 1 c))
      |> Value.list
  | value, t ->
      failwith
      @@ Printf.sprintf "%s cannot be interpreted as %s"
           (to_string @@ of_value value)
           (to_string t)

open Value

type t = Int | String | Cmd | List

let from_value = function
  | VInt _ -> Int
  | VString _ -> String
  | VCmd _ -> Cmd
  | VList _ -> List

let to_string = function
  | Int -> "Int"
  | String -> "String"
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
      Printf.sprintf "%s cannot be interpreted as %s"
        (to_string @@ from_value value)
        (to_string t)
      |> failwith

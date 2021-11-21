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
  | VInt i, String -> VString (string_of_int i)
  | (VString _ as v), String -> v
  | VString s, Int -> VInt (int_of_string s)
  | VString s, List ->
      let l = String.to_seq s |> Seq.map (fun c -> VString (String.make 1 c)) in
      VList l
  | value, t ->
      Printf.sprintf "%s cannot be interpreted as %s"
        (from_value value |> to_string)
        (to_string t)
      |> failwith

open Ast_types
open Utils

let rec eval_program prgrm = List.iter (eval_stmt Env.empty) prgrm

and eval_stmt env = function
  | Assign { name; value } ->
      let value = eval_expr env value in
      Env.set env ~name ~value
  | Expr expr -> eval_expr env expr |> ignore
  | With { env_vars; body } -> eval_with env ~body ~vars:env_vars

and eval_expr env = function
  | Name name -> Env.find_exn env ~name
  | String str -> Value.string str
  | Cmd { name; args } -> Value.cmd ~name ~args
  | Tell { cmd; args } -> eval_tell env ~cmd ~args

and eval_with env ~body ~vars =
  let vars_values =
    Array.map
      (fun (name, value) ->
        match (eval_expr env name, eval_expr env value) with
        | Value.Val_string name, Value.Val_string value -> (name, value)
        | _ -> failwith "must be string")
      vars
  in
  Unix.Env.with_env vars_values ~f:(fun () -> eval_stmt env body)

and eval_tell env ~cmd ~args=
  let cmd_value =
    match eval_expr env cmd with
    | Value.Val_string str -> str
    | _ -> failwith "must be a string"
  and args_value =
    List.map
      (fun expr ->
        match eval_expr env expr with
        | Value.Val_string str -> str
        | _ -> failwith "must be string")
      args
  in
  let line = Printf.sprintf "%s %s" cmd_value (String.concat " " args_value) in
  Sys.command line |> Value.int

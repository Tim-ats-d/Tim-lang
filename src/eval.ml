open Ast
open Utils

let rec eval_program (env : Env.t) = List.iter (eval_stmt env)

and eval_stmt env = function
  | Assign (name, value) ->
      let value = eval_expr env value in
      Env.set env ~name ~value
  | Expr expr -> eval_expr env expr |> ignore
  | Add { target; arg } ->
      let cmd =
        match Env.find_exn env ~name:target with
        | Value.Val_cmd cmd -> cmd
        | _ -> failwith "must be a cmd"
      in
      let new_cmd = Value.Val_cmd (Cmd.add_arg cmd arg) in
      Env.set env ~name:target ~value:new_cmd

and eval_expr env = function
  | Name name -> Env.find_exn env ~name
  | Cmd { name; args } -> Value.cmd ~name ~args
  | Tell { cmd; args; env_vars } -> eval_tell env ~cmd ~args ~env_vars

and eval_tell env ~cmd ~args ~env_vars =
  let cmd =
    match eval_expr env cmd with
    | Value.Val_cmd cmd -> Cmd.to_string cmd
    | _ -> failwith "must be a cmd"
  in
  let line = Printf.sprintf "%s %s" cmd (String.concat "" args) in
  Unix.Env.with_env env_vars ~cmd:line |> Value.int

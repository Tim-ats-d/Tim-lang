open Ast_types
open Utils

let rec eval_program prgrm = List.iter (eval_stmt Env.empty) prgrm

and eval_stmt env = function
  | Assign { name; value } ->
      let value = eval_expr env value in
      eval_assign env ~name ~value
  | Expr expr -> eval_expr env expr |> ignore
  | With { env_vars; body } -> eval_with env ~body ~vars:env_vars
  | For { name; iter; body } ->
      let iter = eval_expr env iter |> Value.to_seq in
      let rec loop i = function
        | Seq.Nil -> ()
        | Seq.Cons (x, xs) ->
            eval_assign env ~name ~value:x;
            eval_stmts env body;
            loop (i + 1) (xs ())
      in
      loop 0 (iter ())

and eval_stmts env stmts = List.iter (eval_stmt env) stmts

and eval_expr env = function
  | Name name -> Env.find_exn env ~name
  | String s -> Value.string s
  | Int i -> Value.int i
  | List l ->
      let lst = Seq.map (eval_expr env) l in
      Value.list lst
  | Cmd { name; args } -> Value.cmd ~name ~args
  | Cast { target; t } ->
      let value = eval_expr env target in
      Type.cast ~from:value ~to_:t
  | Tell { cmd; args } -> eval_tell env ~cmd ~args

and eval_assign env ~name ~value = Env.set env ~name ~value

and eval_with env ~body ~vars =
  let vars_values =
    Array.map
      (fun (name, value) ->
        match (eval_expr env name, eval_expr env value) with
        | Value.VString name, Value.VString value -> (name, value)
        | _ -> failwith "must be string")
      vars
  in
  Unix.Env.with_env vars_values ~f:(fun () -> eval_stmt env body)

and eval_tell env ~cmd ~args =
  let cmd_value =
    match eval_expr env cmd with
    | Value.VString str -> str
    | _ -> failwith "must be a string"
  and args_value =
    List.map
      (fun expr ->
        match eval_expr env expr with
        | Value.VString str -> str
        | _ -> failwith "must be string")
      args
  in
  let line = Printf.sprintf "%s %s" cmd_value (String.concat " " args_value) in
  Sys.command line |> Value.int

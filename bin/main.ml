open Tim_lang

let () =
  let program =
    Ast.
      [
        (* Assign ("print", Cmd { name = "echo"; args = [ Anon "ok" ] });
           Add { target = "print"; arg = Unit "e" };
           Expr (Tell { cmd = Name "print"; args = []; env_vars = [||] }); *)
        Expr
          (Tell
             {
               cmd = Cmd { name = "echo"; args = [] };
               args = [ {|"$LANG"|} ];
               env_vars = [| ("LANG", "FR") |];
             });
      ]
  in

  Eval.eval_program Env.empty program

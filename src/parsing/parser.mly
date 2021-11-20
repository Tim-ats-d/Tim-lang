%{
  open Ast.Ast_types

  let array_of_list list = List.to_seq list |> Array.of_seq
%}

%token <string> IDENT
%token <string> FLAG
%token <string> STRING
%token <string> ENV_VAR

%token EOF

%token EQ
%token LPARENT RPARENT

%token TELL SET AND TO WITH END

%start program

%type <program option> program

%%

let program :=
  | EOF; { None }
  | slist=stmt+; EOF; { Some slist }

let stmt :=
  | assign
  | with_
  | e=expr; { Expr e }

let assign ==
  | SET; name=IDENT; TO; value=sub_expr; { Assign { name; value } }

let with_ ==
  | WITH; env_vars=vars; body=stmt; END; { With { env_vars; body } }

let expr :=
  | sub_expr
  | tell

let sub_expr :=
  | terminal
  | LPARENT; e=expr; RPARENT; { e }

let terminal ==
  | name
  | string

let name ==
  | n=IDENT; { Name n }

let string ==
  | s=STRING; { String s }

let tell :=
  | TELL; cmd=sub_expr;
    { Tell { cmd; args = []; } }
  | TELL; cmd=sub_expr; TO; args=arg+; END;
    { Tell { cmd; args; } }

let vars :=
  | v=var; { [| v |] }
  | v=var; vars=and_var+; { array_of_list (v :: vars) }

let var :=
  | n=env_var; EQ; value=sub_expr; { (n, value) }

let env_var ==
  | e=ENV_VAR; { String e }

let and_var ==
  | AND; v=var; { v }

let arg :=
  | f=FLAG; { String f }
  | t=terminal; { t }

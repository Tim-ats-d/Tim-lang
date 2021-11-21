%{
  open Ast.Ast_types

  let array_of_list list = List.to_seq list |> Array.of_seq
%}

%token <string> IDENT
%token <int> INT
%token <string> FLAG
%token <string> STRING
%token <string> ENV_VAR

%token LPARENT RPARENT
%token LBRACKET RBRACKET
%token COMMA

%token EQ

%token TINT TSTRING TCMD TLIST

%token LET
%token IF THEN ELSE
%token TELL WITH AS
%token FOR IN DO
%token TO AND END

%token EOF

%start program

%type <program> program

%%

let program :=
  | slist=stmt*; EOF; { slist }

let stmt :=
  | assign
  | with_
  | for_
  | e=expr; { Expr e }

let assign ==
  | LET; name=IDENT; EQ; value=sub_expr; { Assign { name; value } }

let with_ ==
  | WITH; vars=vars; body=stmt; END; { With { vars; body } }

let for_ ==
  | FOR; name=IDENT; IN; iter=expr; DO; body=stmt*; END;
    { For { name; iter; body } }

let expr :=
  | sub_expr
  | cast
  | tell

let sub_expr :=
  | terminal
  | LPARENT; e=expr; RPARENT; { e }

let cast ==
  | from=sub_expr; AS; to_=type_; { Cast { from; to_ } }

let type_ ==
  | TINT; { Ast.Type.Int }
  | TSTRING; { Ast.Type.String }
  | TCMD; { Ast.Type.Cmd }
  | TLIST; { Ast.Type.List }

let terminal ==
  | name
  | string
  | int
  | list_

let name ==
  | n=IDENT; { Name n }

let string ==
  | s=STRING; { String s }

let int ==
  | i=INT; { Int i }

let list_ ==
  | LBRACKET; l=separated_list(COMMA, expr); RBRACKET; { List (List.to_seq l) }

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
  | e=sub_expr; { e }

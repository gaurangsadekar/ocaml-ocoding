%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE EOF ASSIGN SEQ
%token <int> LITERAL
%token <int> VARIABLE

%left SEQ
%left ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE

%start expr
%type < Ast.expr> expr

%%

expr:
| expr PLUS   expr     { Binop($1, Add, $3) }
| expr MINUS  expr     { Binop($1, Sub, $3) }
| expr TIMES  expr     { Binop($1, Mul, $3) }
| expr DIVIDE expr     { Binop($1, Div, $3) }
| expr SEQ expr        { Seq($1, $3) }
| VARIABLE ASSIGN expr { Asn($1, $3) }
| VARIABLE             { Var($1) }
| LITERAL              { Lit($1) }

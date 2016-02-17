type token =
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | EOF
  | ASSIGN
  | SEQ
  | LITERAL of (int)
  | VARIABLE of (int)

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 17 "parser.ml"
let yytransl_const = [|
  257 (* PLUS *);
  258 (* MINUS *);
  259 (* TIMES *);
  260 (* DIVIDE *);
    0 (* EOF *);
  261 (* ASSIGN *);
  262 (* SEQ *);
    0|]

let yytransl_block = [|
  263 (* LITERAL *);
  264 (* VARIABLE *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\000\000"

let yylen = "\002\000\
\003\000\003\000\003\000\003\000\003\000\003\000\001\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\008\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\003\000\004\000\
\000\000"

let yydgoto = "\002\000\
\005\000"

let yysindex = "\255\255\
\016\255\000\000\000\000\017\255\015\255\016\255\016\255\016\255\
\016\255\016\255\016\255\001\255\022\255\022\255\000\000\000\000\
\001\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\001\000\020\000\000\000\000\000\000\000\
\000\000\000\000\000\000\006\000\007\000\009\000\000\000\000\000\
\008\000"

let yygindex = "\000\000\
\004\000"

let yytablesize = 271
let yytable = "\001\000\
\007\000\007\000\008\000\009\000\010\000\006\000\001\000\005\000\
\002\000\012\000\013\000\014\000\015\000\016\000\017\000\007\000\
\008\000\009\000\010\000\009\000\011\000\006\000\003\000\004\000\
\009\000\010\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\007\000\007\000\007\000\007\000\000\000\007\000\001\000\
\001\000\002\000\002\000\006\000\001\000\005\000\002\000"

let yycheck = "\001\000\
\000\000\001\001\002\001\003\001\004\001\000\000\000\000\000\000\
\000\000\006\000\007\000\008\000\009\000\010\000\011\000\001\001\
\002\001\003\001\004\001\000\000\006\001\005\001\007\001\008\001\
\003\001\004\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\255\255\006\001\001\001\
\002\001\001\001\002\001\006\001\006\001\006\001\006\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  EOF\000\
  ASSIGN\000\
  SEQ\000\
  "

let yynames_block = "\
  LITERAL\000\
  VARIABLE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Ast.expr) in
    Obj.repr(
# 18 "parser.mly"
                       ( Binop(_1, Add, _3) )
# 158 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Ast.expr) in
    Obj.repr(
# 19 "parser.mly"
                       ( Binop(_1, Sub, _3) )
# 166 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Ast.expr) in
    Obj.repr(
# 20 "parser.mly"
                       ( Binop(_1, Mul, _3) )
# 174 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Ast.expr) in
    Obj.repr(
# 21 "parser.mly"
                       ( Binop(_1, Div, _3) )
# 182 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Ast.expr) in
    Obj.repr(
# 22 "parser.mly"
                       ( Seq(_1, _3) )
# 190 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Ast.expr) in
    Obj.repr(
# 23 "parser.mly"
                       ( Asn(_1, _3) )
# 198 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 24 "parser.mly"
                       ( Var(_1) )
# 205 "parser.ml"
               :  Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 25 "parser.mly"
                       ( Lit(_1) )
# 212 "parser.ml"
               :  Ast.expr))
(* Entry expr *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let expr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  Ast.expr)
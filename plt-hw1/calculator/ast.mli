type operator = Add | Sub | Mul | Div

type expr =
  | Binop of expr * operator * expr
  | Lit of int
  | Asn of int * expr
  | Var of int
  | Seq of expr * expr

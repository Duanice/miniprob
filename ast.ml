(* ast.ml *)

type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr
  | Lt  of expr * expr
  | Eq  of expr * expr
  | Flip of float

type stmt =
  | Skip
  | Assign of string * expr
  | Seq of stmt * stmt
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Observe of expr

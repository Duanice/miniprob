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

let rec string_of_expr = function
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Var v -> v
  | Add (e1, e2) -> "(" ^ string_of_expr e1 ^ " + " ^ string_of_expr e2 ^ ")"
  | Sub (e1, e2) -> "(" ^ string_of_expr e1 ^ " - " ^ string_of_expr e2 ^ ")"
  | Lt (e1, e2) -> "(" ^ string_of_expr e1 ^ " < " ^ string_of_expr e2 ^ ")"
  | Eq (e1, e2) -> "(" ^ string_of_expr e1 ^ " == " ^ string_of_expr e2 ^ ")"
  | Flip p -> "flip(" ^ string_of_float p ^ ")"

let rec string_of_stmt = function
  | Skip -> "skip"
  | Assign (v, e) -> v ^ " = " ^ string_of_expr e
  | Seq (s1, s2) -> "Seq(" ^ string_of_stmt s1 ^ ", " ^ string_of_stmt s2 ^ ")"
  | If (cond, then_stmt, else_stmt) ->
      "If(" ^ string_of_expr cond ^ ", " ^ string_of_stmt then_stmt ^ ", " ^ string_of_stmt else_stmt ^ ")"
  | While (cond, body) -> "While(" ^ string_of_expr cond ^ ", " ^ string_of_stmt body ^ ")"
  | Observe e -> "observe(" ^ string_of_expr e ^ ")"

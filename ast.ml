(* ast.ml *)

type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr  (* 乘法 *)
  | Lt  of expr * expr
  | Gt  of expr * expr  (* 大于 *)
  | Eq  of expr * expr
  | Flip of float
  | Neg of expr  (* 一元负号 *)
  (* 布尔运算 *)
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Xor of expr * expr
  (* 数学函数 *)
  | Max of expr * expr
  | Min of expr * expr
  | Abs of expr

type stmt =
  | Skip
  | Assign of string * expr
  | Seq of stmt * stmt
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Observe of expr
  | Print of expr  (* 输出函数 *)
  | PrintWith of string * expr  (* 带前缀的输出：前缀 + 表达式值 *)

let rec string_of_expr = function
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Var v -> v
  | Add (e1, e2) -> "(" ^ string_of_expr e1 ^ " + " ^ string_of_expr e2 ^ ")"
  | Sub (e1, e2) -> "(" ^ string_of_expr e1 ^ " - " ^ string_of_expr e2 ^ ")"
  | Mul (e1, e2) -> "(" ^ string_of_expr e1 ^ " * " ^ string_of_expr e2 ^ ")"
  | Lt (e1, e2) -> "(" ^ string_of_expr e1 ^ " < " ^ string_of_expr e2 ^ ")"
  | Gt (e1, e2) -> "(" ^ string_of_expr e1 ^ " > " ^ string_of_expr e2 ^ ")"
  | Eq (e1, e2) -> "(" ^ string_of_expr e1 ^ " == " ^ string_of_expr e2 ^ ")"
  | Flip p -> "flip(" ^ string_of_float p ^ ")"
  | Neg e -> "(-" ^ string_of_expr e ^ ")"
  | And (e1, e2) -> "(" ^ string_of_expr e1 ^ " && " ^ string_of_expr e2 ^ ")"
  | Or (e1, e2) -> "(" ^ string_of_expr e1 ^ " || " ^ string_of_expr e2 ^ ")"
  | Not e -> "(!" ^ string_of_expr e ^ ")"
  | Xor (e1, e2) -> "(" ^ string_of_expr e1 ^ " xor " ^ string_of_expr e2 ^ ")"
  | Max (e1, e2) -> "max(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Min (e1, e2) -> "min(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Abs e -> "abs(" ^ string_of_expr e ^ ")"

let rec string_of_stmt = function
  | Skip -> "skip"
  | Assign (v, e) -> v ^ " := " ^ string_of_expr e
  | Seq (s1, s2) -> "Seq(" ^ string_of_stmt s1 ^ ", " ^ string_of_stmt s2 ^ ")"
  | If (cond, then_stmt, else_stmt) ->
      "If(" ^ string_of_expr cond ^ ", " ^ string_of_stmt then_stmt ^ ", " ^ string_of_stmt else_stmt ^ ")"
  | While (cond, body) -> "While(" ^ string_of_expr cond ^ ", " ^ string_of_stmt body ^ ")"
  | Observe e -> "observe(" ^ string_of_expr e ^ ")"
  | Print e -> "print(" ^ string_of_expr e ^ ")"
  | PrintWith (prefix, e) -> "printwith(\"" ^ String.escaped prefix ^ "\", " ^ string_of_expr e ^ ")"

(* ============================================
   解释器实现 (Interpreter Implementation)
   ============================================ *)

open Ast

exception Reject

(* 值类型：解释器计算表达式的结果 *)
type value =
  | IntVal of int    (* 整数值 *)
  | BoolVal of bool  (* 布尔值 *)

(* 环境：变量名到值的映射 *)
type env = (string * value) list

(* 辅助函数：从环境中查找变量值 *)
let lookup (env : env) (var : string) : value =
  try List.assoc var env
  with Not_found -> failwith ("Variable " ^ var ^ " not found")

(* 辅助函数：更新环境中的变量值 *)
let update (env : env) (var : string) (value : value) : env =
  (var, value) :: List.remove_assoc var env

(* 表达式求值函数 *)
let rec eval_expr (env : env) : expr -> value = function
  | Int i -> IntVal i                                    (* 整数字面量 *)
  | Bool b -> BoolVal b                                  (* 布尔字面量 *)
  | Var v -> lookup env v                                (* 变量查找 *)
  | Add (e1, e2) ->                                      (* 加法运算 *)
      (match eval_expr env e1, eval_expr env e2 with
       | IntVal v1, IntVal v2 -> IntVal (v1 + v2)
       | _ -> failwith "Type error: Add expects integers")
  | Sub (e1, e2) ->                                      (* 减法运算 *)
      (match eval_expr env e1, eval_expr env e2 with
       | IntVal v1, IntVal v2 -> IntVal (v1 - v2)
       | _ -> failwith "Type error: Sub expects integers")
  | Mul (e1, e2) ->                                      (* 乘法运算 *)
      (match eval_expr env e1, eval_expr env e2 with
       | IntVal v1, IntVal v2 -> IntVal (v1 * v2)
       | _ -> failwith "Type error: Mul expects integers")
  | Lt (e1, e2) ->                                       (* 小于比较 *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      BoolVal (match (v1, v2) with
        | (IntVal i1, IntVal i2) -> i1 < i2
        | (BoolVal b1, BoolVal b2) -> b1 < b2
        | _ -> failwith "Type error in comparison")
  | Gt (e1, e2) ->                                       (* 大于比较 *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      BoolVal (match (v1, v2) with
        | (IntVal i1, IntVal i2) -> i1 > i2
        | (BoolVal b1, BoolVal b2) -> b1 > b2
        | _ -> failwith "Type error in comparison")
  | Eq (e1, e2) ->                                       (* 等于比较 *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      BoolVal (v1 = v2)
  | Flip p ->                                            (* 概率翻转 *)
      BoolVal (Random.float 1.0 < p)

  | Neg e ->                                             (* 一元负号 *)
      (match eval_expr env e with
       | IntVal i -> IntVal (-i)
       | _ -> failwith "Type error: Neg expects integer")

  (* 布尔运算 *)
  | And (e1, e2) ->                                      (* 逻辑与 *)
      (match eval_expr env e1, eval_expr env e2 with
       | BoolVal b1, BoolVal b2 -> BoolVal (b1 && b2)
       | _ -> failwith "Type error: And expects booleans")

  | Or (e1, e2) ->                                       (* 逻辑或 *)
      (match eval_expr env e1, eval_expr env e2 with
       | BoolVal b1, BoolVal b2 -> BoolVal (b1 || b2)
       | _ -> failwith "Type error: Or expects booleans")

  | Not e ->                                             (* 逻辑非 *)
      (match eval_expr env e with
       | BoolVal b -> BoolVal (not b)
       | _ -> failwith "Type error: Not expects boolean")

  | Xor (e1, e2) ->                                      (* 逻辑异或 *)
      (match eval_expr env e1, eval_expr env e2 with
       | BoolVal b1, BoolVal b2 -> BoolVal ((b1 || b2) && not (b1 && b2))
       | _ -> failwith "Type error: Xor expects booleans")

  (* 数学函数 *)
  | Max (e1, e2) ->                                      (* 最大值 *)
      (match eval_expr env e1, eval_expr env e2 with
       | IntVal i1, IntVal i2 -> IntVal (max i1 i2)
       | _ -> failwith "Type error: Max expects integers")
  | Min (e1, e2) ->                                      (* 最小值 *)
      (match eval_expr env e1, eval_expr env e2 with
       | IntVal i1, IntVal i2 -> IntVal (min i1 i2)
       | _ -> failwith "Type error: Min expects integers")
  | Abs e ->                                             (* 绝对值 *)
      (match eval_expr env e with
       | IntVal i -> IntVal (abs i)
       | _ -> failwith "Type error: Abs expects integer")

(* 语句执行函数 *)
let rec eval_stmt (env : env) : stmt -> env = function
  | Skip -> env                                          (* 空语句 *)
  | Assign (var, expr) ->                                (* 赋值语句 *)
      let value = eval_expr env expr in
      update env var value
  | Seq (s1, s2) ->                                      (* 语句序列 *)
      let env' = eval_stmt env s1 in
      eval_stmt env' s2
  | If (cond, then_stmt, else_stmt) ->                   (* 条件语句 *)
      (match eval_expr env cond with
       | BoolVal condition ->
           if condition then
             eval_stmt env then_stmt
           else
             eval_stmt env else_stmt
       | _ -> failwith "Type error: If condition must be boolean")
  | While (cond, body) ->                                (* 循环语句 *)
      (match eval_expr env cond with
       | BoolVal condition ->
           if condition then
             let env' = eval_stmt env body in
             eval_stmt env' (While (cond, body))  (* 递归执行循环 *)
           else
             env
       | _ -> failwith "Type error: While condition must be boolean")
  | Observe expr ->                                      (* 观测语句 *)
      (match eval_expr env expr with
       | BoolVal true -> env
       | BoolVal false -> raise Reject
       | _ -> failwith "Type error: Observe expects boolean")

  | Print expr ->                                        (* 打印语句 *)
      let value = eval_expr env expr in
      (match value with
       | IntVal i -> Printf.printf "%d" i
       | BoolVal b -> Printf.printf "%b" b);
      Printf.printf "\n";
      env  (* 打印不改变环境 *)

  | PrintWith (prefix, expr) ->                         (* 带前缀的打印语句 *)
      let value = eval_expr env expr in
      Printf.printf "%s " prefix;  (* 前缀后加空格 *)
      (match value with
       | IntVal i -> Printf.printf "%d" i
       | BoolVal b -> Printf.printf "%b" b);
      Printf.printf "\n";
      env  (* 打印不改变环境 *)

(* 辅助函数：打印环境 *)
let print_env (env : env) : unit =
  Printf.printf "Environment:\n";
  List.iter (fun (var, value) ->
    Printf.printf "  %s = " var;
    match value with
    | IntVal i -> Printf.printf "%d" i
    | BoolVal b -> Printf.printf "%b" b;
    Printf.printf "\n"
  ) env

(* 示例程序演示 *)
let example_program () =
  (* 创建程序：x := 5; y := 3; z := x + y; observe z; if flip(0.5) then x := 10 else skip *)
  let program =
    Seq (Assign ("x", Int 5),
    Seq (Assign ("y", Int 3),
    Seq (Assign ("z", Add (Var "x", Var "y")),
    Seq (Observe (Var "z"),
    Seq (If (Flip 0.5, Assign ("x", Int 10), Skip),
         Observe (Var "x"))))))
  in

  Printf.printf "=== Interpreter Demo ===\n";
  Printf.printf "Program: x := 5; y := 3; z := x + y; observe z; if flip(0.5) then x := 10 else skip; observe x\n\n";

  (* 初始化随机数种子 *)
  Random.self_init ();

  (* 执行程序 *)
  let initial_env = [] in
  let final_env = eval_stmt initial_env program in

  Printf.printf "\nFinal ";
  print_env final_env

(* ============================================
   解释器实现 (Interpreter Implementation)
   ============================================ *)

open Ast

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
  | Lt (e1, e2) ->                                       (* 小于比较 *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      BoolVal (match (v1, v2) with
        | (IntVal i1, IntVal i2) -> i1 < i2
        | (BoolVal b1, BoolVal b2) -> b1 < b2
        | _ -> failwith "Type error in comparison")
  | Eq (e1, e2) ->                                       (* 等于比较 *)
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      BoolVal (v1 = v2)
  | Flip p ->                                            (* 概率翻转 *)
      BoolVal (Random.float 1.0 < p)

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
      let value = eval_expr env expr in
      Printf.printf "Observed: ";
      (match value with
       | IntVal i -> Printf.printf "%d" i
       | BoolVal b -> Printf.printf "%b" b);
      Printf.printf "\n";
      env  (* 观测不改变环境 *)

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

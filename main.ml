let () =
  let args = Sys.argv in
  let run_interpreter = Array.length args > 1 && args.(1) = "--run" in

  let lexbuf = Lexing.from_channel stdin in
  try
    let ast = Parser.program Lexer.read lexbuf in

    if run_interpreter then begin
      Printf.printf "=== Running Interpreter ===\n";
      Random.self_init ();  (* 初始化随机数种子 *)
      let final_env = Interpreter.eval_stmt [] ast in  (* 空环境开始执行 *)
      Printf.printf "\nFinal ";
      Interpreter.print_env final_env
    end else begin
      print_endline "Parsed AST:";
      print_endline (Ast.string_of_stmt ast)
    end
  with
  | Lexer.Lexing_error msg ->
      Printf.eprintf "Lexer error: %s\n" msg
  | Parser.Error ->
      let pos = Lexing.lexeme_start lexbuf in
      Printf.eprintf "Parser error at position %d (around line %d)\n" pos (Lexing.lexeme_end lexbuf)

let () =
  let lexbuf = Lexing.from_channel stdin in
  try
    let _ast = Parser.program Lexer.read lexbuf in
    print_endline "Parse OK."
  with
  | Lexer.Lexing_error msg ->
      Printf.eprintf "Lexer error: %s\n" msg
  | Parser.Error ->
      Printf.eprintf "Parser error\n"

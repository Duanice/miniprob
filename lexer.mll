{
  open Parser
  exception Lexing_error of string
}

rule read = parse
  | [' ' '\t' '\r' '\n'] { read lexbuf }   (* skip whitespace *)

  | "skip"      { SKIP }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "while"     { WHILE }
  | "do"        { DO }
  | "observe"   { OBSERVE }
  | "true"      { BOOL(true) }
  | "false"     { BOOL(false) }

  | ":="        { ASSIGN }
  | ";"         { SEMI }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "<"         { LT }
  | "=="        { EQ }

  | "("         { LPAREN }
  | ")"         { RPAREN }

  | "flip"      { FLIP }

  | ['0'-'9']+ as i
      { INT (int_of_string i) }

  | ['0'-'9']+ '.' ['0'-'9']+ as f
      { FLOAT (float_of_string f) }

  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
      as id
      { IDENT id }

  | eof         { EOF }

  | _ as c
      { raise (Lexing_error (Printf.sprintf "Unexpected character: %c" c)) }


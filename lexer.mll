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
  | "print"     { PRINT }
  | "printwith" { PRINT_WITH }
  | "true"      { BOOL(true) }
  | "false"     { BOOL(false) }

  | ":="        { ASSIGN }
  | ";"         { SEMI }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "*"         { TIMES }
  | "<"         { LT }
  | ">"         { GT }
  | "=="        { EQ }
  | "&&"        { AND }
  | "||"        { OR }
  | "!"         { NOT }
  | "xor"       { XOR }
  | ","         { COMMA }

  | "("         { LPAREN }
  | ")"         { RPAREN }

  | "flip"      { FLIP }
  | "max"       { MAX }
  | "min"       { MIN }
  | "abs"       { ABS }

  | ['0'-'9']+ as i
      { INT (int_of_string i) }

  | ['0'-'9']+ '.' ['0'-'9']+ as f
      { FLOAT (float_of_string f) }

  | '"' [^ '"']* '"' as s
      { STRING (String.sub s 1 (String.length s - 2)) }

  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
      as id
      { IDENT id }

  | eof         { EOF }

  | _ as c
      { raise (Lexing_error (Printf.sprintf "Unexpected character: %c" c)) }



(* The type of tokens. *)

type token = 
  | XOR
  | WHILE
  | TIMES
  | THEN
  | STRING of (string)
  | SKIP
  | SEMI
  | RPAREN
  | PRINT_WITH
  | PRINT
  | PLUS
  | OR
  | OBSERVE
  | NOT
  | MINUS
  | MIN
  | MAX
  | LT
  | LPAREN
  | INT of (int)
  | IF
  | IDENT of (string)
  | GT
  | FLOAT of (float)
  | FLIP
  | EQ
  | EOF
  | ELSE
  | DO
  | COMMA
  | BOOL of (bool)
  | ASSIGN
  | AND
  | ABS

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.stmt)

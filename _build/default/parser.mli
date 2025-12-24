
(* The type of tokens. *)

type token = 
  | WHILE
  | THEN
  | SKIP
  | SEMI
  | RPAREN
  | PLUS
  | OBSERVE
  | MINUS
  | LT
  | LPAREN
  | INT of (int)
  | IF
  | IDENT of (string)
  | FLOAT of (float)
  | FLIP
  | EQ
  | EOF
  | ELSE
  | DO
  | BOOL of (bool)
  | ASSIGN

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.stmt)

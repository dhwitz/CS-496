
(* The type of tokens. *)

type token = 
  | UNITTYPE
  | TYPE
  | TIMES
  | THEN
  | SETREF
  | SET
  | SEMICOLON
  | RPAREN
  | REFTYPE
  | RBRACE
  | RANGLE
  | PROC
  | PLUS
  | PIPE
  | OF
  | NEWREF
  | MINUS
  | LPAREN
  | LETREC
  | LET
  | LBRACE
  | LANGLE
  | ISZERO
  | INTTYPE
  | INT of (int)
  | IN
  | IF
  | ID of (string)
  | EQUALS
  | EOF
  | END
  | ELSE
  | DIVIDED
  | DEREF
  | DEBUG
  | CONSTRUCTOR of (string)
  | COMMA
  | COLON
  | CASE
  | BOOLTYPE
  | BEGIN
  | ARROW

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.prog)

type token =
  | INT of (int)
  | PLUS
  | MINUS
  | MUL
  | DIV
  | GT
  | LT
  | EQ
  | NE
  | LAM
  | DOT
  | LET
  | IN
  | IF
  | THEN
  | ELSE
  | BOOL of (bool)
  | IDEN of (string)
  | EOL
  | EOF
  | END
  | LP
  | RP
  | ADDL
  | REML
  | GETL
  | COMMA
  | POLICY
  | HIGH
  | LOW

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.expr

{
        open Parser        (* The type token is defined in parser.mli *)
        exception Eof
}

rule token = parse
	  [' ' '\n' '\t']		{ token lexbuf }     (* skip blanks *)
	| '('			{ LP }
	| ')'			{ RP }
	| "policy"		{ POLICY }	(* label *)
	| "high"		{ HIGH }	(* label *)
	| "low"			{ LOW }		(* label *)
	| "lambda"		{ LAM }
	| '.'			{ DOT }
	| "let"			{ LET }
	| '='			{ EQ }
	| "in"			{ IN }
	| "if"			{ IF }
	| "then"		{ THEN }
	| "else"		{ ELSE }
	| "true"|"false" as bool				{ BOOL(bool_of_string bool) }
	| "addLab"		{ ADDL }	(* label *)
	| "remLab"		{ REML }	(* label *)
	| "getLab"		{ GETL }	(* label *)
	| ','			{ COMMA } (* label *)
	| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']* as iden	{ IDEN(iden) }
        | ['0'-'9']+ as num	{ INT(int_of_string num) }
        | '+'            	{ PLUS }
        | '-'            	{ MINUS }
        | '*'            	{ MUL }
        | '/'            	{ DIV }
        | '>'            	{ GT }
        | '<'            	{ LT }
        | "=="            	{ EQ }
        | "!="            	{ NE }
	| ";;"			{ END }
        | eof            	{ raise Eof }


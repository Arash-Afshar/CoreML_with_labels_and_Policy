%{
open Syntax;;
%}

%token <int> INT
%token PLUS MINUS MUL DIV
%token GT LT EQ NE
%token LAM DOT LET IN
%token IF THEN ELSE
%token <bool> BOOL
%token <string> IDEN
%token EOL EOF END
%token LP RP
%token ADDL REML GETL COMMA
%token HIGH LOW

%left MUL DIV
%left PLUS MINUS
%left GT LT EQ NE


%start main             
%type <Syntax.expr> main
%type <Syntax.expr> exper


%%
main:
	exper END	                	{ $1 }
;
exper:
	  ADDL LP exper COMMA exper RP		{ App(App(addLab, $3), $5) }
	| REML LP exper RP			{ App(remLab, $3) }
	| GETL LP exper RP			{ App(getLab, $3) }
	| LP exper RP				{ $2 }
	| HIGH					{ Const {name = Lab "high"; arity = 0; constr = false} }
	| LOW					{ Const {name = Lab "low"; arity = 0; constr = false} }
	| IDEN					{ Var $1 }
	| LAM IDEN DOT exper			{ Fun($2,$4) }
	| exper exper				{ App($1, $2) }
	| INT					{ int ($1) }
	| BOOL					{ bool($1) } 
	| exper PLUS exper			{ App(App(plus, $1), $3) } 
	| exper MINUS exper			{ App(App(minus, $1), $3) } 
	| exper MUL exper			{ App(App(times, $1), $3) } 
	| exper DIV exper			{ App(App(div, $1), $3) } 
	| exper GT exper			{ App(App(gt, $1), $3) }
	| exper LT exper			{ App(App(lt, $1), $3) }
	| exper EQ exper			{ App(App(eq, $1), $3) }
	| exper NE exper			{ App(App(ne, $1), $3) }
	| LET IDEN EQ exper IN exper		{ Let($2, $4, $6) }
	| IF exper THEN exper ELSE exper	{ App(App(App(branch, $2), $4), $6) }
;


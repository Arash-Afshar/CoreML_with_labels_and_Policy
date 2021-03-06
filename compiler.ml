open Syntax;;
open Reduce;;
open Poly;;
open Unify;;

let string_of_name = function
	  Name s -> s
	| Int i -> string_of_int i
	| Bool b -> string_of_bool b
	| Lab l -> l	(* label *)
	| _ -> "Not a name!"


let rec string_of_labelList l=							(* label *)
	if (List.length l)==0 then						(* label *)
		""								(* label *)
	else									(* label *)
		begin match List.hd l with					(* label *)
		  Const {name = Lab label} ->					(* label *)
			let listStr = (string_of_labelList (List.tl l)) in	(* label *)
			if (String.compare listStr "") == 0 then		(* label *)
				label						(* label *)
			else							(* label *)
				label^", "^listStr				(* label *)
		| _ -> "Not a Label!"						(* label *)
		end;;								(* label *)


let rec string_of_answer = function
	  Value v ->
		begin match v with
			  Constant (c, value) ->
				string_of_name c.name
			| Closure (var, ex, env) -> 
				"lambda "^var^"."^"body"
			| LabelValue (value, expList) ->				(* label *)
				let labels = "{"^(string_of_labelList expList)^"}" in	(* label *)
				if (String.compare labels "{}") == 0 then		(* label *)
					string_of_answer(Value value)			(* label *)
				else							(* label *)
					string_of_answer(Value value)^labels		(* label *)
		end
	| Error -> 
		"Not a value!";;


let string_of_abstract_name = function
	  Name s -> "Name \""^s^"\""
	| Int i -> "Int "^(string_of_int i)
	| Bool b -> "Bool "^(string_of_bool b)
	| Lab l -> "Lab \""^l^"\""	(* label *)
	| _ -> "Not a name!"


let rec string_of_abstract_expr = function
	  Var v -> "Var \""^v^"\""
	| Const c ->
		"Const { name = "^(string_of_abstract_name c.name)^"; constr = "^(string_of_bool c.constr)^"; arity = "^(string_of_int c.arity)^"}"
	| Fun (v, e) ->
		"Fun (\""^v^"\" , "^string_of_abstract_expr(e)^")"
	| App (e1, e2) ->
		"App( "^(string_of_abstract_expr e1)^" , "^(string_of_abstract_expr e2)^")"
	| Let (v, e1, e2) ->
		"Let ( \""^v^"\" , "^string_of_abstract_expr(e1)^" , "^string_of_abstract_expr(e2)^")"
	| _ -> "Not an expression!"


let rec string_of_concrete_expr = function
	  Var v -> v
	| Const c ->
		string_of_name c.name
	| Fun (v, e) ->
		"lambda "^v^"."^(string_of_concrete_expr e)
	| App (e1, e2) ->
		string_of_concrete_expr(e1)^" "^string_of_concrete_expr(e2)
	| Let (v, e1, e2) ->
		"let "^v^"="^string_of_concrete_expr(e1)^" in "^string_of_concrete_expr(e2)
	| _ -> "Not an expression!"



let rec string_of_texp t= 
	let t =repr t in
	match desc t with
	  Tvar var -> "x"^string_of_int(var)
	| Tcon (con, l) ->
		begin match con, l with
		  Tarrow , l -> "("^string_of_texp(List.hd(l))^" -> "^string_of_texp(List.hd(List.tl(l)))^")"
		| Tint , l -> "int"
		| Tbool , l -> "bool"
		end		
;;



let _ =
	try
		let lexbuf = Lexing.from_channel (open_in Sys.argv.(1) ) in
		while true do
			let result = Parser.main Lexer.token lexbuf in
			(*print_string(string_of_texp(type_of result)); print_newline(); *)
			print_string(string_of_concrete_expr(result));  print_newline();
			print_string(string_of_abstract_expr(result));  print_newline();
			print_string(string_of_answer(eval [] result)); print_newline();
			flush stdout
		done
	with Lexer.Eof ->
		exit 0;;



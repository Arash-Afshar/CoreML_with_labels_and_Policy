open Syntax;;
open Reduce;;
(*open Poly;;
open Unify;;*)
open InferDefs;;

(* =============================================== print abstract syntax =============================================== *)

let rec string_of_abstract_varList vl=
	if (List.length vl)==1 then
		"\""^(List.hd vl)^"\""
	else
		"\""^(List.hd vl)^"\"; "^string_of_abstract_varList (List.tl vl);;


let string_of_abstract_name = function
	  Name s -> "Name \""^s^"\""
	| Int i -> "Int "^(string_of_int i)
	| Bool b -> "Bool "^(string_of_bool b)
	| Lab l -> "Lab \""^l^"\""
	| _ -> "Not a name!"

let rec string_of_abstract_expr = function
	  Var v ->
		"Var \""^v^"\""
	| Const c ->
"Const { name = "^(string_of_abstract_name c.name)^"; constr = "^(string_of_bool c.constr)^"; arity = "^(string_of_int c.arity)^"}"
	| Fun (v, e) ->
		"Fun (\""^v^"\" , "^string_of_abstract_expr(e)^")"
	| App (e1, e2) ->
		"App( "^(string_of_abstract_expr e1)^" , "^(string_of_abstract_expr e2)^")"
	| Let (v, e1, e2) ->
		"Let ( ["^(string_of_abstract_varList v)^"] , "^string_of_abstract_expr(e1)^" , "^string_of_abstract_expr(e2)^")"
	| LetP (v, e1, e2) ->
		"LetP ( ["^(string_of_abstract_varList v)^"] , "^string_of_abstract_expr(e1)^" , "^string_of_abstract_expr(e2)^")"
	| _ -> "Not an expression!"

let print_abstract_expr ae = print_string ((string_of_abstract_expr ae)^";;");;

(* =============================================== print concrete syntax =============================================== *)

let string_of_nameValue = function
	  Name s -> s
	| Int i -> string_of_int i
	| Bool b -> string_of_bool b
	| Lab l -> l
	| _ -> "Not a name!"

let rec string_of_varList vl=
	if (List.length vl)==1 then
		(List.hd vl)
	else
		(List.hd vl)^" "^string_of_varList (List.tl vl);;

let rec string_of_concrete_expr = function
	  Var v ->
		v
	| Const c ->
		string_of_nameValue c.name
	| Fun (v, e) ->
		"(lambda "^v^"."^(string_of_concrete_expr e)^")"
	| App (App (Const {name = Name "+";  arity = 2; constr = false}, e1), e2) ->
		"("^string_of_concrete_expr(e1)^" + "^string_of_concrete_expr(e2)^")"
	| App (App (Const {name = Name "-";  arity = 2; constr = false}, e1), e2) ->
		"("^string_of_concrete_expr(e1)^" - "^string_of_concrete_expr(e2)^")"
	| App (App (Const {name = Name "*";  arity = 2; constr = false}, e1), e2) ->
		"("^string_of_concrete_expr(e1)^" * "^string_of_concrete_expr(e2)^")"
	| App (App (Const {name = Name "/";  arity = 2; constr = false}, e1), e2) ->
		"("^string_of_concrete_expr(e1)^" / "^string_of_concrete_expr(e2)^")"
	| App (App (Const {name = Name ">";  arity = 2; constr = false}, e1), e2) ->
		"("^string_of_concrete_expr(e1)^" > "^string_of_concrete_expr(e2)^")"
	| App (App (Const {name = Name "<";  arity = 2; constr = false}, e1), e2) ->
		"("^string_of_concrete_expr(e1)^" < "^string_of_concrete_expr(e2)^")"
	| App (App (Const {name = Name "==";  arity = 2; constr = false}, e1), e2) ->
		"("^string_of_concrete_expr(e1)^" == "^string_of_concrete_expr(e2)^")"
	| App (App (Const {name = Name "!=";  arity = 2; constr = false}, e1), e2) ->
		"("^string_of_concrete_expr(e1)^" != "^string_of_concrete_expr(e2)^")"
	| App (App (App (Const {name = Name "branch";  arity = 2; constr = false}, e1), e2), e3) ->
		"("^"if ("^string_of_concrete_expr(e1)^") then "^string_of_concrete_expr(e2)^" else "^string_of_concrete_expr(e3)^")"
	| App (e1, e2) ->
		"("^string_of_concrete_expr(e1)^" "^string_of_concrete_expr(e2)^")"
	| Let  (v, e1, e2) ->
		"let "^(string_of_varList v)^" = "^string_of_concrete_expr(e1)^" in "^string_of_concrete_expr(e2)
	| LetP (v, e1, e2) ->
		"let policy "^(string_of_varList v)^" = "^string_of_concrete_expr(e1)^" in "^string_of_concrete_expr(e2)
	| _ ->
		"Not an expression!"


let print_concrete_expr ce = print_string ((string_of_concrete_expr ce)^";;");;

(* =============================================== print answer =============================================== *)

let rec string_of_labelList l=
	if (List.length l)==0 then
		""
	else
		begin match List.hd l with
		  Const {name = Lab label} ->
			let listStr = (string_of_labelList (List.tl l)) in
			if (String.compare listStr "") == 0 then
				label
			else
				label^", "^listStr
		| _ -> "Not a Label!"
		end;;

(*
let string_of_env = function
	(string * value) list ->
	| _ ->
		"Not an environment";;
*)


let rec string_of_answer = function
	  Value v ->
		begin match v with
		  Constant (c, valueList) ->
			(string_of_nameValue c.name)(* ^" with "^(string_of_env valueList) *)
		| Closure (var, ex, color, env) -> 
			"(lambda "^var^"."^(string_of_concrete_expr ex)^") with color "^color
		| LabelValue (value, expList) ->
			let labels = "{"^(string_of_labelList expList)^"}" in
			if (String.compare labels "{}") == 0 then
				string_of_answer(Value value)
			else
				string_of_answer(Value value)^labels
		| _ ->
			"Not a value!"
		end
	| _ -> 
		"Not an answer!";;

let print_answer a = print_string (string_of_answer a);;

(* =============================================== print type =============================================== *)

let rec string_of_short_texp t=
	match tdesc (Tnode t) with
	  Tvar var -> "a"^string_of_int(var)
	| Tcon (symb, l) -> 
		begin match symb, l with
		  Tint, _ -> "int"
		| Tbool, _ -> "bool"
		| Tlab, _ -> "lab"
		| Tarrow , l -> "( "^string_of_short_texp(List.hd(l))^" -> "^string_of_short_texp(List.hd(List.tl(l)))^" )"
		end
	| Tlabled (t, e) ->
		begin match edesc (Enode e) with
		  Evar var -> (string_of_short_texp t)^"{ "^("x"^string_of_int var)^" }"
		| Econ e   -> (string_of_short_texp t)^"{ "^(string_of_concrete_expr e)^" }"
		end
	| _ -> "Not an texp!"
;;

let print_type t = print_string (string_of_short_texp t);;

(* =============================================== print expression type =============================================== *)
(*
let rec string_of_expTypePair etPair =
	match etPair with
	  ETempty     -> ""
	| ETnode node ->
		"("^(string_of_concrete_expr (node.etExp))^") : "^(string_of_short_texp node.etType)^"\n";
		string_of_expTypePair (node.etLink)
;;

let print_expType et = print_string ("["^(string_of_expTypePair et)^"]")
*)
(* =============================================== print constraint set =============================================== *)

let rec string_of_constraint c =
	match c with
	  { texp_node = Tvar var; tlink_node = Tempty; tmark = 0 } -> 
		begin match var with
		  -1 -> ""
		| _  -> "a"^string_of_int(var)
		end
	| { texp_node = Tcon (symb, l); tlink_node = Tempty; tmark = 0 } -> 
		begin match symb, l with
		  Tint, _ -> "int"
		| Tbool, _ -> "bool"
		| Tlab, _ -> "lab"
		| Tarrow , l -> 
			let t1 = List.hd(l) in
			let t2 = List.hd(List.tl(l)) in
				"( "^string_of_constraint(t1)^" -> "^string_of_constraint(t2)^" )"
		end
	| { texp_node = Tlabled (t, e); tlink_node = Tempty; tmark = 0 } ->
		begin match edesc (Enode e) with
		  Evar var -> (string_of_constraint t)^"{ "^("x"^string_of_int var)^" }"
		| Econ e   -> (string_of_constraint t)^"{ "^(string_of_concrete_expr e)^" }"
		end
	| { texp_node = d; tlink_node = Tnode u; tmark = 0 } ->
		(string_of_constraint (texp d))^" = "^(string_of_constraint u)
	| _ -> "Not an texp!"
;;

let rec string_of_constraint_set cs =
	match cs with
	  Cempty  -> ""
	| Cnode c -> 
		(string_of_constraint c.cnstrnt)^", \n"^(string_of_constraint_set c.link)
;;

let print_constraint_set cs =
	let res = (string_of_constraint_set cs) in
	print_string ("[ "^(String.sub res 2 ((String.length res)-4) )^" ]");;
	(*print_string res;;*)

(* =============================================== print substitution set =============================================== *)

let string_of_subs (te1, te2) =
	"["^(string_of_short_texp (texp te1))^" ==> "^(string_of_short_texp (texp te2))^"]\n";;

let print_subsSet sl =
	List.map print_string (List.map string_of_subs sl);;





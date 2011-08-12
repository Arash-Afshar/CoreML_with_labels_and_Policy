open Syntax;;

type env = (string * value) list
and value = | Closure of var * expr * color * env | Constant of constant * value list | LabelValue of value * expr list;; (* label *)

exception AnswerError of string
type answer = Value of value;;

let val_int u =
	Value (Constant ({name = Int u; arity = 0; constr = true}, []));;

let val_bool u =
	Value (Constant ({name = Bool u; arity = 0; constr = true}, []));;


let rec extractLab v =			(* label *)
	match v with			(* label *)
	  Closure (_, _, _, _) -> []	(* label *)
	| Constant (_, _) -> []		(* label *)
	| LabelValue(va, el) -> 	(* label *)
		(extractLab va) @ el;;	(* label *)

let rec exctract_val v=			(* label *)
	match v with			(* label *)
	  LabelValue(va, el) ->		(* label *)
		exctract_val va		(* label *)
	| _ ->				(* label *)
		v;;			(* label *)


let val_addLab v lab =										(* label *)
	match lab with										(* label *)
	  Const {name = Lab l; arity = 0; constr = false} ->					(* label *)
		if (String.compare l "noLab") == 0 then						(* label *)
			Value v									(* label *)
		else										(* label *)
			begin match v with							(* label *)
			 Closure (var, exp, c, env) ->						(* label *)
				Value(LabelValue(v,(lab::[])))					(* label *)
			| Constant (c, vl) ->							(* label *)
				Value(LabelValue(v,(lab::[])))					(* label *)
			| LabelValue (va, el) ->						(* label *)
				Value(LabelValue((exctract_val v), (lab::(extractLab v))))	(* label *)
			| _ ->									(* label *)
				raise (AnswerError "In addLab: the expression is not a value")	(* label *)
			end									(* label *)
	| _ ->											(* label *)
		raise (AnswerError "In addLab: not a correct label");;				(* label *)

let val_remLab v =								(* label *)
	match v with								(* label *)
	  Closure (_, _, _, _) ->							(* label *)
		Value v								(* label *)
	| Constant (_, _) ->							(* label *)
		Value v								(* label *)
	| LabelValue (va, el) ->						(* label *)
		if el == [] then						(* label *)
			Value(LabelValue(va, []))				(* label *)
		else								(* label *)
			Value(LabelValue(va, List.tl el))			(* label *)
	| _ ->									(* label *)
		raise (AnswerError "In remLab: can't remove label from a non-value!");;	(* label *)


let val_getLab v =										(* label *)
	match v with										(* label *)
	  Closure (_, _, _, _) ->									(* label *)
		Value (Constant ({name = Lab "noLab"; arity = 0; constr = false}, []))	(* label *)
	| Constant (_, _) ->									(* label *)
		Value (Constant ({name = Lab "noLab"; arity = 0; constr = false}, []))	(* label *)
	| LabelValue (va, el) ->								(* label *)
		if el == [] then								(* label *)
			Value (Constant ({name = Lab "noLab"; arity = 0; constr = false}, []))(* label *)
		else										(* label *)
			match List.hd el with							(* label *)
			 Const c ->								(* label *)
				Value (Constant (c, []))					(* label *)
			| _ ->									(* label *)
				raise (AnswerError "In getLab: The top label is not a label!")	(* label *)
	| _ ->											(* label *)
		raise (AnswerError "In getLab: can't get label from a non-value");;		(* label *)


let rec create_lam_expr varList expr =
	if (List.length varList) == 1 then
		Fun (List.hd varList, expr)
	else
		Fun (List.hd varList, create_lam_expr (List.tl varList) expr);;


let delta c l color=
	match c.name, l with
	  Name "+", [ Constant ({name=Int second}, []); Constant ({name=Int first}, [])] ->
		val_int (first + second)
	| Name "-", [ Constant ({name=Int second}, []); Constant ({name=Int first}, [])] ->
		val_int (first - second)
	| Name "*", [ Constant ({name=Int second}, []); Constant ({name=Int first}, [])] ->
		val_int (first * second)
	| Name "/", [ Constant ({name=Int second}, []); Constant ({name=Int first}, [])] ->
		val_int (first / second)
	| Name ">", [ Constant ({name=Int second}, []); Constant ({name=Int first}, [])] ->
		val_bool (first > second)
	| Name "<", [ Constant ({name=Int second}, []); Constant ({name=Int first}, [])] ->
		val_bool (first < second)
	| Name "==", [ Constant ({name=Int second}, []); Constant ({name=Int first}, [])] ->
		val_bool (first == second)
	| Name "==", [ Constant ({name=Lab second}, []); Constant ({name=Lab first}, [])] ->
		let res = String.compare first second in
		if res == 0 then
			val_bool (true)
		else
			val_bool (false)
	| Name "!=", [ Constant ({name=Int second}, []); Constant ({name=Int first}, [])] ->
		val_bool (first != second)
	| Name "branch", [ third; second; Constant ({name=Bool first}, [])] ->
		if first then
			Value second
		else
			Value third
	| Name "addLab", [ second; Constant ({name=Lab first}, [])] ->					(* label *)
		if (String.compare color "policy") == 0 then						(* label *)
			let labConstant = Const {name = Lab first; arity = 0; constr = false} in	(* label *)
			val_addLab second labConstant							(* label *)
		else											(* label *)
			raise (AnswerError "In delta: can't add label while color is code!")		(* label *)
	| Name "remLab", [ first ] ->									(* label *)
		if (String.compare color "policy") == 0 then						(* label *)
			val_remLab first								(* label *)
		else											(* label *)
			raise (AnswerError "In delta: can't remove label while color is code!")		(* label *)
	| Name "getLab", [ first ] ->									(* label *)
		if (String.compare color "policy") == 0 then						(* label *)
			val_getLab first								(* label *)
		else											(* label *)
			raise (AnswerError "In delta: can't get label while color is code!")		(* label *)
	| _ ->
		raise (AnswerError "In delta: unknow constant!");;


let get x env =
	try Value (List.assoc x env) with Not_found -> raise (AnswerError "In get: variable is not in the environment");;

let rec eval env color = function
	  Var x -> get x env
	| Const c -> Value (Constant (c, []))
	| Fun (x, a) -> Value (Closure (x, a, color, env))
	| Let (x, a1, a2) ->
		let funcBody = create_lam_expr (List.tl x) a1 in
		begin match eval env color funcBody with
		| Value v1 ->
			let newEnv = (List.hd x, v1)::env in
			eval newEnv color a2
		| _ -> raise (AnswerError "In eval: function definition in let is not a value!")
		end
	| LetP (x, a1, a2) ->
		let funcBody = create_lam_expr (List.tl x) a1 in
		begin match eval env "policy" funcBody with
		| Value v1 ->
			let newEnv = (List.hd x, v1)::env in
			eval newEnv color a2
		| _ -> raise (AnswerError "In eval: function definition in letP is not a value!")
		end
	| App (a1, a2) ->
		begin match eval env color a1 with
		| Value v1 ->
			begin match v1, eval env color a2 with
			| Constant (c, l), Value v2 ->
				let k = List.length l + 1 in
				if c.arity < k then raise (AnswerError "In eval: constant arity is less than expected!")
				else if c.arity > k then Value (Constant (c, v2::l))
				else if c.constr then Value (Constant (c, v2::l))
				else delta c (v2::l) color
			| Closure (x, e, c, env0), Value v2 ->
				eval ((x, v2) :: env0) c e
			| LabelValue (_, _), _ ->
				raise (AnswerError "In eval: label value can't be applied!")
			| _, _ ->
				raise (AnswerError "In eval: applicant is not a value!")
			end
		| _ -> raise (AnswerError "In eval: Application construct is not a value!")
		end
	| _ -> raise (AnswerError "In eval: eval input is not an expr!") ;;


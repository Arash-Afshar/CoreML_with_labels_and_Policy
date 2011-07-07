open Syntax;;

type env = (string  * value) list 
and value =  | Closure of var  * expr * env  | Constant of constant  * value list | LabelValue of value * expr list;; (* label *)

type answer = Error  | Value of value;;

let val_int  u =
	Value (Constant ({name = Int  u; arity = 0; constr = true}, []));; 

let val_bool u =
	Value (Constant ({name = Bool u; arity = 0; constr = true}, []));; 


let rec extractLab v =			(* label *)
	match v with			(* label *)
	  Closure  (_, _, _)    -> []	(* label *)
	| Constant (_, _)    -> []	(* label *)
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
	  Const {name = Lab l;   arity = 0; constr = false} ->					(* label *)
		if (String.compare l "noLab") == 0 then						(* label *)
			Value v									(* label *)
		else										(* label *)
			begin match v with							(* label *)
			  Closure (var, exp, env) ->						(* label *)
				Value(LabelValue(v,(lab::[])))					(* label *)
			| Constant (c, vl) ->							(* label *)
				Value(LabelValue(v,(lab::[])))					(* label *)
			| LabelValue (va, el) ->						(* label *)
				Value(LabelValue((exctract_val v), (lab::(extractLab v))))	(* label *)
			| _ ->									(* label *)
				Error								(* label *)
			end									(* label *)
	| _ ->											(* label *)
		Error;;										(* label *)

let val_remLab v =								(* label *)
	match v with								(* label *)
	  Closure (_, _, _) ->							(* label *)
		Value v								(* label *)
	| Constant (_, _) ->							(* label *)
		Value v								(* label *)
	| LabelValue (va, el) ->						(* label *)
		if el == [] then						(* label *)
			Value(LabelValue(va, []))				(* label *)
		else								(* label *)
			Value(LabelValue(va, List.tl el))			(* label *)
	| _ ->									(* label *)
		Error;;								(* label *)
	

let val_getLab v =										(* label *)
	match v with										(* label *)
	  Closure (_, _, _) ->									(* label *)
		Value (Constant ({name = Lab "noLab";   arity = 0; constr = false}, []))	(* label *)
	| Constant (_, _) ->									(* label *)
		Value (Constant ({name = Lab "noLab";   arity = 0; constr = false}, []))	(* label *)
	| LabelValue (va, el) ->								(* label *)
		if el == [] then								(* label *)
			Value (Constant ({name = Lab "noLab";   arity = 0; constr = false}, []))(* label *)
		else										(* label *)
			match List.hd el with							(* label *)
			  Const c ->								(* label *)
				Value (Constant (c, []))					(* label *)
			| _ ->									(* label *)
				Error								(* label *)
	| _ ->											(* label *)
		Error;;										(* label *)


let delta c  l =
	match c.name, l with
	| Name "+", [ Constant ({name=Int second}, []); Constant ({name=Int  first}, [])] ->
		val_int (first + second)
	| Name "-", [ Constant ({name=Int second}, []); Constant ({name=Int  first}, [])] ->
		val_int (first - second)
	| Name "*", [ Constant ({name=Int second}, []); Constant ({name=Int  first}, [])] ->
		val_int (first * second)
	| Name "/", [ Constant ({name=Int second}, []); Constant ({name=Int  first}, [])] ->
		val_int (first / second)
	| Name ">", [ Constant ({name=Int second}, []); Constant ({name=Int  first}, [])] ->
		val_bool (first > second)
	| Name "<", [ Constant ({name=Int second}, []); Constant ({name=Int  first}, [])] ->
		val_bool (first < second)
	| Name "==", [ Constant ({name=Int second}, []); Constant ({name=Int  first}, [])] ->
		val_bool (first == second)
	| Name "==", [ Constant ({name=Lab second}, []); Constant ({name=Lab  first}, [])] ->
		let res = String.compare first second in
		if res == 0 then
			val_bool (true)
		else
			val_bool (false)
	| Name "!=", [ Constant ({name=Int second}, []); Constant ({name=Int  first}, [])] ->
		val_bool (first != second)
	| Name "branch", [ Constant ({name=Int third}, []); Constant ({name=Int second}, []); Constant ({name=Bool first}, [])] ->
		let res =
			if first then second else third in
			val_int res
	| Name "branch", [ Constant ({name=Bool third}, []); Constant ({name=Bool second}, []); Constant ({name=Bool first}, [])] ->
		let res =
			if first then second else third in
			val_bool res
	| Name "addLab", [ second; Constant ({name=Lab first}, [])] ->				(* label *)
		let labConstant = Const {name = Lab first; arity = 0; constr = false} in	(* label *)
		val_addLab second labConstant							(* label *)
	| Name "remLab", [ first ] ->								(* label *)
		val_remLab first								(* label *)
	| Name "getLab", [ first ] ->								(* label *)
		val_getLab first								(* label *)
	| _ ->
		Error;;


let get x  env =
	try Value (List.assoc x  env) with Not_found  -> Error;;

let rec eval env = function
	| Var x -> get x env  
	| Const c -> Value  (Constant (c, []))  
	| Fun (x, a) -> Value (Closure (x, a, env))  
	| Let (x, a1, a2) ->  
		begin match eval  env a1 with  
		| Value v1 -> eval  ((x, v1)::env) a2  
		| Error -> Error  
		end  
	| App (a1, a2) ->  
		begin match eval  env a1 with  
		| Value v1 ->  
			begin match v1, eval env a2  with  
			| Constant  (c, l), Value  v2 ->  
				let  k = List.length  l + 1 in  
				if  c.arity  < k then Error  
				else  if c.arity  > k then Value (Constant (c, v2::l))  
				else  if c.constr  then Value (Constant  (c, v2::l))  
				else  delta c (v2::l)  
			| Closure (x, e, env0), Value v2  ->  
				eval ((x, v2) :: env0) e
			| LabelValue (_, _), _ ->
				Error
			| _, Error -> Error
			end  
		| Error -> Error  
		end  
	| _ -> Error ;;


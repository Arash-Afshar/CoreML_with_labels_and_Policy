open Syntax;;
open ConstraintUnify;;
open PrettyPrinter;;
open InferDefs;;
(*open Typescheme;;*)


exception Undefined_constant of string
let type_of_const c t setC =
let int3 = tarrow tint (tarrow tint tint) in
	let param = tvar() in
	let bool3 = tarrow param (tarrow param tbool) in
	match c.name  with
	| Int _ -> link t tint; addC t setC
	| Bool _ -> link t tbool; addC t setC
	| Name ("+" | "-" | "*" | "/") -> link t int3; addC t setC
	| Name (">" | "<" | "==" | "!=") -> link t bool3; addC t setC
	| Name ("branch") ->
		let branch =
			let br1 = tvar() in
			let br2 = tvar() in
			let res = tvar() in
			link br1 br2; link br2 res; link t (tarrow tbool (tarrow br1 (tarrow br2 res))); addC br1 setC in
		branch
	| Name ("addLab") ->
			let add =
			let t1   = tvar() in
			let e   = evar() in  (* fixme: how to say exp has type lab??? *)
			let lab = tlab in
			let res=(tlabled t1 e) in
			link t (tarrow lab (tarrow t1 res)); addC t setC in
		add
	| Name ("remLab") ->
			let rem =
			let t1  = tvar() in
			let e   = evar() in
			let t2   = tlabled t1 e in
			link t (tarrow t2 t1); addC t setC in
		rem
	| Name ("getLab") ->
			let get =
			let t1  = tvar() in
			let e   = evar() in
			let t2  = tlabled t1 e in
			link t (tarrow t2 tlab); addC t setC in
		get
	| Lab ("noLab") -> link t (tvar()); addC t setC
	| Lab ("high") -> link t tlab; addC t setC
	| Lab ("low") -> link t tlab; addC t setC
	| Name n -> raise  (Undefined_constant n)
	| Lab l -> raise  (Undefined_constant l);;


exception Free_variable  of var
let type_of_var tenv x t setC =
	try link t (List.assoc x tenv); addC t setC
	with Not_found -> link t (tvar()); addC t setC;;

let extend tenv (x, t) = (x, t)::tenv;;

(* =================================== poly infer ================================== *)

let rec create_lam_expr varList expr =
	let length = (List.length varList) in
	if (length == 0) then
		expr
	else if (length == 1) then
		Fun (List.hd varList, expr)
	else
		Fun (List.hd varList, create_lam_expr (List.tl varList) expr);;


let rec constraintGen tenv e t setC (*(ETnode expType)*) =
	(print_concrete_expr e); print_string(" : "); (print_type t); print_newline();
	(*expType.etLink <- ETnode (expTypePair e t);*)
	match e with
	| Var x -> type_of_var tenv x t setC
	| Const c -> type_of_const c t setC
	| Fun (x, e) ->
		let targ = tvar() and tbody = tvar() in
		let setC1 = constraintGen (extend tenv (x, targ)) e tbody setC  in
		link t (tarrow targ tbody); addC t setC
	| App (e1, e2) ->
		let te1 = tvar() and te2 = tvar() in
		let setC1 = constraintGen tenv e1 te1 setC  in
		let setC2 = constraintGen tenv e2 te2 setC  in
		link te1 (tarrow te2 t); addC te1 setC
	| Let (x, e1, e2) ->
		let tfunc = tvar() and te1 = tvar() and te2 = tvar() in
		let func = create_lam_expr (List.tl x) e1 in
		constraintGen tenv func te1 setC;
		constraintGen (extend tenv (List.hd x, tfunc)) e2 te2 setC;
		link tfunc te1; link t te2; addC tfunc setC; addC t setC
	| LetP (x, e1, e2) ->
		let tfunc = tvar() and te1 = tvar() and te2 = tvar() in
		let func = create_lam_expr (List.tl x) e1 in
		constraintGen tenv func te1 setC;
		constraintGen (extend tenv (List.hd x, tfunc)) e2 te2 setC;
		link tfunc te1; link t te2; addC tfunc setC; addC t setC

let genConstrSet e t cs (*etPair*) = constraintGen [] e t cs (*etPair*);;


(*
let constraintGen e setC = 
	match e with
	| Var x -> unify (type_instance (type_of_var tenv  x)) t
	| Const c -> unify (type_instance (type_of_const c)) t
	| Fun (x, a) ->
		let  tv1 = tvar() and  tv2 = tvar() in
		infer (extend tenv  (x, ([], tv1))) a  tv2;
		unify t (tarrow  tv1 tv2)
	| App (a1, a2) ->
		let  tv = tvar() in
		infer tenv a1  (tarrow tv t);
		infer tenv a2  tv
	| Let (x, a1, a2) ->
		let  tv = tvar() in
		infer tenv a1  tv;
		let  s = generalizable tenv tv, tv in
		infer (extend tenv (List.hd x, s)) a2  t
	| LetP (x, a1, a2) ->
		let  tv = tvar() in
		infer tenv a1  tv;
		let  s = generalizable tenv tv, tv in
		infer (extend tenv (List.hd x, s)) a2  t;;
*)

(* NOTICE: SIn the paper, I have defined alpha to be type variable. Therefore, by definition alpha cannot match to somthing like t{e}. But here, evar() can match both types of the form t and t{e}. Thus, tevar() is created! *)
open Syntax;;
open ConstraintUnify;;
open PrettyPrinter;;
open InferDefs;;
(*open Typescheme;;*)


exception Undefined_constant of string
let type_of_const c t setC =
let int3 = (tlabeled (tarrow (tlabeled tint nol) (tlabeled (tarrow (tlabeled tint nol) (tlabeled tint nol)) nol)) nol) in
	let param = (tlabeled (tvar()) nol) in
	let bool3 = (tlabeled (tarrow param (tlabeled (tarrow param (tlabeled tbool nol)) nol)) nol) in
	match c.name  with
	| Int _ ->  link t (tlabeled tint (evar())); addC t setC (* T-INT *)
	| Bool _ -> link t (tlabeled tbool (evar())); addC t setC (* T-BOOL *)
	| Name ("+" | "-" | "*" | "/") -> link t int3; addC t setC (* T-SUM, T-MINUS, T-PLUS, T-DIV *)
	| Name (">" | "<" | "==" | "!=") -> link t bool3; addC t setC (* T-GT, T-LT, T-EQ, T-NE *)
	| Name ("branch") -> (* T-BRANCH *)
		let branch =
			let tAll = tevar() in
			link t (tlabeled (tarrow (tlabeled tbool nol) (tlabeled (tarrow tAll (tlabeled (tarrow tAll tAll) nol)) nol)) nol); addC t setC in
		branch
	| Name ("addLab") -> (* T-RELAB *)
		let add =
			let e1			 = evar() in
			let te1			 = tlabeled (tlab e1) nol in
			let te2			 = tevar() in
			let tres		 = (tlabeled te2 e1) in
			link t (tlabeled (tarrow te1 (tlabeled (tarrow te2 tres) nol)) nol);
			addC t setC in
		add
	| Name ("remLab") -> (* T-UNLAB *)
		let rem =
			let e2		= evar() in
			let tPartOf_te1 = tvar() in
			let te1		= tlabeled tPartOf_te1 e2 in
			link t (tlabeled (tarrow te1 tPartOf_te1) nol); addC t setC in
		rem
	| Name ("getLab") -> (* T-GETLAB *)
		let get =
			let e2	 = evar() in
			let te1	 = tlabeled (tvar()) e2 in
			let tres = (tlabeled (tlab e2) nol) in
			link t (tlabeled (tarrow te1 tres) nol); addC t setC in
		get
	| Lab ("noLab") -> link t (tevar()); addC t setC
	| Lab ("high") -> link t (tlabeled (tlab (eexp (Econ high))) (evar())); addC t setC (* T-HIGH *)
	| Lab ("low") -> link t (tlabeled (tlab (eexp (Econ low))) (evar())); addC t setC (* T-LOW *)
	| Name n -> raise  (Undefined_constant n)
	| Lab l -> raise  (Undefined_constant l);;


exception Free_variable  of var
let type_of_var tenv x t setC =
	try link t (List.assoc x tenv); addC t setC (* T-existingVAR *)
	with Not_found -> link t (tevar()); addC t setC;; (* T-newVAR *)

let extend tenv (x, t) = (x, t)::tenv;;

let rec create_lam_expr varList expr =
	let length = (List.length varList) in
	if (length == 0) then
		expr
	else if (length == 1) then
		Fun (List.hd varList, expr)
	else
		Fun (List.hd varList, create_lam_expr (List.tl varList) expr);;

let rec getAllLabels t =
	match t.texp_node with
		  Tlabeled(t2,e) ->
			begin match e with
			  Eempty ->
				[] (* fixme: Error *)
			| Enode enode ->
				enode::(getAllLabels t2)
			end
		| _ ->
			[]
;;

let rec assertEqual labelList label =
	let length = (List.length labelList) in
	if (length != 0) then
	begin
		elink (List.hd labelList) label;
		assertEqual (List.tl labelList) label
	end
;;

(* =================================== poly infer ================================== *)

let rec constraintGen tenv e t setC (*(ETnode expType)*) =
	(print_concrete_expr e); print_string(" : "); (print_type t); print_newline();
	(*expType.etLink <- ETnode (expTypePair e t);*)
	match e with
	| Var x -> type_of_var tenv x t setC
	| Const c -> type_of_const c t setC
	| Fun (x, e) -> (* T-ABS *)
		let tX = tevar() and tbody = tevar() in
		let setC1 = constraintGen (extend tenv (x, tX)) e tbody setC  in
		link t (tlabeled (tarrow tX tbody) nol); addC t setC
	| App (e1, e2) -> (* T-APP *)
		let te1 = tevar() and te2 = tevar() in
		let setC1 = constraintGen tenv e1 te1 setC  in
		let setC2 = constraintGen tenv e2 te2 setC  in
		let labelList = getAllLabels te1 in
		let _ = assertEqual labelList nol in
		link te1 (tlabeled (tarrow te2 t) nol); addC te1 setC
	| Let (x, e1, e2) -> (* T-LET *) (* fixme: In paper, add the case for [let a b c ... = e1 in e2] *)
		let tfunc = tevar() and te1 = tevar() and te2 = tevar() in
		let func = create_lam_expr (List.tl x) e1 in
		constraintGen tenv func te1 setC;
		constraintGen (extend tenv (List.hd x, tfunc)) e2 te2 setC;
		link tfunc te1; link t te2; addC tfunc setC; addC t setC
	| LetP (x, e1, e2) ->
		let tfunc = tevar() and te1 = tevar() and te2 = tevar() in
		let func = create_lam_expr (List.tl x) e1 in
		constraintGen tenv func te1 setC;
		constraintGen (extend tenv (List.hd x, tfunc)) e2 te2 setC;
		link tfunc te1; link t te2; addC tfunc setC; addC t setC

let genConstrSet e t cs (*etPair*) = constraintGen [] e t cs (*etPair*);;



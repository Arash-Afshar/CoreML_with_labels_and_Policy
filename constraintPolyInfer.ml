(* NOTICE: In the paper, I have defined alpha to be type variable.         *)
(* Therefore, by definition alpha cannot match to somthing like t{e}. But  *)
(* here, evar() can match both types of the form t and t{e}. Thus, tevar() *)
(* is created! fixme: there is a problem! in T-UNLAB, if the type has only *)
(* one lable, the resulting type would have literally no label. Therefore, *)
(* it would not match expressions with NOL labels. this sitution can       *)
(* happen in other places to! So, the problem is how to match NOLs with    *)
(* really no labels and also how to match these two types which are        *)
(* actually equal: t{NOL}{l1}{NOL}{l2}{l3} and t{l1}{l2}{l3} or t{NOL} and *)
(* t solution: In remLab, I will always add nol, then after constraint     *)
(* generation finishes, in unification part, I will omit nol labels just   *)
(* like the way I delete equal types.                                      *)
open Syntax;;
open ConstraintUnify;;
open PrettyPrinter;;
open InferDefs;;
open Aux;;

exception Undefined_constant of string
exception PolicyError of string
let type_of_const color c t setC iNodeRoot =
	let int3 = (tlabeled (tarrow (tlabeled tint nol) (tlabeled (tarrow (tlabeled tint nol) (tlabeled tint nol)) nol)) nol) in
	let param = (tlabeled (tvar()) nol) in
	let bool3 = (tlabeled (tarrow param (tlabeled (tarrow param (tlabeled tbool nol)) nol)) nol) in
	match c.name with
	(*|	| Int _ -> tLink t (tlabeled tint (evar())) (ngable + nged); addC t setC (* T-INT *)*)
	| Int _ -> tLink t (tlabeled tint (evar())) setC; (* T-INT *)
	| Bool _ -> tLink t (tlabeled tbool (evar())) setC; (* T-BOOL *)
	| Name ("+" | "-" | "*" | "/") -> tLink t int3 setC; (* T-SUM, T-MINUS, T-PLUS, T-DIV *)
	| Name (">" | "<" | "==" | "!=") -> tLink t bool3 setC; (* T-GT, T-LT, T-EQ, T-NE *)
	| Name ("branch") -> (* T-BRANCH *)
			let branch =
				let tAll = tevar() in
				tLink t (tlabeled (tarrow (tlabeled tbool nol) (tlabeled (tarrow tAll (tlabeled (tarrow tAll tAll) nol)) nol)) nol) setC; in
			branch
	| Name ("addLab") -> (* T-RELAB *)
			if (String.compare color "policy") == 0 then
				begin
					let add =
						let e1 = evar() in
						let te1 = tlabeled (tlab e1) nol in
						let te2 = tevar() in
						let tReturn = (tlabeled te2 e1) in
						(*| Should the whole function have the label NOL or not? yes. because in T-APP expects to see a label for the function*)
						tLink t (tlabeled (tarrow te1 (tlabeled (tarrow te2 tReturn) nol)) nol) setC; in
					add
				end
			else
				raise (PolicyError "In type_of_const: can't add label while color is code!")
	| Name ("remLab") -> (* T-UNLAB *)
			if (String.compare color "policy") == 0 then
				begin
					let rem =
						let e2 = evar() in
						let tPartOf_te1 = tevar() in (*  *)
						let te1 = tlabeled tPartOf_te1 e2 in
						let tReturn	= tPartOf_te1 in
						(*| link t (tlabeled (tarrow te1 (tlabeled tPartOf_te1 nol)) nol); addC t setC in*)
						tLink t (tlabeled (tarrow te1 tReturn) nol) setC; in
					(*| fixme: adding nol to tPartOf_te1 leads to stack_overflow                                  *)
					(*| nol is added to tPartOf_te1 to make sure that it has a label even if e2 was its last label*)
					rem
				end
			else
				raise (PolicyError "In delta: can't remove label while color is code!")
	| Name ("getLab") -> (* T-GETLAB *)
			if (String.compare color "policy") == 0 then
				begin
					let get =
						let e2 = evar() in
						let te1 = tlabeled (tvar()) e2 in
						let tReturn = (tlabeled (tlab e2) nol) in
						tLink t (tlabeled (tarrow te1 tReturn) nol) setC; in
					get
				end
			else
				raise (PolicyError "In delta: can't get label while color is code!")
	| Lab ("noLab") -> tLink t (tlabeled (tlab (emark (nged) (eeyxp (Econ noLab)))) (evar())) setC;
	| Lab ("high") -> tLink t (tlabeled (tlab (emark (nged) (eeyxp (Econ high)))) (evar())) setC; (* T-HIGH *)
	| Lab ("low") -> tLink t (tlabeled (tlab (emark (nged) (eeyxp (Econ low)))) (evar())) setC; (* T-LOW *)
	| Name n -> raise (Undefined_constant n)
	| Lab l -> raise (Undefined_constant l);;

let rec create_lam_expr varList expr =
	let length = (List.length varList) in
	if (length == 0) then
		expr
	else if (length == 1) then
		Fun (List.hd varList, expr)
	else
		Fun (List.hd varList, create_lam_expr (List.tl varList) expr);;

let extend tenv (x, t) = (x, t):: tenv;;

exception Free_variable of var
let type_of_var color tenv x t setC iNodeRoot ieNodeRoot =
	try
		let foundType = List.assoc x tenv in
		let resType =
			typeInstance foundType iNodeRoot ieNodeRoot
		(*|			if (foundType.tmark == (gable + ged)) then*)
		(*|				typeInstance Tempty foundType           *)
		(*|			else                                      *)
		(*|				foundType                               *)
		in
		tLink t resType setC; (* T-existingVAR *)
	(*|		tLink t resType (t.tmark); addC t setC (* T-existingVAR *)*)
	with Not_found -> tLink t (tevar()) setC;; (* T-newVAR *)

(*| ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ*)
(*| ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ*)
(*| ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ*)
(*| ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ*)

(*|let rec assertEqual labelList label =              *)
(*|	let length = (List.length labelList) in          *)
(*|	if (length != 0) then                            *)
(*|		begin                                          *)
(*|			let hd = (List.hd labelList) in              *)
(*|			let mark = chooseMark hd.emark label.emark in*)
(*|			eLink hd label mark;                         *)
(*|			assertEqual (List.tl labelList) label        *)
(*|		end                                            *)
(*|;;                                                 *)

(*| ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ*)
(*| ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ*)
(*| ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ*)
(*| ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ*)

(* =================================== poly infer                          *)
(* ==================================                                      *)

let rec constraintGen color tenv e t setC iNodeRoot ieNodeRoot (*(ETnode expType)*) =
	print_string "";
	(* expType.etLink <- ETnode (expTypePair e t); *)
	begin match e with
		| Var x -> type_of_var color tenv x t setC iNodeRoot ieNodeRoot
		| Const c -> type_of_const color c t setC iNodeRoot
		| Fun (x, body) -> (* T-ABS *)
				let tX = tevar() and tbody = tevar() in
				let setC1 = constraintGen color (extend tenv (x, tX)) body tbody setC iNodeRoot ieNodeRoot in
				(*|			fixme                            *)
				(*|			let labelList = getAllLabels t in*)
				(*|			let _ = assertEqual              *)
				(*|					labelList nol in             *)
				(*|			tLink t (tlabeled (tarrow tX tbody) nol) (ngable+nged); addC t setC*)
				tLink t (tlabeled (tarrow tX tbody) nol) setC;
		| App (e1, e2) -> (* T-APP *)
				let te1 = tevar() and te2 = tevar() in
				let setC1 = constraintGen color tenv e1 te1 setC iNodeRoot ieNodeRoot in
				let setC2 = constraintGen color tenv e2 te2 setC iNodeRoot ieNodeRoot in
				(* let labelList = getAllLabels te1 in let _ = assertEqual         *)
				(* labelList nol in pairs                                          *)
				tLink te1 (tlabeled (tarrow te2 t) nol) setC;
		| Let (x, e1, e2) -> (* T-LET *) (* fixme: In paper, add the case for [let a b c ... = e1 in e2] *) (* also, implement the generalization *)
				let tfunc = tevar() and te1 = tevar() and te2 = tevar() in
				let func = create_lam_expr (List.tl x) e1 in
				let setC1 = constraintGen color tenv func te1 setC iNodeRoot ieNodeRoot in
				let te1 = generalize te1 in
				let setC2 = constraintGen color (extend tenv (List.hd x, te1)) e2 te2 setC iNodeRoot ieNodeRoot in
				tLink t te2 setC; tLink tfunc te1 setC;
		(*|			tLink t te2 (chooseMark t.tmark te2.tmark); addC tfunc setC; addC t setC*)
		| LetP (x, e1, e2) ->
				let tfunc = tevar() and te1 = tevar() and te2 = tevar() in
				let func = create_lam_expr (List.tl x) e1 in
				let setC1 = constraintGen "policy" tenv func te1 setC iNodeRoot ieNodeRoot in
				(print_constraint_set (Cnode setC)); print_newline();
				let te1 = generalize te1 in
				let setC2 = constraintGen color (extend tenv (List.hd x, te1)) e2 te2 setC iNodeRoot ieNodeRoot in
				tLink t te2 setC; tLink tfunc te1 setC;
	end;
	(print_concrete_expr e); print_string(" : "); (print_type t); print_newline();
;;

let genConstrSet e t cs (*etPair*) =
	let iNodeRoot = (Inode (iInit())) in
	let ieNodeRoot = (IEnode (iEInit())) in
	constraintGen "code" [] e t cs iNodeRoot ieNodeRoot (*etPair*);
	(*|	print_newline(); print_string "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"; print_newline();*)
	(*|	print_constraint_set (Cnode cs);                                                *)
	(*|	print_newline(); print_string "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"; print_newline();*)
	createExtraRelations iNodeRoot cs;
	(*|	createExtraERelations ieNodeRoot cs;*)
	(*|	print_newline(); print_string "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"; print_newline();*)
	(*|	print_constraint_set (Cnode cs);                                                *)
	(*|	print_newline(); print_string "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"; print_newline();*)
	print_string "";
	print_string "";
;;



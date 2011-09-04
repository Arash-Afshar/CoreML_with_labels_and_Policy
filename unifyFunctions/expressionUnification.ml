open InferDefs;;
open PrettyPrinter;;
open Syntax;;


exception UnunifyingConstraints of string;;
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++ Unify Expressions +++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

let rec unifyTwoE e1 e2 =
	match e1, e2 with
	  Evar v1, Evar v2 ->
		(Evar v1, Evar v2)
	| Evar v, Econ expr ->
		(*unifyExp expr;*)
		(e1, e2)
	| Econ expr, Evar v ->
		(*unifyExp expr;*)
		(e2, e1)
	| Econ expr1, Econ expr2 ->
		begin match expr1, expr2 with
		  Var v1, Var v2 ->
			begin
				if (v1 != v2) then
					raise (UnunifyingConstraints ("expression variables "^v1^" and "^v2^" cannot be unified!")) (* fixme: it might be wrong!*)
				else
					(Evar (-1), Evar (-1)) (* means: nothing to do *)
			end
		(*| Var v, exp -> 1
		| exp, Var v -> 1*)
		| Fun(v1, e1), Fun(v2, e2) ->
			begin 
				let subs1 = unifyTwoE (Econ (Var v1)) (Econ (Var v2)) in
				(* substitute the changes *)
				let subs2 = unifyTwoE (Econ e1) (Econ e2) in
				subs2
			end
		| App(e1, e2), App(e3, e4) ->
			begin 
				let subs1 = unifyTwoE (Econ e1) (Econ e3) in
				(* substitute the changes *)
				let subs2 = unifyTwoE (Econ e2) (Econ e4) in
				subs2
			end
		| Let(vl1, e1, e2), Let(vl2, e3, e4) -> (Evar (-1), Evar (-1))
		| LetP(vl1, e1, e2), LetP(vl2, e3, e4) -> (Evar (-1), Evar (-1))
		| Const {name = Int n1;    arity = 0; constr = true}, Const {name = Int n2;    arity = 0; constr = true} -> (Evar (-1), Evar (-1))
		| Const {name = Bool b1;   arity = 0; constr = true}, Const {name = Bool b2;   arity = 0; constr = true} -> (Evar (-1), Evar (-1))
		| _ -> (Evar (-2), Evar (-2))
		end
(*and
unifyExp currentTE rootCS = 
	match currentTE with
		  Tempty ->
			-3
		| Tnode node ->
			begin match node.tlink_node with	
				  Tempty ->
					(*delSingleInCS rootCS rootCS;*) -4
				| Tnode nextTnode ->
					let subs = unifyTwoTE1 (node.texp_node) (nextTnode.texp_node) rootCS in
					begin match subs with
						  (Tvar (-1), Tvar (-1)) -> (* for optimization, we only delete  equal *)
							delEqCS rootCS; (*delSingleInCS rootCS rootCS;*) -5
						(*fixme: error handling is done in unifyTwoTE1 *)	
						| _  ->
							applyAndClean subs rootCS; -6
					end;
					unifyTE1 currentTE rootCS;
			end*)
;; (* fixme *)
(* ================================================= Unify Expressions ============================================== *)




(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++ Exp Subs Var with Var +++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
let rec eSubsVarVar (ev1, ev2) te =
	begin match te.texp_node with
	  Tvar var0 ->
		-15
	| Tcon (sym, l)->
		begin match sym with
			  Tarrow ->
				eSubsVarVar (ev1, ev2) (List.hd l); eSubsVarVar (ev1, ev2) (List.hd (List.tl l)); -16
			| _ -> -17
		end
	| Tlabeled (t, e) ->
		begin match e with
		  Enode node ->
			begin match node.eexp_node with
				  Evar v ->
					if (ev1 == v) then
						te.texp_node <- (tlabeled t (eexp (Evar ev2))).texp_node;
						-16
				| _ ->
					-18
			end
		| _ ->
			-18
		end;
		eSubsVarVar (ev1, ev2) t; -18
	| _ -> -19
	end;
	begin match te.tlink_node with
	  Tnode node ->
		eSubsVarVar (ev1, ev2) node; -20
	| Tempty -> -21
	end
;;

let rec eSubsVarVarCS (ev1, ev2) csNode =
	eSubsVarVar (ev1, ev2) csNode.cnstrnt;
	match csNode.link with
		  Cnode node ->
			eSubsVarVarCS (ev1, ev2) node; -22
		| Cempty -> -23
;;
(* ============================================== Exp Subs Var with Var ============================================== *)


(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* ++++++++++++++++++++++++++++++++++++++++++++++ Subs Var with other Exp ++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
let rec eSubsVarOther (ev, another) te =
	begin match te.texp_node with
	  Tvar var ->
		-15
	| Tcon (sym, l)->
		begin match sym with
			  Tarrow ->
				eSubsVarOther (ev, another) (List.hd l); eSubsVarOther (ev, another) (List.hd (List.tl l)); -16
			| _ -> -17
		end
	| Tlabeled (t, e) ->
		begin match e with
		  Enode node ->
			begin match node.eexp_node with
				  Evar v ->
					if (ev == v) then
						te.texp_node <- (tlabeled t (eexp another)).texp_node;
					-16
				| _ ->
					-18
			end
		| _ ->
			-18
		end;
		eSubsVarOther (ev, another) t; -18
	| _ -> -19
	end;
	begin match te.tlink_node with
	  Tnode node ->
		eSubsVarOther (ev, another) node; -20
	| Tempty -> -21
	end
;;

let rec eSubsVarOtherCS (ev, another) csNode =
	eSubsVarOther (ev, another) csNode.cnstrnt;
	match csNode.link with
		  Cnode node ->
			eSubsVarOtherCS (ev, another) node; -22
		| Cempty -> -23
;;
(* ============================================= Subs Var with other Exp ============================================= *)

(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* ++++++++++++++++++++++++++++++++++++++++++++++ apply exp substitute +++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

let rec applyExpSubsCS (e1, e2) csNode = (* fixme *)
	begin match e1, e2 with
	  Evar v1, Evar v2 ->
		eSubsVarVarCS (v1, v2) csNode; 1
	| Evar v, another ->
		eSubsVarOtherCS (v, another) csNode; 2
	| another, Evar v ->
		eSubsVarOtherCS (v, another) csNode; 3
	| Econ expr1, Econ expr2 ->
		begin match expr1, expr2 with
		  Var v1, Var v2 ->
			-4
		| Fun(v1, e1), Fun(v2, e2) ->
			-4
		| App(e1, e2), App(e3, e4) ->
			-4
		| Let(vl1, e1, e2), Let(vl2, e3, e4) ->
			-4
		| LetP(vl1, e1, e2), LetP(vl2, e3, e4) ->
			-4
		| Const {name = Int n1;    arity = 0; constr = true}, Const {name = Int n2;    arity = 0; constr = true} ->
			-4
		| Const {name = Bool b1;   arity = 0; constr = true}, Const {name = Bool b2;   arity = 0; constr = true} ->
			-4
		| _ ->
			-5 (* fixme: error. the above -4s can also contain error *)
		end
	end
;;

(*let rec applyExpSubsList subsSet csNode =
	match subsSet with
		  [] -> -24
		| _  ->
			begin
			applyExpSubsCS (List.hd subsSet) csNode;
			match csNode.link with
				  Cnode node ->
					applyExpSubsList (List.tl subsSet) csNode; -22
				| Cempty -> -23
			end
;;*)
(* ============================================== apply exp substitute =============================================== *)



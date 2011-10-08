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
		(Evar v1, [Evar v2])
	| Evar v, Econ expr ->
		(*unifyExp expr;*)
		(e1, [e2])
	| Econ expr, Evar v ->
		(*unifyExp expr;*)
		(e2, [e1])
	| Econ expr1, Econ expr2 ->
		begin match expr1, expr2 with
		  Var v1, Var v2 ->
			begin
				if (v1 != v2) then
					raise (UnunifyingConstraints ("expression variables "^v1^" and "^v2^" cannot be unified!")) (* fixme: it might be wrong!*)
				else
					(Evar (-1), [Evar (-1)]) (* means: nothing to do *)
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
		| Let(vl1, e1, e2), Let(vl2, e3, e4) -> (Evar (-1), [Evar (-1)])
		| LetP(vl1, e1, e2), LetP(vl2, e3, e4) -> (Evar (-1), [Evar (-1)])
		| Const {name = Int n1;    arity = 0; constr = true}, Const {name = Int n2;    arity = 0; constr = true} -> (Evar (-1), [Evar (-1)])
		| Const {name = Bool b1;   arity = 0; constr = true}, Const {name = Bool b2;   arity = 0; constr = true} -> (Evar (-1), [Evar (-1)])
		| _ -> (Evar (-2), [Evar (-2)])
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


(*|                                                                                                                         *)
(*|(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)*)
(*|(* +++++++++++++++++++++++++++++++++++++++++++++++ Exp Subs Var with Var +++++++++++++++++++++++++++++++++++++++++++++ *)*)
(*|(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)*)
(*|let rec eSubsVarVar_InTlabeled ev1 ev2 labelList resultList =                                                            *)
(*|	let length = (List.length labelList) in                                                                                *)
(*|	if (length == 0) then                                                                                                  *)
(*|		resultList                                                                                                           *)
(*|	else                                                                                                                   *)
(*|	begin                                                                                                                  *)
(*|		let hd = (List.hd labelList) in                                                                                      *)
(*|		begin match hd.eexp_node with                                                                                        *)
(*|			  Evar v ->                                                                                                        *)
(*|				if (v == ev1) then                                                                                               *)
(*|					eSubsVarVar_InTlabeled ev1 ev2 (List.tl labelList) (resultList @ [emark 0 (eezzxp (Evar ev2))])                *)
(*|				else                                                                                                             *)
(*|					eSubsVarVar_InTlabeled ev1 ev2 (List.tl labelList) (resultList @ [hd])                                         *)
(*|			| _ ->                                                                                                             *)
(*|					eSubsVarVar_InTlabeled ev1 ev2 (List.tl labelList) (resultList @ [hd])                                         *)
(*|		end                                                                                                                  *)
(*|	end                                                                                                                    *)
(*|;;                                                                                                                       *)
(*|                                                                                                                         *)
(*|let rec eSubsVarVar (ev1, ev2) te =                                                                                      *)
(*|	begin match te.texp_node with                                                                                          *)
(*|	  Tvar var0 ->                                                                                                         *)
(*|		()                                                                                                                   *)
(*|	| Tcon (sym, l)->                                                                                                      *)
(*|		begin match sym with                                                                                                 *)
(*|			  Tarrow ->                                                                                                        *)
(*|				eSubsVarVar (ev1, ev2) (List.hd l); eSubsVarVar (ev1, ev2) (List.hd (List.tl l))                                 *)
(*|			| _ -> ()                                                                                                          *)
(*|		end                                                                                                                  *)
(*|	| Tlabeled (t, el) ->                                                                                                  *)
(*|		let resultantLabels = eSubsVarVar_InTlabeled ev1 ev2 el [] in                                                        *)
(*|		te.texp_node <- Tlabeled (t, resultantLabels);                                                                       *)
(*|		eSubsVarVar (ev1, ev2) t                                                                                             *)
(*|	| _ -> ()                                                                                                              *)
(*|	end;                                                                                                                   *)
(*|	begin match te.tlink_node with                                                                                         *)
(*|	  Tnode node ->                                                                                                        *)
(*|		eSubsVarVar (ev1, ev2) node                                                                                          *)
(*|	| Tempty -> ()                                                                                                         *)
(*|	end                                                                                                                    *)
(*|;;                                                                                                                       *)
(*|                                                                                                                         *)
(*|let rec eSubsVarVarCS (ev1, ev2) csNode =                                                                                *)
(*|	eSubsVarVar (ev1, ev2) csNode.cnstrnt;                                                                                 *)
(*|	match csNode.link with                                                                                                 *)
(*|		  Cnode node ->                                                                                                      *)
(*|			eSubsVarVarCS (ev1, ev2) node                                                                                      *)
(*|		| Cempty -> ()                                                                                                       *)
(*|;;                                                                                                                       *)
(*|(* ============================================== Exp Subs Var with Var ============================================== *)*)

(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* ++++++++++++++++++++++++++++++++++++++++++++++ Subs Var with other Exp ++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
let rec eSubsVarOther_InTlabeled ev another labelList resultList =
	let length = (List.length labelList) in
	if (length == 0) then
		resultList
	else
	begin
		let hd = (List.hd labelList) in
		begin match hd.eexp_node with
			  Evar v ->
				if (v == ev) then
(*|					eSubsVarOther_InTlabeled ev another (List.tl labelList) (resultList @ (List.map (emark 1) (List.map eexp another)))*)
					eSubsVarOther_InTlabeled ev another (List.tl labelList) (resultList @ (List.map eeyxp another))
				else
					eSubsVarOther_InTlabeled ev another (List.tl labelList) (resultList @ [hd])
			| _ ->
					eSubsVarOther_InTlabeled ev another (List.tl labelList) (resultList @ [hd])
		end
	end
;;

let rec eSubsVarOther (ev, another) te =
	begin match te.texp_node with
	  Tvar var ->
		()
	| Tcon (sym, l)->
		begin match sym with
			  Tarrow ->
				eSubsVarOther (ev, another) (List.hd l); eSubsVarOther (ev, another) (List.hd (List.tl l))
			| _ -> ()
		end
	| Tlabeled (t, el) ->
		let resultantLabels = eSubsVarOther_InTlabeled ev another el [] in
		te.texp_node <- Tlabeled (t, resultantLabels);
		eSubsVarOther (ev, another) t
	| _ -> ()
	end;
	begin match te.tlink_node with
	  Tnode node ->
		eSubsVarOther (ev, another) node
	| Tempty -> ()
	end
;;

let rec eSubsVarOtherCS (ev, another) csNode =
	eSubsVarOther (ev, another) csNode.cnstrnt;
	match csNode.link with
		  Cnode node ->
			eSubsVarOtherCS (ev, another) node
		| Cempty -> ()
;;
(* ============================================= Subs Var with other Exp ============================================= *)

(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* ++++++++++++++++++++++++++++++++++++++++++++++ apply exp substitute +++++++++++++++++++++++++++++++++++++++++++++++ *)
(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

let rec applyExpSubsCS (e1, el) csNode = (* fixme *)
	(*print_newline(); print_string "----------------------------------"; print_newline();
	print_string (string_of_label (eexp e1));
	print_newline();
	List.map print_string (List.map string_of_label (List.map eexp el));
	print_newline(); print_string "----------------------------------"; print_newline();*)
	if (List.length el) == 1 then
	begin
		let e2 = List.hd el in
		begin match e1, e2 with
		  Evar v1, Evar v2 ->
			eSubsVarOtherCS (v1, [e2]) csNode
		| Evar v, another ->
			eSubsVarOtherCS (v, [another]) csNode
		| another, Evar v ->
			eSubsVarOtherCS (v, [another]) csNode
		| Econ expr1, Econ expr2 ->
			begin match expr1, expr2 with
			  Var v1, Var v2 ->
				()
			| Fun(v1, e1), Fun(v2, e2) ->
				()
			| App(e1, e2), App(e3, e4) ->
				()
			| Let(vl1, e1, e2), Let(vl2, e3, e4) ->
				()
			| LetP(vl1, e1, e2), LetP(vl2, e3, e4) ->
				()
			| Const {name = Int n1;    arity = 0; constr = true}, Const {name = Int n2;    arity = 0; constr = true} ->
				()
			| Const {name = Bool b1;   arity = 0; constr = true}, Const {name = Bool b2;   arity = 0; constr = true} ->
				()
			| _ ->
				() (* fixme: error. the above -4s can also contain error *)
			end
		end
	end
	else
		let Evar v = e1 in
		eSubsVarOtherCS (v, el) csNode
;;

let rec applyExpSubsList subsSet csNode =
	match subsSet with
		  [] -> ()
		| _  ->
			begin
			applyExpSubsCS (List.hd subsSet) csNode;
			match csNode.link with
				  Cnode node ->
					applyExpSubsList (List.tl subsSet) csNode
				| Cempty -> ()
			end
;;
(* ============================================== apply exp substitute =============================================== *)



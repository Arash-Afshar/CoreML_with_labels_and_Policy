open InferDefs;;

(* fixme: check to see if these functions consider types in Tcon ... *)

let nextVar txp =
	match txp.tlink_node with
	  Tnode node ->
		begin match node.texp_node with
		  Tvar var -> var
		| _ -> -1 (* not a var *)
		end
	| Tempty -> -2 (* end of equation *)
;;

let rec isSingleTE te =
	match te.texp_node with
		  Tvar var ->
			(*print_string "The var "; print_int var;*)
			begin match te.tlink_node with
				  Tempty -> (*print_string " is single.";  print_newline();*) true
				| _ -> (*print_string " is NOt single.";  print_newline();*) false
			end
		| Tcon (sym, l) ->
			begin match sym with
			  Tarrow ->
				let ret1 = isSingleTE (List.hd l) and ret2 = isSingleTE (List.hd (List.tl l)) in
					if ( ret1 & ret2) then
						true
					else
						false
			| _ ->
				begin match te.tlink_node with
					  Tempty -> true
					| _ -> false
				end
			end
		| Tlabled (t, e) ->
			isSingleTE t;
;;

let rec delSingleInCS currentC parentC =
	match currentC.link with
		  Cnode nextC ->
			if (isSingleTE currentC.cnstrnt) then
				begin
					parentC.link <- currentC.link;
					begin match parentC.link with
						  Cnode newNextC ->
							delSingleInCS newNextC parentC
						| Cempty -> -36
					end
				end
			else
				delSingleInCS nextC currentC
		| Cempty ->
			if (isSingleTE currentC.cnstrnt) then
				begin parentC.link <- Cempty; -35 end
			else
				-34
;;


let rec delEqTE te =
	begin match te.texp_node with
	  Tvar var0 ->
		begin match te.tlink_node with
			  Tnode nextTnode ->
				let var1 = (nextVar te) in
					if (var0 == var1) then
						begin te.tlink_node <- nextTnode.tlink_node; -3 end
					else
						-4
			| Tempty ->
				-5
		end
	| Tcon (sym, l)->
		begin match sym with
			  Tarrow ->
				delEqTE (List.hd l); delEqTE (List.hd (List.tl l)); -6
			| _ -> -7
		end
	| Tlabled (t, e) ->
		delEqTE t; -8
	| _ -> -9
	end;
	begin match te.tlink_node with
	  Tnode node ->
		delEqTE node; -10
	| Tempty -> -11
	end
;;

let rec delEqCS csNode =
	delEqTE csNode.cnstrnt;
	match csNode.link with
		  Cnode node ->
			delEqCS node; -12
		| Cempty -> -13
;;


let rec subsTE tv1 tv2 te =
	begin match te.texp_node with
	  Tvar var0 ->
		if (var0 == tv1) then
			begin te.texp_node <- (Tvar tv2); -14 end
		else
			-15
	| Tcon (sym, l)->
		begin match sym with
			  Tarrow ->
				subsTE tv1 tv2 (List.hd l); subsTE tv1 tv2 (List.hd (List.tl l)); -16
			| _ -> -17
		end
	| Tlabled (t, e) ->
		subsTE tv1 tv2 t; -18
	| _ -> -19
	end;
	begin match te.tlink_node with
	  Tnode node ->
		subsTE tv1 tv2 node; -20
	| Tempty -> -21
	end
;;

let rec subsCS tv1 tv2 csNode =
	subsTE tv1 tv2 csNode.cnstrnt;
	match csNode.link with
		  Cnode node ->
			subsCS tv1 tv2 node; -22
		| Cempty -> -23
;;

let rec unifyVarInTE te cs =
	begin match te.texp_node with
	  Tvar var ->
		let next = nextVar te in
		if next > 0 then
			begin subsCS var next cs; -24 end
		else
			-25
	| Tcon (sym, l)->
		begin match sym with
			  Tarrow ->
				unifyVarInTE (List.hd l) cs; unifyVarInTE (List.hd (List.tl l)) cs; -26
			| _ -> -27
		end
	| Tlabled (t, e) ->
		unifyVarInTE t cs; -28
	| _ -> -29
	end;
	begin match te.tlink_node with
	  Tnode node ->
		unifyVarInTE node cs; -30
	| Tempty -> -31
	end
;;

let rec unifyVarInCS cs1 root =
	unifyVarInTE cs1.cnstrnt root;
	match cs1.link with
		  Cnode node ->
			unifyVarInCS node root; -32
		| Cempty -> -33
;;

let unifyVarAndDelEq cs1 root =
	unifyVarInCS cs1 root;
	delEqCS cs1;;

let unifyVarAndDelEqAndDelSingle cs1 root =
	unifyVarInCS cs1 root;
	delEqCS cs1;
	let Cnode next = cs1.link in
	delSingleInCS  next root;;

let rec unifyTwoTE te1 te2 rootCS =
	
;;

let rec unifyTE currentTE rootCS =
	begin match currentTE.texp_node with
		  Tvar var ->
			-38
		| Tcon (sym, l)->
			begin match sym with
				  Tarrow ->
					unifyTE (List.hd l) rootCS; unifyTE (List.hd (List.tl l)) rootCS; -39
				| _ ->
					-40
			end;
		| Tlabled (t, e) ->
			unifyTE t rootCS; -41
		| _ -> -42
	end;
	begin match currentTE.tlink_node with
		  Tempty -> -43
		| Tnode node ->
			unifyTwoTE (currentTE.texp_node) node rootCS; -44
	end
;;

let rec unifyCS currentC rootCS =
	unifyTE currentC.cnstrnt rootCS;
	match currentC.link with
	  Cnode node ->
		unifyCS node rootCS
	| Cempty -> -37
;;






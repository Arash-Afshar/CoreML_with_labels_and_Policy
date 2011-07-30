open InferDefs;;

(* fixme: these functions do not consider types in Tcon ... *)

let rec subsTE tv1 tv2 te =
	begin match te.texp_node with
	  Tvar var0 ->
		begin match tv1 with
		  Tvar var1 ->
			if (var0 == var1) then
				begin te.texp_node <- tv2; 1 end
			else
				1
		end
	| _ -> 0
	end;
	begin match te.tlink_node with
	  Tnode node ->
		subsTE tv1 tv2 node; 1
	| Tempty -> 0
	end
;;

let rec subsCS tv1 tv2 csNode =
	subsTE tv1 tv2 csNode.cnstrnt;
	match csNode.link with
		  Cnode node ->
			subsCS tv1 tv2 node; 1
		| Cempty -> 0
;;

let nextVar txp =
	match txp.tlink_node with
	  Tnode node ->
		begin match node.texp_node with
		  Tvar var -> var
		| _ -> -2 (* not a var *)
		end
	| Tempty -> -3 (* end of equation *)
;;


let rec unifyVarInTE te cs =
	begin match te.texp_node with
	  Tvar var ->
		let next = nextVar te in
		if next > 0 then
			subsCS (Tvar var) (Tvar next) cs
		else
			1
	| _ -> 0
	end;
	begin match te.tlink_node with
	  Tnode node ->
		unifyVarInTE node cs; 1
	| Tempty -> 0
	end
;;

let rec unifyVarInCS cs1 cs =
	unifyVarInTE cs1.cnstrnt cs;
	match cs1.link with
		  Cnode node ->
			unifyVarInCS node cs; 1
		| Cempty -> 0
;;



	



open Syntax;;

type eeeenode = { mutable eexp_node : edesc; mutable elink_node : eexp; mutable  emark : int }
and eexp = Eempty | Enode of eeeenode
and edesc = Evar  of int | Econ of expr;;

type type_symbol = Tarrow | Tint | Tbool | Tlab of eeeenode
type ttttnode = { mutable  texp_node : tdesc; mutable tlink_node : texp; mutable  tmark : int }
and texp = Tempty | Tnode of ttttnode
and tdesc = Tvar of int | Tcon of type_symbol * ttttnode list | Tlabeled  of ttttnode * eexp;;

let eexp e = { eexp_node = e; elink_node = Eempty; emark = 0 };;
let texp d = { texp_node = d; tlink_node = Tempty; tmark = 0 };;

type expTypePairNode = { mutable etExp : expr; mutable etType: ttttnode; mutable etLink: expTypePair}
and expTypePair = ETempty | ETnode of expTypePairNode;;

(*
let expTypePair e t     = {etExp = e; etType = t; etLink = ETempty};;
let initExpTypePair = {etExp = Var "NAN"; etType = texp (Tvar (-1)); etLink = ETempty};;
*)

type scheme = texp list * texp;;

let count = ref  0
let evar() = incr  count; eexp (Evar  !count);;

(*let count = ref  0
let tvar() = incr  count; ref (Desc  (Tvar !count));;*)
let count = ref  0
let tvar() = incr  count; texp (Tvar  !count);;

let tint         = texp (Tcon (Tint,  []))
let tbool        = texp (Tcon (Tbool, []))
let tlab e       = texp (Tcon (Tlab e,  []))
let tarrow t1 t2 = texp (Tcon (Tarrow, [t1; t2]))
let tlabeled t e = texp (Tlabeled (t, Enode e))
let nol          = eexp (Econ noLab);;
(*
let zZZZZZZZZZ = eexp (Econ high);;
let yYYYYYYYYY = eexp (Econ low);;
*)

let tevar() = tlabeled (tvar()) (evar());;

type csNode = { mutable cnstrnt : ttttnode; mutable link : cSet }
and cSet = Cempty | Cnode of csNode;;
let csnode c  = { cnstrnt = c ; link = Cempty } 
let csInit    = { cnstrnt = texp (Tvar (-1)) ; link = Cempty } 
let rec crepr cs =
	match cs  with
	  Cnode cs -> 
		begin match cs.link  with
		  (*u -> let  Enode v = erepr u  in en.eexp_node <- v.eexp_node; en.elink_node  <- Eempty; Enode v*)
		  Cnode u -> let  v = crepr (Cnode u)  in v
		| Cempty -> cs
		end
	| Cempty -> assert false;;
let addC c cs = (crepr (Cnode cs)).link <- Cnode (csnode c);;

let last_mark = ref 0
let marker() = incr last_mark; !last_mark;;

let rec erepr e =
	match e  with
	  Enode en -> 
		begin match en.elink_node  with
		  (*u -> let  Enode v = erepr u  in en.eexp_node <- v.eexp_node; en.elink_node  <- Eempty; Enode v*)
		  Enode u -> let  v = erepr (Enode u)  in v
		| Eempty -> en
		end
	| Eempty -> assert false;;

let edesc e  =
	let e = erepr e in
	match e.elink_node with
	  Enode u -> assert  false
	| Eempty -> e.eexp_node;;

let rec repr  t =
	match t  with
	  Tnode tn -> 
		begin match tn.tlink_node  with
		  (*u -> let  Tnode v = repr u  in tn.texp_node <- v.texp_node; tn.link_node  <- Tempty; Tnode v*)
		  Tnode u -> let  v = repr (Tnode u)  in v
		| Tempty -> tn
		end
	| Tempty -> assert false;;

let tdesc t  =
	let t = repr t in
	match t.tlink_node with
	  Tnode u -> assert  false
	| Tempty -> t.texp_node;;

let link  t1 t2 = ( repr (Tnode t1)).tlink_node <- (Tnode t2);;
let elink e1 e2 = (erepr (Enode e1)).elink_node <- (Enode e2);;

let rec getAllLabels t =
	match t.texp_node with
		  Tlabeled(t2,e) ->
			begin match e with
			  Eempty ->
				[] (* fixme: Error *)
			| Enode enode ->
				(getAllLabels t2)@[enode]
			end
		| _ ->
			[]
;;

let rec getUnlabeledPart t =
	match t.texp_node with
		  Tlabeled(t2,e) ->
			getUnlabeledPart t2
		| _ ->
			t.texp_node
;;


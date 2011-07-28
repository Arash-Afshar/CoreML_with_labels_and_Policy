open Syntax;;
open ConstraintUnify;;
(*open Typescheme;;*)


type eexp = { mutable eexp_node : enode; mutable  emark : int }
and enode = Edesc of edesc | Elink of eexp
and edesc = Evar  of int | Econ of expr;;

type type_symbol = Tarrow | Tint | Tbool | Tlab
type texp = { mutable  texp_node : node; mutable  mark : int }
and node = Desc of desc | Link of texp
and desc = Tvar  of int | Tcon of type_symbol * texp  list | Tlabled  of texp * eexp;;


let eexp e  = { eexp_node = Edesc e; emark = 0 };;
let texp d  = { texp_node = Desc d;   mark = 0 };;

type scheme = texp list * texp;;

let count = ref  0
let evar() = incr  count; eexp (Evar  !count);;

let count = ref  0
let tvar() = incr  count; ref (Desc  (Tvar !count));;
let count = ref  0
let tvar() = incr  count; texp (Tvar  !count);;

let tint         = texp (Tcon (Tint,  []))
let tbool        = texp (Tcon (Tbool, []))
let tlab         = texp (Tcon (Tlab,  []))
let tarrow t1 t2 = texp (Tcon (Tarrow, [t1; t2]))
let tlabled t e  = texp (Tlabled (t, e));;

let last_mark = ref 0
let marker() = incr last_mark; !last_mark;;

let rec erepr  e =
	match e.eexp_node  with
	  Elink u -> let  v = erepr u  in e.eexp_node  <- Elink v; v
	| Edesc _ -> e

let edesc e  =
	match (erepr e).eexp_node with
	  Elink u -> assert  false
	| Edesc d -> d;;

let rec repr  t =
	match t.texp_node  with
	  Link u -> let  v = repr u  in t.texp_node  <- Link v; v
	| Desc _ -> t

let desc t  =
	match (repr t).texp_node with
	  Link u -> assert  false
	| Desc d -> d;;

let link t1  t2 = (repr t1).texp_node <- Link  t2;;

(* =================================== poly infer ================================== *)

exception Undefined_constant of string
let type_of_const c setC =
	let int3 = tarrow tint (tarrow tint tint) in
	let bool3 = tarrow tbool (tarrow tbool tbool) in
	match c.name  with
	| Int _ -> tint, setC
	| Bool _ -> tbool, setC
	| Name ("+" | "-" | "*" | "/") -> int3, setC
	| Name (">" | "<" | "==" | "!=") -> bool3, setC
	| Name ("branch") ->
		let branch =
			let br1 = tvar() in
			let br2 = tvar() in
			let res = tvar() in
			link br1 br2; link br1 res; tarrow tbool (tarrow br1 (tarrow br2 res)), br1::setC in
		branch
	| Name ("addLab") ->
			let add =
			let t   = tvar() in
			let e   = evar() in  (* fixme: how to say exp has type lab??? *)
			let lab = tlab in
			let res = tvar() in
			link res (tlabled t e); tarrow lab (tarrow t res), res::setC in
		add
	| Name ("remLab") ->
			let rem =
			let t1  = tvar() in
			let e   = evar() in
			let t   = tlabled t1 e in
			let res = tvar() in
			link res t1; tarrow t res, res::setC in
		rem
	| Name ("getLab") ->
			let get =
			let t1  = tvar() in
			let e   = evar() in
			let t   = tlabled t1 e in
			let res = tvar() in
			link res tlab; tarrow t res, res::setC in
		get
	| Lab ("noLab") -> tvar(), setC
	| Lab ("high") -> tlab, setC
	| Lab ("low") -> tlab, setC
	| Name n -> raise  (Undefined_constant n)
	| Lab l -> raise  (Undefined_constant l);;
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

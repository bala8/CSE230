(* #use "finals_ocaml.ml";; *)

(* Fall 2013 finals *)
let rec insert l i =
	match l with
	| (h::t) -> if (i >= h) then h::(insert t i) else [i]@l
	| _ -> [i]

let insertion_sort l =
	let f a x = insert a x 
	in let base = [] 
	in List.fold_left f base l

type expr =
	| Var of string
	| Const of int
	| Plus of expr * expr

let rec simpl e =
	match e with
	| Plus(e1, e2)  ->
		let e1' = simpl e1 in
		let e2' =  simpl e2 in
		(match (e1', e2') with
			| (Const a, Const b) -> Const(a+b)
			| _ -> Plus(e1', e2'))
	(* all other cases: *)
	| _ -> e


(* spring 2013 finals *)
let count f l = 
	let fn a x = if f x then a+1 else a
	in let base = 0
	in List.fold_left fn base l

let stretch l =
	let f a x = a@[x;x]
	in let base = []
	in List.fold_left f base l

type 'a tree =
	| Empty
	| Node of 'a * 'a tree list;;

let rec zip l1 l2 =
	match (l1,l2) with
		| ([],[]) -> []
		| (h1::t1, h2::t2) -> (h1,h2)::(zip t1 t2)
		| _ -> raise (Failure "foo")

let rec tree_zip t1 t2 =
	match (t1,t2) with
	| Empty, Empty -> Empty
	| Node(a, l1), Node(b, l2) -> Node((a,b), List.map (fun (x,y) -> tree_zip x y) (zip l1 l2))
	| _ -> raise (Failure "foo")

(* winter 2013 finals *)
let sum_matrix l =
	let fn a x = 
		(let f aa xx = aa+xx
		in let b = 0
		in a+(List.fold_left f b x))
	in let base = 0
	in List.fold_left fn base l

let sum_matrix2 l = List.fold_left (List.fold_left (+)) 0 l

(* winter 2012 finals *)

let rec find d k =
	match d with
		| [] -> raise Not_found
		| (k',v') :: t -> if k' = k then v' 
						else if k' > k then raise Not_found
						else find t k

let rec add d k v =
	match d with
		| [] -> [(k,v)]
		| (k',v') :: t -> if k' = k then (k, v)::t
						  else if k' > k then (k,v)::d
						  else (k',v')::(add t k v)

let keys d = List.map (fun (k,v) -> k) d

let values d = List.map (fun (k,v) -> v) d

let key_of_max_val d =
	(* definition of fold_fn *)
	let fold_fn (k,v) (k',v') = if v' > v then (k', v') else (k, v)
	in match d with
		| [] -> raise Not_found
		(* the call to fold should be: fold_left fold_fn base t *)
		| base::t -> let (k,v) = List.fold_left fold_fn base t in k


(************************************************)

(* winter 2011 finals *)
type 'a dict = 
	| Empty 
	| Node of string * 'a * 'a dict * 'a dict


let fruitd =
Node ("grape", 2.65,
Node ("banana", 1.50,
Node ("apple", 2.25, Empty, Empty),
Node ("cherry", 2.75, Empty, Empty)),
Node ("orange", 0.75,
Node ("kiwi", 3.99, Empty, Empty),
Node ("peach", 1.99, Empty, Empty)))

let rec find d k =
	match d with
		| Empty -> raise Not_found
		| Node (k', v', l, r) ->
			if k = k' then v' else
			if k < k' then find l k else
			(* k > k’ *) find r k


let rec add d k v =
	match d with
		| Empty -> Node(k, v, Empty, Empty)
		| Node (k', v', l, r) ->
			if k = k' then Node(k, v, l, r) else
			if k < k' then Node(k', v', (add l k v), r) else
			(* k > k’ *) Node(k', v', l, (add r k v))


let rec fold f b d =
	match d with
		| Empty -> b
		| Node (k, v, l, r) -> let b = (fold f b l) in let b = (f k v b) in (fold f b r)

type name_space = 
 | EmptyNameSpace
 | Info of (string * value) list * name_space
and value = 
 | Int of int
 | Method of (name_space -> int -> int)

let rec lookup n s = 
	match n with
	| EmptyNameSpace -> raise Not_found
	| Info (l, np) -> let rec helper l = 
							(match l with
							| h::t -> let (s',v') = h in if s' = s then v' else (helper t)
							| [] -> lookup np s)
						in helper l


(*
(* part b and c *)

let method_f self i = (to_int (lookup self "a")) + i
let OBJ3 = Info([("a", Int(10)); ("f", Method(method_f))], EmptyNameSpace)

let invoke_method self name i = (to_method (lookup self name)) i
invoke_method OBJ3 f 3

*)


(************************************************)

(* fall 2007 finals *)

(*

1.
a - 100
b - (101, 1001)
c - [(1, "a"); (2, "b")]
d - Int 12
e - 10000


2.
a - mix
b - a'
c - a'
d - b'
e - a'
*)

(* tail recursive functions *)

let fact x = 
	let rec helper x' a = if x' = 1 then a else (helper (x-1) (x*a)) 
	in helper x 1

let mapper f xs =
	let rec helper x a =
		match x with
		| h::t -> helper t (a@[f h])
		| _ -> a
	in helper xs []

let foldr f xs b =
	let rec rev_helper l ll =
		(match l with
		| h::t -> rev_helper t (ll@[h])
		| _ -> ll)
	in let rec foldr_helper l r = 
		(match l with
		| h::t -> foldr_helper t (f h r)
		| _ -> r)
	in foldr_helper (rev_helper xs []) b



type ty = Tyint | Tybool | Tyfun of ty * ty | Tytemp of int * int * int

let rec lookup tenv x =
	match tenv with
	| (a,b)::t -> if a = x then Some b else lookup t x
	| _ -> None

type binop = Plus | Minus | Eq | Lt | And | Or

type expr =
| Const of int
| Var of string
| Bin of expr * binop * expr
| If of expr * expr * expr
| Let of string * expr * expr (* let X = E1 in E2 ---> Let (X,E1,E2) *)
| App of expr * expr (* E1 E2 ---> App(E1,E2) *)
| Fun of string * ty * expr (* fun X:T -> E ---> Fun(X,T,E) *)

(*
	Let("x", Const(10), Let("y", Bin(Var("x"), Plus, Const(12)), Bin(Var("x"), Plus, Var("y"))))
*)

(*
let rec check env e =
match e with
	| Const i -> Some i
	| Var x -> (lookup env x)
	| Plus (e1,e2) | Minus (e1,e2) ->
		let t1 = check env e1 in
		let t2 = check env e2 in
		if t1 = t2 then Some t1 else None
	| Leq (e1,e2) | Eq (e1,e2) ->
		let t1 = check env e1 in
		let t2 = check env e2 in

	| And (e1,e2) | Or (e1,e2) ->
		let t1 = check env e1 in
		let t2 = check env e2 in

	| App (e1,e2) ->
		let t1 = check env e1 in
		let t2 = check env e2 in
		(match (t1,t2) with None,_ | _,None -> None 
			expr e1, expr e2 -> (check env e1) 
		)
	| Fun (x,t,e) ->
	(match (check ((x,t)::env) e) with None -> None
		_ -> e
	)
	| Let (x,e1,e2) ->
	(match check env e1 with None -> None
		__
	)
	| If (p,t,f) ->
		let tp = check env p in
		let tt = check env t in
		let tf = check env f in
		__


*)



(********************************************)
(* finals fall 2005 *)

(*
1.
a - 122
b - 15
c - 65536

2.
a - int
b - 

*)


type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree

let rec tf f b t =
	match t with
	| Leaf x -> f (b,x)
	| Node (t1,t2) -> tf f (tf f b t1) t2


let to_list t =
	let f (b, x) = b@[x] in
	let b = [] in
	tf f b t

let size t =
	let f (b,x) = b+1 in
	let b = 0 in
	tf f b t

(*
let tf_tail f b t =
	let rec helper f b t =
		match t with
		| Leaf x -> f (b,x)
		| Node (t1,t2) -> let b' = tf f b t1
						  in tf f b' t2
	in helper f b t 

*)

type expr =
| Const of int
| Var of string
| Op of string * expr * expr;;

let rec rename_var e n1 n2 =
	match e with
	| Const i -> Const i
	| Var x -> if x = n1 then Var n2 else Var x
	| Op(s, e1, e2) -> Op(s, (rename_var e1 n1 n2), (rename_var e2 n1 n2))


let to_str e =
	let rec str_helper e top_level =
		match e with
		| Const i -> string_of_int i
		| Var x -> x
		| Op(s, e1, e2) -> if top_level = true then (str_helper e1 false) ^ s ^ (str_helper e2 false)
						   else "(" ^ (str_helper e1 false) ^ s ^ (str_helper e2 false) ^ ")"
	in str_helper e true;;


let average_if f l =
	let folding_fn (c, s) x =
		if f x then (c+1, s+x) else (c, s)
	in let base = (0, 0) in
	let (count, sum) = List.fold_left folding_fn base l in
	if count = 0 then 0 else (sum/count)


let length_2 l = List.fold_left (+) 0 (List.map (fun x -> List.length x) l )

let length_3 l = List.fold_left (+) 0 (List.map length_2 l)

let count l x = 
	let f a x' = if x' = x then a+1 else a
	in let b = 0
	in List.fold_left f b l

let make_palyndrome l =
	let f a x = [x]@a@[x]
	in let b = []
	in List.fold_left f b l

let fold_2 f b l =
	let ff (b', a) x = (f b' x a, a+1)
	in let base = (b, 0) 
	in let (c, i) = List.fold_left ff base l
	in c 

let rec ith l i d = fold_2 (fun x y z -> if z = i then y else x) d l

type 'a fun_tree =
| Leaf of ('a -> 'a)
| Node of ('a fun_tree) * ('a fun_tree);;

let rec apply_all t x =
	match t with
	| Leaf f -> f x
	| Node(fl, fr) -> apply_all fr (apply_all fl x)

let rec compose t1 t2 =
	match (t1,t2) with
	| Leaf f1, Leaf f2 -> Leaf(fun x -> f1 (f2 x))
	| Node (al, ar), Node (bl, br) -> Node((compose al bl), (compose ar br))
	| _,_ -> raise Not_found


let length l = List.fold_left (fun a x -> a+1) 0 l

let remove l x = List.fold_left (fun a x' -> if x' = x then a else a@[x']) [] l


let rec ith l i d =
	match l with
	| [] -> d
	| h::t -> if i = 0 then h else (ith t (i-1) d)


let rec update l i n =
	match l with
	| [] -> []
	| h::t -> if i = 0 then n::t else [h]@(update t (i-1) n) 

let rec update2 l i n d =
	match l with
	| [] -> if i = 0 then [n] else [d]@(update2 [] (i-1) n d)
	| h::t -> if i = 0 then n::t else [h]@(update2 t (i-1) n d)

let categorize f l =
	let base = [] in
	let fold_fn acc elmt = let i = (f elmt) in let res = (ith acc i [])@[elmt] in (update2 acc i res [])
	in List.fold_left fold_fn base l



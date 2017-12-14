(* CSE 130: Programming Assignment 3
 * misc.ml
 * Balachander Padmanabha
 * A53202177
 * bpadmana@ucsd.edu
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)

(*
 * taking each element and computing the square sum and storing in a.
 * Example:
 * # let sqsum xs = 
	  let f a x =  a+(x*x) in
	  let base = 0 in
	    List.fold_left f base xs;;
	val sqsum : int list -> int = <fun>
	# sqsum [1;2;3;4] ;;
	- : int = 30
 *)
let sqsum xs = 
  let f a x =  a+(x*x) in
  let base = 0 in
    List.fold_left f base xs

(*
 * pipe works by using curring. after taking the input we apply it using the function passed in the list.
 * Example:
 	# let pipe fs = 
	  let f a x = (fun z -> x(a(z))) in
	  let base = (fun x -> x) in
	    List.fold_left f base fs      
	  ;;
	val pipe : ('a -> 'a) list -> 'a -> 'a = <fun>
	# pipe [(fun x-> 2*x);(fun x -> x + 3)] 3 ;;
	- : int = 9
 *)
let pipe fs = 
  let f a x = (fun z -> x(a(z))) in
  let base = (fun x -> x) in
    List.fold_left f base fs

(*
 * function concats a list of strings with separator between them.
 * Example:
 	# let rec sepConcat sep sl = match sl with 
	  | [] -> ""
	  | h :: t -> 
	      let f a x = a ^ sep ^ x in
	      let base = h in
	      let l = t in
	        List.fold_left f base l            ;;
	val sepConcat : string -> string list -> string = <fun>
	# sepConcat "" ["a";"b";"c";"d";"e"];;
	- : string = "abcde"
 *)
let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = a ^ sep ^ x in
      let base = h in
      let l = t in
        List.fold_left f base l

(*
 * List.map will apply f to each element in list l which is then
 * feed to sepConcat with delimiter as "; " this gives required output.
 * Example:
 	# let stringOfList f l = "[" ^ sepConcat "; " (List.map f l) ^ "]";;
	val stringOfList : ('a -> string) -> 'a list -> string = <fun>
	# stringOfList (stringOfList string_of_int) [[1;2;3];[4;5];[6];[]];;
	- : string = "[[1; 2; 3]; [4; 5]; [6]; []]"
 *)
let stringOfList f l = "[" ^ sepConcat "; " (List.map f l) ^ "]"

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

(*
 * matching n to either negative/zero value we return empty list.
 * else we recursively call clone by reducing n value and appending lists.
 * Example:
 	# let rec clone x n = 
		match n with
	 	| n when n <= 0 -> []
		| _ -> List.append [x] (clone x (n-1))      ;;
	val clone : 'a -> int -> 'a list = <fun>
	# clone "foo" 2;;
	- : string list = ["foo"; "foo"]
 *)
let rec clone x n = 
	match n with
 	| n when n <= 0 -> []
	| _ -> List.append [x] (clone x (n-1))


(*
 * function finds which list is smaller and appends list of 0's to shorter list
 * clone is used to produce list of 0's
 * Example:
 	# let rec padZero l1 l2 =  
		if (List.length l1 > List.length l2) then
			(l1, (List.append (clone 0 (List.length l1 - List.length l2)) l2))
		else
			((List.append (clone 0 (List.length l2 - List.length l1)) l1), l2)        ;;
	val padZero : int list -> int list -> int list * int list = <fun>
	# padZero [1;0;0;2] [9;9];;
	- : int list * int list = ([1; 0; 0; 2], [0; 0; 9; 9])
 *)
let rec padZero l1 l2 =  
	if (List.length l1 > List.length l2) then
		(l1, (List.append (clone 0 (List.length l1 - List.length l2)) l2))
	else
		((List.append (clone 0 (List.length l2 - List.length l1)) l1), l2)

(*
 * function uses match on the head of the list
 * if its 0 it recurses over the tail else returns h::t
 * Example:
 	# let rec removeZero l = 
		match l with
		| [] -> []
		| h::t -> match h with
				  | 0 -> removeZero t
				  | _ -> h::t          ;;
	val removeZero : int list -> int list = <fun>
	# removeZero [0;0;0;1;0;0;2];;
	- : int list = [1; 0; 0; 2
 *)
let rec removeZero l = 
	match l with
	| [] -> []
	| h::t -> match h with
			  | 0 -> removeZero t
			  | _ -> h::t

(*
 * Takes 2 lists representing numbers and adds them together. 
 * not making use of carry which is being passed. Using head of list as my carry value.
 * Example:
 	# let bigAdd l1 l2 = 
	  let add (l1, l2) = 
	    let f a x = 
	        match x with 
	    	| (x', x'') -> 
	    				let temp = x' + x'' in
	    				match a with 
	    				| (carry, (h::t)) -> (carry, (List.append [(h+temp)/10] (List.append [(h+temp) mod 10] t)))
	    				| (carry, _) -> (carry, List.append [temp/10] [temp mod 10]) in
	    let base = (0, []) in
	    let args = List.combine (List.rev l1) (List.rev l2) in
	    let (_, res) = List.fold_left f base args in
	      res
	  in 
	    removeZero (add (padZero l1 l2))                            ;;
	val bigAdd : int list -> int list -> int list = <fun>
	# bigAdd [9;9;9;9] [9;9;9];;
	- : int list = [1; 0; 9; 9; 8]
 *)
let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x = 
        match x with 
    	| (x', x'') -> 
    				let temp = x' + x'' in
    				match a with 
    				| (carry, (h::t)) -> (carry, (List.append [(h+temp)/10] (List.append [(h+temp) mod 10] t)))
    				| (carry, _) -> (carry, List.append [temp/10] [temp mod 10]) in
    let base = (0, []) in
    let args = List.combine (List.rev l1) (List.rev l2) in
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))

(*
 * Here we are trying to multiply a big integer l (initially reversed for simlicity of logic) by a value i. 
 * It's making use of the carry "c" to keep track of each multiply. 
 * Example:
 	# let rec mulByDigit i l = 
		let multiply i l =
			let f a x =
				let (temp, acc) = a in
				let prod = (x * i) + temp in
				let d = prod mod 10 in
				let temp = prod / 10 in
				let res = d::acc in
				temp, res in
			let base = (0, []) in
			let args = List.rev l in
			let (c, res) = List.fold_left f base args in 
			c::res
		in 
		removeZero (multiply i l);;                            ;;
	val mulByDigit : int -> int list -> int list = <fun>
	# mulByDigit 9 [9;9;9;9];;
	- : int list = [8; 9; 9; 9; 1]
 *)
let rec mulByDigit i l = 
	let multiply i l =
		let f a x =
			let (temp, acc) = a in
			let prod = (x * i) + temp in
			let d = prod mod 10 in
			let temp = prod / 10 in
			let res = d::acc in
			temp, res in
		let base = (0, []) in
		let args = List.rev l in
		let (c, res) = List.fold_left f base args in 
		c::res
	in 
	removeZero (multiply i l);;

(*
 * taking l1 as the args the function uses bigAdd and mulByDigit to caculate l1 multiplied by l2.
 * Example:
 	# let bigMul l1 l2 = 
	  let f a x = 
	  		match a with
	  		| (carry, temp) -> (carry+1, bigAdd temp (List.append (mulByDigit x l2) (clone 0 carry)))
	  		in
	  let base = (0, []) in
	  let args = removeZero l1 in
	  let (_, res) = List.fold_left f base args in
	    res                ;;
	val bigMul : int list -> int list -> int list = <fun>
	# bigMul [9;9;9;9;9] [9;9;9;9;9];;
	- : int list = [9; 9; 9; 9; 8; 0; 0; 0; 0; 1]
 *)
let bigMul l1 l2 = 
  let f a x = 
  		match a with
  		| (carry, temp) -> (carry+1, bigAdd temp (List.append (mulByDigit x l2) (clone 0 carry)))
  		in
  let base = (0, []) in
  let args = removeZero l1 in
  let (_, res) = List.fold_left f base args in
    res



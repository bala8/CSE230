(* CSE 130: Programming Assignment 1
 * misc.ml
 * Name: Balachander Padmanabha
 * PID: A53202177
 * Email: bpadmana@ucsd.edu
 *)

(* sumList : int list -> int 
   iterate the list using the head and recursively call sumList on the tail 
   to get the sum of the list elements.
   Example:
   	# let rec sumList l = match l with [] -> 0 | h::t -> h+(sumList t);;
	val sumList : int list -> int = <fun>
	# let l = [1;2;3;4];;
	val l : int list = [1; 2; 3; 4]
	# sumList l;;
	- : int = 10
*) 

let rec sumList l = match l with 
			| [] -> 0 
			| h::t -> h+(sumList t);;


(* digitsOfInt : int -> int list 
   iterate over digits in the integer and append the digit to resultant list via custom append fucntion.
   runs untill we reach 0. returns empty list for negative integers.
   Example:
	# let rec digitsOfInt n = 
				let rec custom_append l num =
					match l with
					| [] -> [num]
					| h::t -> h::(custom_append t num)
				in
				match n with
					| 0 -> []
					| n when n < 0 -> []
					| _ -> custom_append (digitsOfInt(n/10)) (n mod 10)                  
	  ;;
	val digitsOfInt : int -> int list = <fun>
	# digitsOfInt 12345;;
	- : int list = [1; 2; 3; 4; 5]
	# digitsOfInt (-12345)
	  ;;
	- : int list = []
 *)

let rec digitsOfInt n = 
				let rec custom_append l num = (* custom append function to append num at end of list l *)
					match l with
					| [] -> [num]
					| h::t -> h::(custom_append t num)
				in
				match n with
					| 0 -> []
					| n when n < 0 -> [] 
					| _ -> custom_append (digitsOfInt(n/10)) (n mod 10)

(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)
 
let digits n = digitsOfInt (abs n)

(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required to obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)


(* ***** PROVIDE COMMENT BLOCKS FOR THE FOLLOWING FUNCTIONS ***** *)

(*
 * recursively check if number is greater than 9. 
 * If it is, increment the result and recurse over the new number generated by suming digits of original integer.
 * If it is not, return 0 to stop recursion.
 * Example:
 	# let rec additivePersistence n = if(n>9) then (additivePersistence(sumList(digits n))+1) else 0;;
	val additivePersistence : int -> int = <fun>
	# additivePersistence 12345;;
	- : int = 2
 *)

let rec additivePersistence n = if(n>9) then (additivePersistence(sumList(digits n))+1) else 0;;

(*
 * recursively iterate over the interger if its greater than 9.
 * first get a list of digits for the integer, then sum the list giving transformed integer.
 * recurse over new integer if its greater than 9 else stop and return the integer.
 * Example:
 	# let rec digitalRoot n = if(n>9) then (digitalRoot(sumList(digits n))) else n;;
	val digitalRoot : int -> int = <fun>
	# digitalRoot 12345;;
	- : int = 6
 *)

let rec digitalRoot n = if(n>9) then (digitalRoot(sumList(digits n))) else n;;

(*
 * recurse through the list using the tail and use custom append 
 * to a resultant list using the head at the end of it.
 * return empty list if the list is empty.
 * Example:
 	# let rec listReverse l = 
			let rec cus_append l i =
			  match l with
			  | [] -> [i]
			  | h::t -> h::(cus_append t i)
			in
			match l with 
				| [] -> [] 
				| h::t -> cus_append (listReverse(t)) (h)
                  
	  ;;
	val listReverse : 'a list -> 'a list = <fun>
	# listReverse [1;2;3;4;5;6;7;8;9];;
	- : int list = [9; 8; 7; 6; 5; 4; 3; 2; 1]
 *)

let rec listReverse l = 
			let rec custom_append l num = (* custom append function to append num at end of list l *)
			  match l with
			  | [] -> [num]
			  | h::t -> h::(custom_append t num)
			in
			match l with 
				| [] -> [] 
				| h::t -> custom_append (listReverse(t)) (h)

(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0

(* function used to find if string is palindrome.
 * using recursive helper function which takes two lists,
 * normal_list and reverse_list, checks head element equality and recurses over the tail.
 * using explode and listReverse function declared above.
 * Example:
 	# let palindrome w =  
		let rec helper nl rl = 
			match nl, rl with
			| [], [] -> true
			| nh::nt, rh::rt -> (nh = rh && helper nt rt)
			| [], _ -> false
			| _, [] -> false
		in
		let a = explode w in
		let b = listReverse a in
		helper a b;;                    
	val palindrome : string -> bool = <fun>
	# palindrome "malayalam";;
	- : bool = true
 *)

let palindrome w =  
	let rec helper nl rl = 
		match nl, rl with
		| [], [] -> true
		| nh::nt, rh::rt -> (nh = rh && helper nt rt)
		| [], _ -> false
		| _, [] -> false
	in
	let a = explode w in
	let b = listReverse a in
	helper a b;;

(************** Add Testing Code Here ***************)


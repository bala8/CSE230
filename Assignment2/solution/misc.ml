(* CSE 130: Programming Assignment 2
 * Balachander Padmanabha
 * A53202177
 * bpadmana@ucsd.edu
 * misc.ml
 *)

(* ***** DOCUMENT ALL FUNCTIONS YOU WRITE OR COMPLETE ***** *)

(*
 * Function uses recursion to find first key equal to k in list l 
 * returns the corresponding value stored else returns d by default.
 * Example:
    # let rec assoc (d,k,l) = 
      match l with
      | [] -> d
      | h::t -> match h with
                  | (a,b) -> if a = k then b else assoc (d, k, t);;
      val assoc : 'a * 'b * ('b * 'a) list -> 'a = <fun>
      # assoc (-1,"jeff",[("sorin",85);("jeff",23);("moose",44)]);;
      - : int = 23 
 *)
let rec assoc (d,k,l) = 
    match l with
    | [] -> d
    | h::t -> match h with
                | (a,b) -> if a = k then b else assoc (d, k, t)

(* fill in the code wherever it says : failwith "to be written" *)
(*
 * Function uses a recursive helper funciton to return list with no duplicates
 * it maintains the ordering of the original numbers in the list. 
 * Example:
    # let removeDuplicates l = 
        let rec helper (seen,rest) = 
            match rest with 
            | [] -> seen
            | h::t -> 
              let seen' = match List.mem h seen with
                          | true -> seen
                          | false -> h::seen
              in
              let rest' = t in 
          helper (seen',rest') 
        in
            List.rev (helper ([],l))                        ;;
    val removeDuplicates : 'a list -> 'a list = <fun>
    # removeDuplicates [1;6;2;4;12;2;13;6;9];;
    - : int list = [1; 6; 2; 4; 12; 13; 9] 
 *)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
      | [] -> seen
      | h::t -> 
        let seen' = match List.mem h seen with
                    | true -> seen
                    | false -> h::seen
        in
        let rest' = t in 
	  helper (seen',rest') 
  in
      List.rev (helper ([],l))


(* Small hint: see how ffor is implemented below *)
(*
 * Function uses a recursion to apply f on b until c' is true.
 * when its false it returns b'
 * Example:
    # let rec wwhile (f,b) = 
        match f b with
        | (b', c') -> match c' with 
                       | true -> wwhile (f,b')
                       | false -> b';;
    val wwhile : ('a -> 'a * bool) * 'a -> 'a = <fun>
    # let f x = let xx = x*x*x in (xx,xx<100);; 
    val f : int -> int * bool = <fun>
    # wwhile (f,2);; 
    - : int = 512
 *)
let rec wwhile (f,b) = 
    match f b with
    | (b', c') -> match c' with 
                   | true -> wwhile (f,b')
                   | false -> b'

(* fill in the code wherever it says : failwith "to be written" *)
(*
 * Example:
    # let fixpoint (f,b) = wwhile ((
          let helper b = 
              let temp = f b in
                if b = temp then (temp, false) else (temp, true)   
          in helper), b)        ;;
    val fixpoint : ('a -> 'a) * 'a -> 'a = <fun>
    # let g x = truncate (1e6 *. cos (1e-6 *. float x));;
    val g : int -> int = <fun>
    # fixpoint (g,0);;
    - : int = 739085
 *)
let fixpoint (f,b) = wwhile ((
          let helper b = 
              let temp = f b in
                if b = temp then (temp, false) else (temp, true)   
          in helper), b)


(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)


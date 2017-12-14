exception MLFailure of string

type binop = 
  Plus 
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False      
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil                    
| Pair of value * value 
| Native of string    

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"
  | Native x -> Printf.sprintf "%s" x

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | NilExpr -> Printf.sprintf ""

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

(*
 * the lookup function will search for first occurance of x in evn list
 * first occurance from the left side and return that.
 *)
let lookup (x, evn) = 
  match listAssoc(x, evn) with
  | Some x -> x
  | _ -> raise (MLFailure ("variable not bound: " ^ x))

let rec eval (evn, e) = 
  match e with 
  | Const x -> Int x
  | True  -> Bool true 
  | False -> Bool false     
  | NilExpr -> Nil
  | Var x -> if x = "hd" then Native("hd") else
             if x = "tl" then Native("tl") else lookup(x, evn)
  | Bin(e1, oper, e2) ->
     (match ((eval(evn, e1)), oper, (eval(evn, e2))) with
       | (Int e1, Minus, Int e2) -> Int(e1 - e2) 
       | (Int e1, Plus, Int e2) -> Int(e1 + e2) 
       | (Int e1, Div, Int e2) -> (match e2 = 0 with 
                                  | true -> raise (MLFailure("Divide by 0 error!"))
                                  | _ -> Int(e1 / e2))
       | (Int e1, Mul, Int e2) -> Int(e1 * e2)
       | (Int e1, Ne, Int e2) -> Bool(e1 != e2)
       | (Int e1, Eq, Int e2) -> Bool(e1 = e2)
       | (Int e1, Lt, Int e2) -> Bool(e1 < e2)
       | (Int e1, Le, Int e2) -> Bool(e1 <= e2)
       | (Bool e1, And, Bool e2) -> Bool(e1 && e2)
       | (Bool e1, Or, Bool e2) -> Bool(e1 || e2)
       | (Bool e1, Ne, Bool e2) -> Bool(e1 != e2)
       | (Bool e1, Eq, Bool e2) -> Bool(e1 = e2)
       | (_ , Cons, _) -> let f2 = eval(evn, e2) in
                          let f1 = eval(evn, e1) in 
                          (match f2 with
                            | Pair(a,b) -> Pair(f1, f2)
                            | Nil -> Pair(f1, f2)
                            | _ -> raise (MLFailure ("Error: List doesn't terminate correctly!")))
       | (_,_,_) -> raise (MLFailure ("Error: Not a valid Operation!"))
      )
  | If(e, t, f) -> if eval(evn, e) = (Bool true) then eval(evn, t) else eval(evn, f)
  | Let(x, e1, e2) -> let evn = (x, eval(evn, e1))::evn in eval(evn, e2)
  | App(e1, e2) -> let arg = eval(evn, e2) in
      (match eval(evn, e1) with
      | Closure(evn_update, Some f, x, e) as close -> eval((f, close)::(x, arg)::evn_update@evn, e)
      | Closure(evn_update, None, x, e) -> eval((x, arg)::evn_update@evn, e)
      | Native ("hd") -> (match arg with 
                         | Pair(x,y) -> x
                         | _ -> raise (MLFailure ("Error: hd not a pair")))
      | Native ("tl") -> (match arg with 
                         | Pair(x,y) -> y
                         | _ -> raise (MLFailure ("Error: tl not a pair")))
      | _ -> raise (MLFailure ("Error: unknown App!")))
  | Fun(e1, e2) -> Closure(evn, None, e1, e2)
  | Letrec(x, e1, e2) -> let temp = eval(evn, e1) in
                         let temp1 = (match temp with 
                             | Closure(evn, n, arg, e) -> Closure(evn, Some x, arg, e)
                             | _ -> temp) in let evn = (x, temp1)::evn in eval(evn, e2)


(**********************     Testing Code  ******************************)

(* Programming language concepts for software developers, 2010-08-28 *)

(* Evaluating simple expressions with variables *)

module Intro2

(* Association lists map object language variables to their values *)

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)];;

let emptyenv = []; (* the empty environment *)

let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let cvalue = lookup env "c";;


(* Object language expressions with variables *)

type expr = 
  | CstI of int
  | Var of string
  | Prim of string * expr * expr
  | CondExpress of expr * expr * expr;;

type aexpr = 
  | ACstI of int //Needs to be named something different compared to expr
  | AVar of string // --//--
  | Add of aexpr * aexpr
  | Mul of aexpr * aexpr
  | Sub of aexpr * aexpr;;

let e1 = CstI 17;;

let e2 = Prim("+", CstI 3, Var "a");;

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;


//Exercise 1.2
let ae1 : aexpr = Sub(AVar("v"), 
            Add(AVar("w"),AVar("z")));;

let ae2 : aexpr  = Mul(ACstI(2), 
            Sub(AVar("v"),
              Add(AVar("w"), AVar("z"))));;

let ae3 : aexpr  = Add(AVar "x", 
          Add(AVar "y", 
          Add(AVar "z", AVar "w")));;

(* Evaluation within an environment *)

let rec eval e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | Prim(ope, e1, e2) ->
      let i1 = eval e1 env
      let i2 = eval e2 env
      match ope with
      | "+" -> i1 + i2
      | "*" -> i1 * i2
      | "-" -> i1 - i2
      | "max" -> if (i1 > i2) then i1 else i2
      | "min" -> if (i1 < i2) then i1 else i2
      | "==" -> if(i1 = i2) then 1 else 0 
      | _ -> failwith "unknown primitive"
    | CondExpress(e1, e2, e3) ->
      match eval e1 env with
      | 0 -> eval e3 env
      | _ -> eval e2 env

let rec fmt (ae: aexpr) : string =
  match ae with
  | ACstI(i) -> sprintf "%d" i
  | Add(ae1, ae2) -> sprintf "(%s + %s)" (fmt ae1) (fmt ae2)
  | Mul(ae1, ae2) -> sprintf "(%s * %s)" (fmt ae1) (fmt ae2)
  | Sub(ae1, ae2) -> sprintf "(%s - %s)" (fmt ae1) (fmt ae2)
  | _ -> "not int";;

let rec simplify (ae: aexpr) : aexpr =
  match ae with 
  | Add(ae1, ae2) ->
    match (ae1, ae2) with
    | (ACstI(0), i) -> i
    | (i, ACstI(0)) -> i
    | _ -> Add(ae1, ae2)
  | Sub(ae1, ae2) ->
    match (ae1, ae2) with
    | (ACstI(0), i) -> i
    | (i, ACstI(0)) -> i
    | (ACstI(x), ACstI(y)) when x = y -> ACstI(0)
    | _ -> Add(ae1, ae2)
  | ACstI(i) -> ACstI(i);;

(*
  0 + e −→ e
  e + 0 −→ e
  e − 0 −→ e
1 ∗ e −→ e
e ∗ 1 −→ e
0 ∗ e −→ 0
e ∗ 0 −→ 0
  e − e −→ 0
*)

let e1v  = eval e1 env;;
let e2v1 = eval e2 env;;
let e2v2 = eval e2 [("a", 314)];;
let e3v  = eval e3 env;;

let max1 = eval (Prim("max", e1, e2)) env;;
let max2 = eval (Prim("max", e3, e2)) env;;
let min1 = eval (Prim("min", e1, e2)) env;;
let min2 = eval (Prim("min", e3, e2)) env;;
let same1 = eval (Prim("==", e1, e2)) env;;
let same2 = eval (Prim("==", e2, e2)) env;;
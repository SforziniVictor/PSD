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

let e1 = CstI 17;;

let e2 = Prim("+", CstI 3, Var "a");;

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;


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
      | _ -> failwith "unknown primitive";;

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
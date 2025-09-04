(* Programming language concepts for software developers, 2012-02-17 *)

(* Evaluation, checking, and compilation of object language expressions *)
(* Stack machines for expression evaluation                             *) 

(* Object language expressions with variable bindings and nested scope *)

module Intcomp1

type expr = 
  | CstI of int
  | Var of string
  | Let of (string * expr) list * expr
  | Prim of string * expr * expr;;

let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let rec eval e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | Let(varExpress, ebody) ->
      let rec letEval (exprs: (string * expr) list) (env : (string * int) list) : (string * int) list = 
        match exprs with
        | (s,e) :: x -> 
          let xval = eval e env
          let env1 = (s, xval) :: env 
          letEval x env1 
        | (s, e) :: [] ->
          let xval = eval e env
          let env1 = (s, xval) :: env 
          env1
        | _ -> env
      eval ebody (letEval varExpress env)
    | Prim("+", e1, e2) -> eval e1 env + eval e2 env
    | Prim("*", e1, e2) -> eval e1 env * eval e2 env
    | Prim("-", e1, e2) -> eval e1 env - eval e2 env
    | Prim _            -> failwith "unknown primitive";;

let e1 = eval (Let([("x1", CstI 1); ("x2", CstI 5)], Prim("+", Var "x1", Var "x2"))) [];;
let e2 = eval (Let([("x1", CstI 2); ("x2", Prim("+", Var "x1", CstI 7))], Prim("+", Var "x1", Var "x2"))) [];;

let run e = eval e [];;

(* ---------------------------------------------------------------------- *)

(* Closedness *)

// let mem x vs = List.exists (fun y -> x=y) vs;;

let rec mem x vs = 
    match vs with
    | []      -> false
    | v :: vr -> x=v || mem x vr;;

(* Checking whether an expression is closed.  The vs is 
   a list of the bound variables.  *)

(* ---------------------------------------------------------------------- *)

(* Substitution of expressions for variables *)

(* This version of lookup returns a Var(x) expression if there is no
   pair (x,e) in the list env --- instead of failing with exception: *)

let rec lookOrSelf env x =
    match env with 
    | []        -> Var x
    | (y, e)::r -> if x=y then e else lookOrSelf r x;;

(* Remove (x, _) from env: *)

let rec remove env x =
    match env with 
    | []        -> []
    | (y, e)::r -> if x=y then r else (y, e) :: remove r x;;

(* Naive substitution, may capture free variables: *)
let rec union (xs, ys) = 
    match xs with 
    | []    -> ys
    | x::xr -> if mem x ys then union(xr, ys)
               else x :: union(xr, ys);;

(* minus xs ys  is the set of all elements in xs but not in ys *)

let rec minus (xs, ys) = 
    match xs with 
    | []    -> []
    | x::xr -> if mem x ys then minus(xr, ys)
               else x :: minus (xr, ys);;

(* Find all variables that occur free in expression e *)

let rec freevars e : string list =
    match e with
    | CstI i -> []
    | Var x  -> [x]
    | Let(xList, ebody) -> 
          let rec freemanyvars (xList : (string * expr) list) : string list =
            match xList with
            | [] -> []
            | (_, e) :: x -> union (freevars e, freemanyvars x)
          let bounded = List.map fst xList
          let defs = freemanyvars xList
          let body = minus (freevars ebody, bounded)
          union (defs, body)
    | Prim(ope, e1, e2) -> union (freevars e1, freevars e2);;
type expr = 
  | Const of float 
  | Var of string 
  | Plus of expr * expr 
  | Times of expr * expr

type env = (string * float) list

let e = Plus (Const 2.0, Var "x")  (* Valid because expr is defined *)
let environment = [("x", 3.0)]      (* Valid because env is defined *)
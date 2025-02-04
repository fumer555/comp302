type expr = 
  | Const of float 
  | Var of string 
  | Plus of expr * expr 
  | Times of expr * expr

type env = (string * float) list

let mystery f g x = match x with
  | Some y -> f y
  | None -> g ()
    
let rec lookup key lst =
  match lst with
  | [] -> None
  | (k, v) :: rest -> if k = key then Some v else lookup key rest (* from 3a*)
    
let rec eval expr env =
  match expr with
  | Const c -> Some c
  | Var v -> lookup v env
  | Plus (e1, e2) ->
      mystery 
        (fun v1 -> mystery (fun v2 -> Some (v1 +. v2)) (fun () -> None) (eval e2 env))
        (fun () -> None)
        (eval e1 env)
  | Times (e1, e2) ->
      mystery 
        (fun v1 -> mystery (fun v2 -> Some (v1 *. v2)) (fun () -> None) (eval e2 env))
        (fun () -> None)
        (eval e1 env)
  

(* Test cases *)
let env = [("x", 2.0); ("y", 3.0)]  (* Environment with known variables *)

let test1 = eval (Plus (Const 1.0, Const 2.0)) env  
(* Expected: Some 3.0 *)

let test2 = eval (Times (Var "x", Var "y")) env  
(* Expected: Some 6.0 (since x = 2.0 and y = 3.0) *)

let test3 = eval (Plus (Var "x", Var "z")) env  
(* Expected: None (fails because "z" is unknown in env) *)



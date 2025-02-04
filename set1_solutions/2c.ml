let x = 2 in 
let f g = g (x + 1) in 
let x = 5 in 
f (fun y -> x * y)

(* 6 *)
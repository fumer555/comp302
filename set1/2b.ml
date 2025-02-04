let sum lst =
    let rec helper acc lst =
      match lst with
      | [] -> acc
      | h :: t -> helper (acc + h) t
    in helper 0 lst 
      
let result = sum [1; 2; 3]

(* 6 *)
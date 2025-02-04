let rec lookup key lst sc fc =
  match lst with
  | [] -> fc ()  (* Key not found, invoke failure continuation *)
  | (k, v) :: rest ->
      if k = key then sc v  (* Key found, invoke success continuation *)
      else lookup key rest sc fc  (* Continue searching *)

(*
let _ = lookup "x" [("x", 2); ("y", 3)]   (* (int -> '_weak4) -> (unit -> '_weak4) -> '_weak4 = <fun> *)
let _ = lookup "z" [("x", 2)]             (* (int -> '_weak4) -> (unit -> '_weak4) -> '_weak4 = <fun> *)
let _ = lookup "x" [("x", 1); ("x", 2)]   (* (int -> '_weak4) -> (unit -> '_weak4) -> '_weak4 = <fun> *)
*)
(* Test cases with explicit continuations *)
let _ =
  let print_result res =
    Printf.printf "Result: %s\n" res
  in
  lookup "x" [("x", 2); ("y", 3)] 
    (fun v -> print_result ("Found: " ^ string_of_int v))  (* Success case *)
    (fun () -> print_result "Not found");  (* Failure case *)

  lookup "z" [("x", 2)] 
    (fun v -> print_result ("Found: " ^ string_of_int v)) 
    (fun () -> print_result "Not found");

  lookup "x" [("x", 1); ("x", 2)] 
    (fun v -> print_result ("Found: " ^ string_of_int v)) 
    (fun () -> print_result "Not found")

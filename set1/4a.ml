let rec lookup key lst k =
  match lst with
  | [] -> k None  (* If the key is not found, invoke k with None *)
  | (k', v) :: rest -> 
      if k' = key then k (Some v)  (* If the key matches, invoke k with Some v *)
      else lookup key rest k  (* Continue searching with the same continuation k *)



(*
let _ = lookup "x" [("x", 2); ("y", 3)]   (* (int option -> '_weak9) -> '_weak9 = <fun> *)
let _ = lookup "z" [("x", 2)]             (* (int option -> '_weak9) -> '_weak9 = <fun> *)
let _ = lookup "x" [("x", 1); ("x", 2)]   (* (int option -> '_weak9) -> '_weak9 = <fun> *)
*)
(* Test cases with explicit continuations *)
let () =
  let print_result opt =
    match opt with
    | None -> print_endline "Not found"
    | Some v -> Printf.printf "Found: %d\n" v
  in
  lookup "x" [("x", 2); ("y", 3)] print_result;
  lookup "z" [("x", 2)] print_result;
  lookup "x" [("x", 1); ("x", 2)] print_result


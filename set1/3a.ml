let rec lookup key lst =
  match lst with
  | [] -> None
  | (k, v) :: rest -> if k = key then Some v else lookup key rest

let _ = lookup "x" [("x", 2); ("y", 3)]   (* Some 2 *)
let _ = lookup "z" [("x", 2)]             (* None *)
let _ = lookup "x" [("x", 1); ("x", 2)]   (* Some 1 *)

let example1 = match lookup 2 [(1, "a"); (2, "b"); (3, "c")] with
  | Some v -> "Found: " ^ v
  | None -> "Not found"

let example2 = match lookup 4 [(1, "a"); (2, "b"); (3, "c")] with
  | Some v -> "Found: " ^ v
  | None -> "Not found


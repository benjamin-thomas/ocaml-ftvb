let add a b = a + b

let is_upper2 c =
  match c with
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let%test _ = is_upper2 'c' = false
let%test _ = is_upper2 'A' = true
let%test _ = is_upper2 'B' = true
let%test _ = is_upper2 'C' = true
let%test _ = is_upper2 'D' = true
let%test _ = is_upper2 'd' = false

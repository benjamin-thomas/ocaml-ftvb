let max_non_empty h t =
  List.fold_left
    (fun mx n ->
      if n > mx then
        n
      else
        mx)
    h t

let%test _ = max_non_empty 1 [ 2; 4; 3 ] = 4
let%test _ = max_non_empty 5 [ 2; 4; 3 ] = 5

let greatest max n =
  if n > max then
    n
  else
    max

let max_non_empty' h = List.fold_left greatest h

let%test _ = max_non_empty' 1 [ 2; 4; 3 ] = 4
let%test _ = max_non_empty' 5 [ 2; 4; 3 ] = 5

let rec get lst countdown =
  match lst with
  | [] -> failwith "Index is too big!"
  | h :: t ->
      let res =
        if countdown = 0 then
          h
        else
          get t (countdown - 1)
      in
      res

let%test_unit _ = [%test_eq: Base.int] 1 @@ get [ 1; 2; 3; 4 ] 0
let%test_unit _ = [%test_eq: Base.int] 2 @@ get [ 1; 2; 3; 4 ] 1
let%test_unit _ = [%test_eq: Base.int] 3 @@ get [ 1; 2; 3; 4 ] 2
let%test_unit _ = [%test_eq: Base.int] 4 @@ get [ 1; 2; 3; 4 ] 3

let swap_with_idx_zero lst new_idx =
  let a = get lst 0 in
  let b = get lst new_idx in
  (* let _ = Printf.printf "(a, b) = (%d, %d)\n" a b in *)
  let rec inner lst cnt =
    match lst with
    | [] -> []
    | h :: t ->
        let v =
          if cnt = 0 then
            (* let _ = Printf.printf "0 => %d\n" b in *)
            b
          else if cnt = new_idx then
            (* let _ = Printf.printf "%d ~> %d\n" new_idx a in *)
            a
          else
            (* let _ = Printf.printf "%d -> %d\n" cnt h in *)
            h
        in
        let res = v :: inner t (cnt + 1) in
        (* let _ = Core.print_s [%sexp (res : Base.int Base.list)] in *)
        (* let _ = Printf.printf "%s\n" (show_foo res) in *)
        res
  in
  inner lst 0

(* 3 -> [4;2;3;1] *)
let%test_unit _ =
  [%test_eq: Base.int Base.list] [ 4; 2; 3; 1 ]
  @@ swap_with_idx_zero [ 1; 2; 3; 4 ] 3
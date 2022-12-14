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
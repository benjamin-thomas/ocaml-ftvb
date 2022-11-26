let add a b = a + b

let%test _ = add 1 1 = 2
let%test _ = 2 |> add 1 = 3

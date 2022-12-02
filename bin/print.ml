let nums_to_string nums = nums |> List.map string_of_int |> String.concat ", "

(* dune exec bin/print.exe *)
(* dune exec --display=quiet bin/print.exe *)
let () =
  let _ = Printf.printf "\n\nPROGRAM START:\n\n" in
  let sorted = Lib.Chapter05.inssort' Asc [ 1; 4; 3; 5; 2 ] in
  Printf.printf "Sorted: %s\n" @@ nums_to_string sorted

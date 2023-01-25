(*
   Text statistics

   To run, see: Procfile.dev
 *)

type stats = int * int * int * int

let lines (l, _, _, _) = l
let chars (_, c, _, _) = c
let words (_, _, w, _) = w
let sentences (_, _, _, s) = s

let stats_of_channel ic =
  let lines = ref 0 in
  let chars = ref 0 in
  let words = ref 0 in
  let sentences = ref 0 in
  try
    while true do
      let line = input_line ic in
      lines := !lines + 1
      ; chars := !chars + String.length line
      ; String.iter
          (fun c ->
            match c with
            | '.'
            | '?'
            | '!' ->
                sentences := !sentences + 1
            | ' ' -> words := !words + 1
            | _ -> ())
          line
    done
    ; (0, 0, 0, 0)
  with
  | End_of_file -> (!lines, !chars, !words, !sentences)
;;

let stats_of_file (path : string) : stats =
  let chan = open_in path in
  let stats = stats_of_channel chan in
  close_in chan
  ; stats
;;

(* let%test_unit _ =
     let open Base in
     let path = "../../../../../lib/chapter13.txt" in
     [%test_eq: int * int * int * int] (stats_of_file path) @@ (8, 461, 77, 4)
   ;; *)

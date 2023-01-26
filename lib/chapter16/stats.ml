(*
   echo ./stats.ml | entr -c bash -c 'ocamlc ./text_stats.ml{i,} ./stats.ml && ./a.out ../chapter13.txt'

   ocamlc ./text_stats.ml{i,} ./stats.ml
   ocamlopt ./text_stats.ml{i,} ./stats.ml
*)

module Ansi = struct
  let red = "\027[31m"
  let rst = "\027[m"
end

type path = Valid_path of string

let print_stats (Valid_path p) =
  let s = Text_stats.stats_of_file p in

  print_endline @@ "Stats for: " ^ p
; print_newline ()

; print_string "Words     : "
; print_int @@ Text_stats.words s
; print_newline ()

; print_string "Chars     : "
; print_int @@ Text_stats.chars s
; print_newline ()

; print_string "Sentences : "
; print_int @@ Text_stats.sentences s
; print_newline ()

; print_string "Lines     : "
; print_int @@ Text_stats.lines s
; print_newline ()
;;

let validate path =
  if Sys.file_exists path then
    Ok (Valid_path path)
  else
    Error ("file does not exist: " ^ "'" ^ path ^ "'")
;;

let valid_path_result =
  match Sys.argv with
  | [|_; path|] -> validate path
  | _           -> Error "must provide file name"
;;

let () =
  match valid_path_result with
  | Ok vp -> print_stats vp
  | Error reason ->
    print_endline @@ Ansi.red ^ "ERROR: " ^ reason ^ Ansi.rst
  ; print_endline "Usage: ./stats.exe PATH"
;;

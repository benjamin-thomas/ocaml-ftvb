(*
  See ./lib/grep_lib.ml for run instructions
*)

module Ansi = struct
  let red = "\027[31m"
  let rst = "\027[m"
end

let usage () = print_endline "Usage: grep.exe FRAG PATH"

type 'a error = Error of 'a

let do_exit (Error msg) code =
  print_endline @@ Ansi.red ^ "ERROR => " ^ msg ^ Ansi.rst
  ; usage ()
  ; exit code
;;

let run frag path =
  match Grep_lib.valid_path_of_string path with
  | Error msg -> do_exit (Error msg) 2
  | Ok vp ->
      print_endline "Results:"
      ; Grep_lib.filter ~frag vp |> print_endline
;;

let () =
  match Sys.argv with
  | [| _; frag; path |] -> run frag path
  | _ -> do_exit (Error "missing one or more arguments") 1
;;

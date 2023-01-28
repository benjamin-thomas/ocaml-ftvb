(*
  See Rev_lib entry point for build/run instructions

  Write and compile a standalone program to reverse the lines in a text file, writing to another file.
*)

module Ansi = struct
  let red = "\027[31m"
  let rst = "\027[m"
end

let usage () = print_endline "Usage: rev.exe PATH"

type 'a error = Error of 'a

let do_exit (Error msg) code =
  print_endline @@ Ansi.red ^ "ERROR => " ^ msg ^ Ansi.rst
  ; usage ()
  ; exit code
;;

let run path =
  match Rev_lib.valid_path_of_string path with
  | Ok vp -> Rev_lib.rev_file vp
  | Error msg -> do_exit (Error msg) 2
;;

let () =
  match Sys.argv with
  | [| _; path |] -> run path
  | _ -> do_exit (Error "missing argument") 1
;;

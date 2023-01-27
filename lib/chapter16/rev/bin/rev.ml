(*
  See Rev_lib entry point for build/run instructions

  Write and compile a standalone program to reverse the lines in a text file, writing to another file.
*)

module Ansi = struct
  let red = "\027[31m"
  let rst = "\027[m"
end

type error = App_error of string

let usage () = print_endline "Usage: rev.exe PATH"

let do_exit (App_error msg) code =
  print_endline @@ Ansi.red ^ "ERROR => " ^ msg ^ Ansi.rst
  ; usage ()
  ; exit code
;;

(* let do_exit (Error msg) code =
     print_endline @@ Ansi.red ^ "ERROR => " ^ msg ^ Ansi.rst
     ; usage ()
     ; exit code
   ;; *)

let run path =
  match Rev_lib.valid_path_of_string path with
  | Ok vp -> Rev_lib.rev_file vp
  | Error msg -> do_exit (App_error msg) 2
;;

let () =
  match Sys.argv with
  | [| _; path |] -> run path
  | _ -> do_exit (App_error "missing argument") 1
;;

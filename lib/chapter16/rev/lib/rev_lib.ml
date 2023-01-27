(*
  Interact with the REPL: (see local .ocamlinit)

    while true;do utop;sleep 0.1;done

  Run binary

    dune exec --display=quiet ./bin/rev.exe

  NOTE:

    With this setup, at no point do I need to manually build (neither via a watch mode)!

    Unused declaration warnings will trigger when building the binary

    Unused declaration warnings will trigger for the lib, but only once an mli interface
    has been created (everything is public otherwise)
*)

(*
  Write and compile a standalone program to reverse the lines in a text file, writing to another file.
*)

type path = Valid_path of string

let valid_path_of_string p =
  if not @@ Sys.file_exists p then
    Error ("file does not exist: " ^ p)
  else
    Ok (Valid_path p)
;;

let rev_file (Valid_path p) = print_endline @@ "Ready to work on: " ^ p
let hello = 23

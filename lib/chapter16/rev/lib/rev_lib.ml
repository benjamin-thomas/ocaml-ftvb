(*
  Interact with the REPL: (see local .ocamlinit)

    while true;do utop;sleep 0.1;done

  Run binary

    dune exec --display=quiet ./bin/rev.exe
    dune exec --display=quiet ./bin/rev.exe /tmp/tmp && cat /tmp/tmp.rev


  NOTE:

    With this setup, at no point do I need to manually build (neither via a watch mode)!

    Unused declaration warnings will trigger when building the binary

    Unused declaration warnings will trigger for the lib, but only once an mli interface
    has been created (everything is public otherwise)
*)

(*
  Write and compile a standalone program to reverse the lines in a text file, writing to another file.
*)

type valid_path = Valid_path of string

let valid_path_of_string p =
  if not @@ Sys.file_exists p then
    Error ("file does not exist: " ^ p)
  else
    Ok (Valid_path p)
;;

let input_line_opt ic =
  try Some (input_line ic) with
  | End_of_file -> None
;;

let do_rev ic oc =
  let next () = input_line_opt ic in

  let new_lines =
    Seq.fold_left (fun acc x -> x :: acc) [] (Seq.of_dispenser next)
  in
  let output_line line = output_string oc (line ^ "\n") in
  let put_lines = List.iter output_line in

  new_lines |> put_lines
;;

let rev_file (Valid_path p) =
  let ic = open_in p in
  let oc = open_out (p ^ ".rev") in
  do_rev ic oc
  ; close_in ic
  ; close_out oc
;;

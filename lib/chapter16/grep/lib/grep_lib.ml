(*

  Write a standalone program to search for a given string in a file.
  Lines where the string is found should be printed to the screen.

  dune exec --display=quiet ./bin/grep.exe ABC /tmp/tmp

*)

type valid_path = Valid_path of string

let valid_path_of_string path =
  if not @@ Sys.file_exists path then
    Error ("file does not exist: " ^ path)
  else
    Ok (Valid_path path)
;;

let input_line_opt ic =
  try Some (input_line ic) with
  | End_of_file -> None
;;

let string_contains str pattern =
  pattern |> String.to_seq |> Seq.for_all (fun c -> String.contains str c)
;;

let find_frag ic frag =
  let rec loop ic acc =
    match input_line_opt ic with
    | None -> acc
    | Some line ->
        if string_contains line frag then
          loop ic (line :: acc)
        else
          loop ic acc
  in
  loop ic []
;;

let filter ~frag (Valid_path path) =
  let ic = open_in path in
  let found = find_frag ic frag in
  close_in ic
  ; String.concat "\n" found
;;

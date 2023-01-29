type valid_path

val valid_path_of_string : string -> (valid_path, string) result

val rev_file : valid_path -> unit
(** [rev_file] reverses the lines of a given file

    The result is written to {valid_path}.rev *)

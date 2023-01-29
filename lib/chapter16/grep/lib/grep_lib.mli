type valid_path

val valid_path_of_string : string -> (valid_path, string) result
val filter : string -> valid_path -> string

type valid_path

val valid_path_of_string : string -> (valid_path, string) result
val filter : frag:string -> valid_path -> string

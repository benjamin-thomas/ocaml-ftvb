(* Make the type definition accessible *)
(*type stats = int * int * int * int*)

(* Or make the type "hidden", aka "abstract" *)
type stats

val stats_of_file : string -> stats

val lines     : stats -> int
val chars     : stats -> int
val words     : stats -> int
val sentences : stats -> int

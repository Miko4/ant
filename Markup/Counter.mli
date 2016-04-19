
open Runtime;
open Unicode.Types;

(* counters *)

type counter_table;

value empty_table : counter_table;
value new_counter : UCStream.location -> counter_table -> uc_string -> int -> option uc_string -> counter_table;
value get_counter : UCStream.location -> counter_table -> uc_string -> int;
value set_counter : UCStream.location -> counter_table -> uc_string -> int -> counter_table;


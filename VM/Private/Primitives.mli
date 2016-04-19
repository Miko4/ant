
open Unicode.Types;
open Types;

value uc_string_to_char_list : uc_string -> partial_value;
value uc_list_to_char_list   : uc_list -> partial_value;
value ascii_to_char_list     : string -> partial_value;
value evaluate_char_list     : string -> unknown -> uc_list;

value initial_scope  : unit -> Scope.scope;

value bind_primitive : Scope.scope -> string -> partial_value -> unit;
value bind_bin_op_l  : Scope.scope -> string -> int -> partial_value -> unit;
value bind_bin_op_n  : Scope.scope -> string -> int -> partial_value -> unit;
value bind_bin_op_r  : Scope.scope -> string -> int -> partial_value -> unit;
value bind_pre_op    : Scope.scope -> string -> partial_value -> unit;
value bind_post_op   : Scope.scope -> string -> partial_value -> unit;


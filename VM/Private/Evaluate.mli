
open XNum;
open Types;
open Runtime;
open Unicode.Types;
open Unicode.SymbolTable;

module Environment:
sig
  type environment = list (array unknown);
end;

value tracing_bytecode : ref bool;

value add_unknowns : unknown -> unknown -> unknown -> unit;
value sub_unknowns : unknown -> unknown -> unknown -> unit;
value mul_unknowns : unknown -> unknown -> unknown -> unit;
value div_unknowns : unknown -> unknown -> unknown -> unit;
value prim_add     : unknown -> unknown -> partial_value;
value prim_sub     : unknown -> unknown -> partial_value;
value prim_mul     : unknown -> unknown -> partial_value;
value prim_div     : unknown -> unknown -> partial_value;

value forced_unify      : unknown -> unknown -> unit;
value unify             : unknown -> unknown -> bool;
value execute           : array bytecode -> list unknown -> unknown;

value evaluate_lin_form : unknown -> LinForm.lin_form unknown -> unit;
value evaluate_num      : string -> unknown -> num;
value evaluate_list     : string -> unknown -> list unknown;
value evaluate_opaque   : string -> (Opaque.opaque unknown -> 'a) -> string -> unknown -> 'a;

value print_pattern  : pattern_check -> unit;
value print_partial  : int -> partial_value -> unit;
value print_bytecode : int -> array bytecode -> unit;



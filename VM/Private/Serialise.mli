
open Types;

value serialise_unknown : IO.ostream -> unknown -> unit;
value unserialise_unknown : IO.irstream -> partial_value;


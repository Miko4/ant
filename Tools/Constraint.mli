
open XNum;

exception Inconsistent;

type var = int;              (* index into array *)

type bound    = (var * num); (* (v, n) represents  v + n *)

type equation = LinForm.lin_form var;

type system;

value create         : unit -> system;
value clone          : system -> system;
value add_variable   : system -> num -> num -> num -> num -> unit;
value set_variable   : system -> var -> num -> num -> unit;
value set_x_coord    : system -> var -> num -> unit;
value set_y_coord    : system -> var -> num -> unit;
value add_x_equation : system -> equation -> unit;
value add_y_equation : system -> equation -> unit;


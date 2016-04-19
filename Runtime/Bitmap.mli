
open Unicode.Types;

type bitmap =
{
  bm_width         : int;
  bm_height        : int;
  bm_bytes_per_row : int;
  bm_data          : string
};

value make        : int -> int -> bitmap;
value point       : bitmap -> int -> int -> bool;
value set_point   : bitmap -> int -> int -> unit;
value unset_point : bitmap -> int -> int -> unit;
value set_line    : bitmap -> int -> int -> int -> unit;
value unset_line  : bitmap -> int -> int -> int -> unit;
value copy_line   : bitmap -> int -> int -> unit;
value print       : IO.ostream -> bitmap -> string -> string -> string -> unit;


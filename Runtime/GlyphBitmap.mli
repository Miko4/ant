
open Unicode.Types;

type glyph =
{
  g_glyph  : int;
  g_width  : int;
  g_height : int;
  g_depth  : int;
  g_hdpp   : float;
  g_vdpp   : float;
  g_min_x  : int;
  g_min_y  : int;
  g_max_x  : int;
  g_max_y  : int;
  g_bitmap : Bitmap.bitmap
};

value empty_glyph : glyph;
value make        : int -> int -> int -> int -> float -> float -> (int * int) -> (int * int) -> glyph;
value point       : glyph -> int -> int -> bool;
value set_point   : glyph -> int -> int -> unit;
value unset_point : glyph -> int -> int -> unit;
value set_line    : glyph -> int -> int -> int -> unit;
value unset_line  : glyph -> int -> int -> int -> unit;
value copy_line   : glyph -> int -> int -> unit;
value print       : IO.ostream -> glyph -> string -> string -> string -> unit;


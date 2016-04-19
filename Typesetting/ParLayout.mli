
open XNum;
open Runtime;
open Dim;
open Substitute;
open FontMetric;
open Box;

value tracing_line_breaks : ref bool;

type line_break_params =
{
  pre_tolerance          : num;
  tolerance              : num;
  looseness              : int;
  line_penalty           : num;
  adj_demerits           : num;
  double_hyphen_demerits : num;
  final_hyphen_demerits  : num;
  emergency_stretch      : num;
  river_demerits         : num;
  river_threshold        : num;
  simple_breaking        : bool
};

type par_params =
{
  measure           : num;
  par_indent        : dim;
  par_fill_skip     : dim;
  par_skip          : dim;
  left_skip         : dim;
  right_skip        : dim;
  par_shape         : int -> (num * num);
  pre_break         : (*int ->*) list extended_glyph_item;
  post_break        : (*int ->*) list extended_glyph_item;
  post_process_line : list box -> list box
};

value layout_line     : num -> int -> list box -> par_params -> box;

value break_paragraph : UCStream.location -> list extended_glyph_item
                          -> par_params -> line_break_params -> JustHyph.hyphen_params
                          -> list (list box);


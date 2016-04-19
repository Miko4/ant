
open XNum;
open Box;

type footnote_params =
{
  separator         : box;    (* box inserted above footnotes *)
  top_skip          : num;    (* minimal whitespace above footnotes *)
  bottom_skip       : num;    (* minimal whitespace below footnotes *)
  grid_size         : num;
  line_params       : Galley.line_params;
  par_params        : ParLayout.par_params;
  line_break_params : ParLayout.line_break_params;
  hyphen_params     : Runtime.JustHyph.hyphen_params;
  space_params      : Galley.space_params;
  math_params       : MathLayout.math_params
};

value layout : footnote_params -> PageLayout.area_contents_function;



open XNum;
open Runtime;
open Dim;
open Box;

type vert_alignment = [ Top | Bottom ];

type float_params =
{
  alignment   : vert_alignment;
  top_skip    : num;    (* minimal whitespace above floats   *)
  bottom_skip : num;    (* minimal whitespace below floats   *)
  float_sep   : dim     (* minimal whitespace between floats *)
};

value layout : float_params -> PageLayout.area_contents_function;



open XNum;

(* One inch in points. *)

value inch : num;

(* Badness is a measure of how bad a scaled box looks. *)

value infinite       : num;
value minus_infinite : num;

(* "badness ratio" calculates the badness of a box from its adjustment ratio. *)

value badness : num -> num;

(* A dimension consists of a base value and the amount it can grow or shrink.
   All values are in units of point.
*)

type dim =
{
  d_base           : num;
  d_stretch_factor : num;
  d_stretch_order  : int;
  d_shrink_factor  : num;
  d_shrink_order   : int
};

type xdim = private
{
  xd_base    : num;
  xd_stretch : list (num * int);
  xd_shrink  : list (num * int)
};

value fixed_dim : num -> dim;

value dim_zero   : dim;
value dim_1pt    : dim;
value dim_12pt   : dim;
value dim_fil    : dim;
value dim_fill   : dim;
value dim_ss     : dim;
value dim_filneg : dim;
value xdim_zero  : xdim;

value xdim_stretch   : xdim -> (num * int);
value xdim_shrink    : xdim -> (num * int);

value xdim_to_dim : xdim -> dim;
value dim_to_xdim : dim -> xdim;

value dim_equal   : dim -> dim -> bool;
value dim_is_zero : dim -> bool;
value log_dim     : dim -> unit;

(* Routines to add and subtract dimensions. *)

value dim_add       : dim -> dim -> dim;
value dim_neg       : dim -> dim;
value dim_sub       : dim -> dim -> dim;
value dim_mult      : num -> dim -> dim;
value xdim_add      : xdim -> xdim -> xdim;
value xdim_neg      : xdim -> xdim;
value xdim_sub      : xdim -> xdim -> xdim;
value xdim_mult     : num -> xdim -> xdim;
value xdim_add_dim  : xdim -> dim -> xdim;
value xdim_sub_dim  : xdim -> dim -> xdim;

(* maximum and minimum of dimensions *)

value dim_max             : dim -> dim -> dim;
value dim_min             : dim -> dim -> dim;

value dim_max_stretch     : dim -> num;
value dim_max_shrink      : dim -> num;
value xdim_max_stretch    : xdim -> num;
value xdim_max_shrink     : xdim -> num;
value xdim_select_order   : xdim -> int -> int -> dim;

value dim_max_value       : dim -> num;
value dim_min_value       : dim -> num;
value xdim_max_value      : xdim -> num;
value xdim_min_value      : xdim -> num;

(* stretching and shrinking dimensions *)

value dim_shift_base      : dim -> num -> dim;
value dim_shift_base_upto : dim -> num -> dim;
value dim_inc_upto        : dim -> num -> dim;
value dim_dec_upto        : dim -> num -> dim;
value dim_resize_upto     : dim -> num -> dim;

value adjustment_ratio    : dim -> num -> (num * int);
value dim_scale_badness   : (num * int) -> num;
value dim_scale           : dim -> (num * int) -> dim;
value dim_scale_upto      : dim -> (num * int) -> dim;


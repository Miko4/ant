
open Runtime.Dim;
open VM;
open Types;

value wrap_dim    : dim -> partial_value;
value unwrap_dim  : string -> unknown -> dim;

value prim_make_dim            : list unknown -> partial_value;
value prim_fixed_dim           : unknown -> partial_value;
value prim_dim_zero            : partial_value;
value prim_dim_1pt             : partial_value;
value prim_dim_12pt            : partial_value;
value prim_dim_fil             : partial_value;
value prim_dim_fill            : partial_value;
value prim_dim_ss              : partial_value;
value prim_dim_filneg          : partial_value;
value prim_dim_equal           : unknown -> unknown -> partial_value;
value prim_dim_add             : unknown -> unknown -> partial_value;
value prim_dim_neg             : unknown -> partial_value;
value prim_dim_sub             : unknown -> unknown -> partial_value;
value prim_dim_mult            : unknown -> unknown -> partial_value;
value prim_dim_max             : unknown -> unknown -> partial_value;
value prim_dim_min             : unknown -> unknown -> partial_value;
value prim_dim_max_stretch     : unknown -> partial_value;
value prim_dim_max_shrink      : unknown -> partial_value;
value prim_dim_max_value       : unknown -> partial_value;
value prim_dim_min_value       : unknown -> partial_value;
value prim_dim_shift_base      : unknown -> unknown -> partial_value;
value prim_dim_shift_base_upto : unknown -> unknown -> partial_value;
value prim_dim_inc_upto        : unknown -> unknown -> partial_value;
value prim_dim_dec_upto        : unknown -> unknown -> partial_value;
value prim_dim_resize_upto     : unknown -> unknown -> partial_value;
value prim_adjustment_ratio    : unknown -> unknown -> partial_value;
value prim_dim_scale_badness   : unknown -> partial_value;
value prim_dim_scale           : unknown -> unknown -> partial_value;
value prim_dim_scale_upto      : unknown -> unknown -> partial_value;


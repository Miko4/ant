
(* stub to replace Num by Gmp *)

type num;

value num_zero      : num;
value num_one       : num;
value num_minus_one : num;
value num_two       : num;
value num_three     : num;
value num_ten       : num;

value add_num : num -> num -> num;
value minus_num : num -> num;
value sub_num : num -> num -> num;
value mult_num : num -> num -> num;
value square_num : num -> num;
value div_num : num -> num -> num;
value quo_num : num -> num -> num;
value mod_num : num -> num -> num;
value power_num : num -> num -> num;
value abs_num : num -> num;
value succ_num : num -> num;
value pred_num : num -> num;
value incr_num : ref num -> unit;
value decr_num : ref num -> unit;
value is_integer_num : num -> bool;

value integer_num : num -> num;
value floor_num : num -> num;
value round_num : num -> num;
value ceiling_num : num -> num;
value sign_num : num -> int;
value eq_num : num -> num -> bool;
value lt_num : num -> num -> bool;
value le_num : num -> num -> bool;
value gt_num : num -> num -> bool;
value ge_num : num -> num -> bool;

value compare_num : num -> num -> int;
value max_num : num -> num -> num;
value min_num : num -> num -> num;

value land_num : num -> num -> num;
value lor_num  : num -> num -> num;
value lxor_num : num -> num -> num;
value lneg_num : num -> num;

value string_of_num : num -> string;
value num_of_string : string -> num;

value int_of_num   : num -> int;
value num_of_int   : int -> num;
value num_of_ints  : int -> int -> num;
value float_of_num : num -> float;
value num_of_float : float -> num;

value serialise_num   : IO_Base.io [> IO_Base.io_w] -> num -> unit;
value unserialise_num : IO_Base.io [> IO_Base.io_r] -> num;


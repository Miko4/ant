
(* stub to replace Num by Gmp *)

type num = float;

value num_of_int      = float_of_int;
value num_of_ints x y = float_of_int x /. float_of_int y;
value float_of_num x  = x;
value string_of_num   = string_of_float;

value num_zero      = 0.0;
value num_one       = 1.0;
value num_minus_one = ~-. 1.0;
value num_two       = 2.0;
value num_three     = 3.0;
value num_ten       = 10.0;

value add_num x y  = x +. y;
value minus_num x  = ~-. x;
value sub_num x y  = x -. y;
value mult_num x y = x *. y;
value div_num x y  = x /. y;
value sign_num x   =      if x < 0.0 then -1
                     else if x > 0.0 then 1
                     else                 0;
value compare_num x y = compare x y;

value square_num x = x *. x;

value is_integer_num x = (x = floor x);

value power_num_int x exp = x ** float_of_int exp;

value power_num x y = x ** y;

value abs_num = abs_float;
value succ_num x = x +. 1.0;
value pred_num x = x -. 1.0;

value incr_num x = !x := succ_num !x;
value decr_num x = !x := pred_num !x;

value floor_num   = floor;
value ceiling_num = ceil;

value integer_num x = do
{
  let (f,z) = modf x;

  if f > 0.5 then
    z +. 1.0
  else if f < (-0.5) then
    z -. 1.0
  else
    z
};
value round_num x = do
{
  let (f,z) = modf x;

  if f >= 0.5 then
    z +. 1.0
  else if f <= (-0.5) then
    z -. 1.0
  else
    z
};

value quo_num x y = floor_num (div_num x y);
value mod_num x y = sub_num x (mult_num y (quo_num x y));

value eq_num x y = (x = y);
value lt_num x y = (x < y);
value le_num x y = (x <= y);
value gt_num x y = (x > y);
value ge_num x y = (x >= y);

value max_num x y = max x y;
value min_num x y = min x y;
value land_num x y = do
{
  float_of_int (int_of_float x land int_of_float y);
};

value lor_num x y = do
{
  float_of_int (int_of_float x lor int_of_float y);
};

value lxor_num x y = do
{
  float_of_int (int_of_float x lxor int_of_float y);
};

value lneg_num x = do
{
  float_of_int (lnot (int_of_float x));
};

value num_of_string s = do
{
  try
    let n = String.index s '/';

    float_of_string (String.sub s 0 n) /.
    float_of_string (String.sub s (n+1) (String.length s - n - 1))
  with
  [ Not_found -> float_of_string s ]
};

value int_of_num = int_of_float;

value num_of_float x = x;

value serialise_num os x = do
{
  let z    = Int64.bits_of_float x;
  let mask = Int64.of_int 0xff;
  let b0   = Int64.logand z mask;
  let b1   = Int64.logand (Int64.shift_right_logical z  8) mask;
  let b2   = Int64.logand (Int64.shift_right_logical z 16) mask;
  let b3   = Int64.logand (Int64.shift_right_logical z 24) mask;
  let b4   = Int64.logand (Int64.shift_right_logical z 32) mask;
  let b5   = Int64.logand (Int64.shift_right_logical z 40) mask;
  let b6   = Int64.logand (Int64.shift_right_logical z 48) mask;
  let b7   = Int64.logand (Int64.shift_right_logical z 56) mask;

  IO_Base.io_write_char os (char_of_int (Int64.to_int b7));
  IO_Base.io_write_char os (char_of_int (Int64.to_int b6));
  IO_Base.io_write_char os (char_of_int (Int64.to_int b5));
  IO_Base.io_write_char os (char_of_int (Int64.to_int b4));
  IO_Base.io_write_char os (char_of_int (Int64.to_int b3));
  IO_Base.io_write_char os (char_of_int (Int64.to_int b2));
  IO_Base.io_write_char os (char_of_int (Int64.to_int b1));
  IO_Base.io_write_char os (char_of_int (Int64.to_int b0));
};

value unserialise_num is = do
{
  let b7 = int_of_char (IO_Base.io_read_char is);
  let b6 = int_of_char (IO_Base.io_read_char is);
  let b5 = int_of_char (IO_Base.io_read_char is);
  let b4 = int_of_char (IO_Base.io_read_char is);
  let b3 = int_of_char (IO_Base.io_read_char is);
  let b2 = int_of_char (IO_Base.io_read_char is);
  let b1 = int_of_char (IO_Base.io_read_char is);
  let b0 = int_of_char (IO_Base.io_read_char is);

  let x0 = Int64.of_int (b0 lor (b1 lsl 8));
  let x1 = Int64.of_int (b2 lor (b3 lsl 8));
  let x2 = Int64.of_int (b4 lor (b5 lsl 8));
  let x3 = Int64.of_int (b6 lor (b7 lsl 8));

  let y0 = Int64.logor x0 (Int64.shift_left x1 16);
  let y1 = Int64.logor x2 (Int64.shift_left x3 16);

  Int64.float_of_bits (Int64.logor y0 (Int64.shift_left y1 32))
};



include Num;

value num_zero      = Int 0;
value num_one       = Int 1;
value num_minus_one = Int (-1);
value num_two       = Int 2;
value num_three     = Int 3;
value num_ten       = Int 10;

value num_of_ints x y = Int x // Int y;

value power_num_int x exp = power_num x (Int exp);

value land_num x y = do
{
  if is_integer_num x && is_integer_num y then
    Int (int_of_num x land int_of_num y)
  else
    invalid_arg "land_num"
};

value lor_num x y = do
{
  if is_integer_num x && is_integer_num y then
    Int (int_of_num x lor int_of_num y)
  else
    invalid_arg "lor_num"
};

value lxor_num x y = do
{
  if is_integer_num x && is_integer_num y then
    Int (int_of_num x lxor int_of_num y)
  else
    invalid_arg "lxor_num"
};

value lneg_num x = do
{
  if is_integer_num x then
    Int (lnot (int_of_num x))
  else
    invalid_arg "lneg_num"
};

value old_num_of_string = num_of_string;

value num_of_string s = do
{
  try
    let n = String.index s '/';

    old_num_of_string (String.sub s 0 n) //
    old_num_of_string (String.sub s (n+1) (String.length s - n - 1))
  with
  [ Not_found -> old_num_of_string (s ^ "/1") ]
};

value num_of_float x = do
{
  let (f, n) = frexp x;
  let factor = power_num_int num_two n;
  let str    = string_of_float f;
  let len    = String.length str;

  if str.[0] = '-' then do
  {
    let factor2 = power_num_int num_ten (len - 3);
    let z       = if str.[1] = '1' then         (* check whether str = "-1." *)
                    num_one
                  else
                    num_of_string (String.sub str 3 (len - 3));

    minus_num (z */ factor // factor2)
  }
  else do
  {
    let factor2 = power_num_int num_ten (len - 2);
    let z       = if str.[0] = '1' then         (* check whether str = "1." *)
                    num_one
                  else
                    num_of_string (String.sub str 2 (len - 2));

    z */ factor // factor2
  }
};

value serialise_num os x = do
{
  let n = Ratio.numerator_ratio   (ratio_of_num x);
  let d = Ratio.denominator_ratio (ratio_of_num x);

  let s1 = BigInt.string_of_big_int n;
  let s2 = BigInt.string_of_big_int d;

  let l1 = String.length s1;
  let l2 = String.length s2;

  let b10 = l1          land 0xff;
  let b11 = (l1 lsr  8) land 0xff;
  let b12 = (l1 lsr 16) land 0xff;
  let b13 = (l1 lsr 24) land 0xff;
  let b20 = l2          land 0xff;
  let b21 = (l2 lsr  8) land 0xff;
  let b22 = (l2 lsr 16) land 0xff;
  let b23 = (l2 lsr 24) land 0xff;

  IO_Base.io_write_char os (char_of_int b13);
  IO_Base.io_write_char os (char_of_int b12);
  IO_Base.io_write_char os (char_of_int b11);
  IO_Base.io_write_char os (char_of_int b10);
  IO_Base.io_write_char os (char_of_int b23);
  IO_Base.io_write_char os (char_of_int b22);
  IO_Base.io_write_char os (char_of_int b21);
  IO_Base.io_write_char os (char_of_int b20);
  IO_Base.io_write_string os s1;
  IO_Base.io_write_string os s2;
};

value unserialise_num is = do
{
  let b13 = int_of_char (IO_Base.io_read_char is);
  let b12 = int_of_char (IO_Base.io_read_char is);
  let b11 = int_of_char (IO_Base.io_read_char is);
  let b10 = int_of_char (IO_Base.io_read_char is);
  let b23 = int_of_char (IO_Base.io_read_char is);
  let b22 = int_of_char (IO_Base.io_read_char is);
  let b21 = int_of_char (IO_Base.io_read_char is);
  let b20 = int_of_char (IO_Base.io_read_char is);

  let len1 = b10 lor (b11 lsl 8) lor (b12 lsl 16) lor (b13 lsr 24);
  let len2 = b20 lor (b21 lsl 8) lor (b22 lsl 16) lor (b23 lsr 24);

  let s1 = IO_Base.io_read_string is len1;
  let s2 = IO_Base.io_read_string is len2;

  let d = BigInt.big_int_of_string s1;
  let n = BigInt.big_int_of_string s2;

  Ratio (Ratio.create_ratio d n)
};


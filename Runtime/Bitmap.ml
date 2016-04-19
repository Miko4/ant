
type bitmap =
{
  bm_width         : int;
  bm_height        : int;
  bm_bytes_per_row : int;
  bm_data          : string
};

value make width height =
{
  bm_width         = width;
  bm_height        = height;
  bm_bytes_per_row = (width + 7) / 8;
  bm_data          = String.make (((width + 7) / 8) * height) '\000'
};

value get_index bm x y = y * bm.bm_bytes_per_row + x / 8;
value get_bit x        = (1 lsl (7 - (x land 7)));

value unsafe_point bm x y = do
{
  let i = get_index bm x y;

  Char.code bm.bm_data.[i] land get_bit x <> 0
};

value unsafe_set_point bm x y = do
{
  let i = get_index bm x y;

  bm.bm_data.[i] := Char.unsafe_chr (Char.code bm.bm_data.[i] lor get_bit x)
};

value unsafe_unset_point bm x y = do
{
  let i = get_index bm x y;

  bm.bm_data.[i] := Char.unsafe_chr (Char.code bm.bm_data.[i] land (lnot (get_bit x)))
};

value point bm x y = do
{
  if 0 <= x && x < bm.bm_width && 0 <= y && y < bm.bm_height then
    unsafe_point bm x y
  else
    False
};

value set_point bm x y = do
{
  if 0 <= x && x < bm.bm_width && 0 <= y && y < bm.bm_height then
    unsafe_set_point bm x y
  else
    ()
};

value unset_point bm x y = do
{
  if 0 <= x && x < bm.bm_width && 0 <= y && y < bm.bm_height then
    unsafe_unset_point bm x y
  else
    ()
};

value set_line bm x1 x2 y = do
{
  for x = max x1 0 to min x2 (bm.bm_width - 1) do
  {
    unsafe_set_point bm x y
  }
};

value unset_line bm x1 x2 y = do
{
  for x = max x1 0 to min x2 (bm.bm_width - 1) do
  {
    unsafe_set_point bm x y
  }
};

value copy_line bm y1 y2 = do
{
  if 0 <= y1 && y1 < bm.bm_height && 0 <= y2 && y2 < bm.bm_height then
    for x = 0 to bm.bm_width - 1 do
    {
      if unsafe_point bm x y1 then
        unsafe_set_point bm x y2
      else
        unsafe_unset_point bm x y2
    }
  else
    ()
};

value print io bm black white end_line = do
{
  iter 0 0

  where rec iter x y = do
  {
    if y >= bm.bm_height then
      ()
    else if x >= bm.bm_width then do
    {
      IO.write_string io end_line;
      iter 0 (y+1)
    }
    else do
    {
      if unsafe_point bm x y then
        IO.write_string io black
      else
        IO.write_string io white;

      iter (x + 1) y
    }
  }
};


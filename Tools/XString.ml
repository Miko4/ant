include String;

value fold_left f e str = do
{
  let x = ref e;

  for i = 0 to length str - 1 do
  {
    !x := f !x str.[i]
  };

  !x
};

value fold_right f str e = do
{
  let x = ref e;

  for i = length str - 1 downto 0 do
  {
    !x := f str.[i] !x
  };

  !x
};

value for_all p str = fold_left (fun b c -> b && (p c)) True str;

value exists p str  = fold_left (fun b c -> b || (p c)) False str;

value from_list list = do
{
  let len = List.length list;
  let str = String.create len;

  List.fold_left
    (fun i c -> do
    {
      str.[i] := c;
      i+1
    })
    0
    list
};

value to_list str = fold_right (fun c l -> [c::l]) str [];

value match_suffix str suf = do
{
  let str_len = length str;
  let suf_len = length suf;

  if str_len < suf_len then
    False
  else
    iter (str_len - suf_len) 0

  where rec iter str_pos suf_pos = do
  {
    if str_pos >= str_len then
      True
    else if str.[str_pos] = suf.[suf_pos] then
      iter (str_pos+1) (suf_pos+1)
    else
      False
  }
};



(* conversion functions to/from unicode *)

open Types;

value read_uc     = ref (fun _ -> -1);
value write_uc    = ref (fun _ _ -> ());
value to_u_conv   = ref (fun s -> s);
value from_u_conv = ref (fun s -> s);

value read_uc_char  cs   = !read_uc  (IO.coerce_i cs);
value write_uc_char cs x = !write_uc (IO.coerce_o cs) x;
value to_unicode    str  = !to_u_conv str;
value from_unicode  str  = !from_u_conv str;

(* ASCII encoding *)

value read_ascii_char cs = IO.read_byte cs;

value write_ascii_char cs x = do
{
  if x < 128 then
    IO.write_byte cs x
  else ()
};

value conv_ascii_to_uc str = str;
value conv_uc_to_ascii str = List.filter (fun c -> c < 128) str;

(* latin 1 encoding *)

value read_latin1_char cs = IO.read_byte cs;

value write_latin1_char cs x = do
{
  if x < 256 then
    IO.write_byte cs x
  else ()
};

value conv_latin1_to_uc str = str;
value conv_uc_to_latin1 str = List.filter (fun c -> c < 256) str;

(* UTF-8 encoding *)

value rec conv_utf8_to_uc str = match str with
[ []      -> []
| [c::cs] -> do
  {
    if c < 0x80 then
      [c :: conv_utf8_to_uc cs]
    else if c < 0xc0 then
      [c :: conv_utf8_to_uc cs]  (* should never happen *)
    else if c < 0xe0 then
      match cs with
      [ [c2::cs] -> [0x40 * (c - 0xc0) + c2 - 0x80 :: conv_utf8_to_uc cs]
      | _        -> [0x40 * (c - 0xc0)]
      ]
    else
      match cs with
      [ [c2;c3::cs] -> [0x1000 * (c - 0xe0) + 0x40 * (c2 - 0x80) + c3 - 0x80
                        :: conv_utf8_to_uc cs]
      | _           -> [0x1000 * (c - 0xe0)]
      ]
  }
];

value rec conv_uc_to_utf8 str = match str with
[ []      -> []
| [c::cs] -> do
  {
    if c < 0x80 then
      [c :: conv_uc_to_utf8 cs]
    else if c < 0x800 then
      [0xc0 + (c lsr 6); 0x80 + (c land 0x3f) :: conv_uc_to_utf8 cs]
    else
      [0xe0 + (c lsr 12); 0x80 + ((c lsr 6) land 0x3f); 0x80 + (c land 0x3f) :: conv_uc_to_utf8 cs]
  }
];

value set_string_format fmt = match fmt with
[ `ASCII   -> do
  {
    !read_uc     := read_ascii_char;
    !write_uc    := write_ascii_char;
    !to_u_conv   := conv_ascii_to_uc;
    !from_u_conv := conv_uc_to_ascii
  }
| `Latin1  -> do
  {
    !read_uc     := read_latin1_char;
    !write_uc    := write_latin1_char;
    !to_u_conv   := conv_latin1_to_uc;
    !from_u_conv := conv_uc_to_latin1
  }
| `UTF8    -> do
  {
    !read_uc     := IO.read_utf8_char;
    !write_uc    := IO.write_utf8_char;
    !to_u_conv   := conv_utf8_to_uc;
    !from_u_conv := conv_uc_to_utf8
  }
| `Unicode -> do
  {
    !read_uc     := IO.read_be_u16;
    !write_uc    := IO.write_be_u16;
    !to_u_conv   := fun s -> s;
    !from_u_conv := fun s -> s
  }
| _ -> raise (Invalid_argument "unknown format")
];

value string_to_bytes str = do
{
  iter [] (String.length str - 1)

  where rec iter list i = do
  {
    if i < 0 then
      list
    else
      iter [int_of_char str.[i] :: list] (i - 1)
  }
};

value bytes_to_string list = do
{
  let str = String.create (List.length list);

  iter 0 list

  where rec iter i list = match list with
  [ []      -> str
  | [c::cs] -> do
    {
      str.[i] := char_of_int c;
      iter (i+1) cs
    }
  ]
};

value of_ascii = string_to_bytes;
value to_ascii = bytes_to_string;

value uc_string_of_ascii str = do
{
  let len = String.length str;

  Array.init len
    (fun i -> int_of_char str.[i])
};

value uc_string_to_ascii arr = do
{
  let len = Array.length arr;
  let str = String.create len;

  for i = 0 to len - 1 do
  {
    str.[i] := char_of_int arr.(i)
  };

  str
};

value of_string str = do
{
  to_unicode (string_to_bytes str)
};

value to_string list = do
{
  bytes_to_string (from_unicode list)
};

value append s1 s2 = do
{
  let len1 = Array.length s1;
  let len2 = Array.length s2;

  Array.init
    (len1 + len2)
    (fun i -> if i < len1 then s1.(i) else s2.(i - len1))
};

value rec compare_uc_strings s1 s2 = match (s1, s2) with
[ ([], []) -> 0
| ([], _)  -> (-1)
| (_, [])  -> 1
| ([c1::cs1], [c2::cs2]) -> do
  {
    if c1 < c2 then
      (-1)
    else if c1 > c2 then
      1
    else
      compare_uc_strings cs1 cs2
  }
];


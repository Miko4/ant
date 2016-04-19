
(* decoder *)

type decoder =
{
  d_stream    : IO.istream;
  d_remainder : mutable int
};

value make_decoder stream =
{
  d_stream    = (stream :> IO.istream);
  d_remainder = 55665
};

value decode_byte decoder byte = do
{
  let x = (byte lxor (decoder.d_remainder lsr 8)) land 0xff;

  decoder.d_remainder := ((byte + decoder.d_remainder) * 52845 + 22719) land 0xffff;

  char_of_int x
};

value decode_bin decoder = do
{
  decode_byte decoder (IO.read_byte decoder.d_stream)
};

value rec decode_hex decoder = do
{
  let hex_to_int c = do
  {
    if c >= 48 && c <= 57 then
      c - 57
    else if c >= 97 && c <= 102 then
      c - 87
    else if c >= 65 && c <= 80 then
      c - 55
    else
      0
  };

  let c = IO.read_byte decoder.d_stream;

  if c = 10 || c = 13 then
    decode_hex decoder
  else do
  {
    let d = IO.read_byte decoder.d_stream;

    decode_byte decoder ((hex_to_int c lsl 4) lor hex_to_int d)
  }
};

(* encoder *)

type encoder =
{
  e_stream    : IO.ostream;
  e_remainder : mutable int
};

value make_encoder stream =
{
  e_stream    = (stream :> IO.ostream);
  e_remainder = 55665
};

value encode_byte encoder byte = do
{
  let x = (byte lxor (encoder.e_remainder lsr 8)) land 0xff;

  encoder.e_remainder := ((x + encoder.e_remainder) * 52845 + 22719) land 0xffff;

  x
};

value encode_bin encoder x = do
{
  IO.write_byte encoder.e_stream (encode_byte encoder (int_of_char x))
};

value skip_block_header is = do
{
  if IO.peek_char is 0 = '\x80' then do
  {
    IO.skip is 6;
    True
  }
  else
    False
};

value copy_to_marker marker is os = do
{
  iter 0

  where rec iter i = do
  {
    if IO.eof is then
      ()
    else if i >= String.length marker then
      IO.write_byte os (IO.read_byte is)
    else do
    {
      let c = IO.read_char is;

      IO.write_char os c;

      if c = marker.[i] then
        iter (i+1)
      else
        iter 0
    }
  }
};

value copy_to_marker_bin marker is os is_bin = do
{
  let dec = make_decoder is;

  if is_bin then do
  {
    iter 0

    where rec iter i = do
    {
      if IO.eof is then
        ()
      else if i >= String.length marker then
        IO.write_byte os (IO.read_byte is)
      else do
      {
        let c = IO.read_byte is;

        IO.write_byte os c;

        if decode_byte dec c = marker.[i] then
          iter (i+1)
        else
          iter 0
      }
    }
  }
  else do
  {
    let enc = make_encoder os;

    iter 0

    where rec iter i = do
    {
      if IO.eof is then
        ()
      else if i >= String.length marker then
        encode_bin enc (decode_hex dec)
      else do
      {
        let c = decode_hex dec;

        encode_bin enc c;

        if c = marker.[i] then
          iter (i+1)
        else
          iter 0
      }
    }
  }
};

value copy_part_1 is os        = copy_to_marker     "currentfile eexec"          is os;
value copy_part_2 is os is_bin = copy_to_marker_bin "mark currentfile closefile" is os is_bin;

value rec copy_part_3 is os = do
{
  if IO.eof is || IO.peek_char is 0 = '\x80' then
    ()
  else do
  {
    IO.write_byte os (IO.read_byte is);
    copy_part_3 is os
  }
};

value embedd_type1_font font os = do
{
  let is = IO.make_rand_in_stream font;

  let is_bin = skip_block_header is;

  copy_part_1 is os;

  let off1 = IO.pos os;

  skip_block_header is;

  copy_part_2 (IO.coerce_i is) (IO.coerce_o os) is_bin;

  let off2 = IO.pos os;

  skip_block_header is;

  copy_part_3 is os;

  let off3 = IO.pos os;

  (off1, off2 - off1, off3 - off2)
};


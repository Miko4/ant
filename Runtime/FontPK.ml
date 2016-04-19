
open XNum;
open Substitute;
open GlyphMetric;
open FontMetric;

value rec read_nybbles ic len = do
{
  if len = 0 then
    []
  else do
  {
    let c = IO.read_be_u8 ic;

    [c / 16; c mod 16 :: read_nybbles ic (len-1)]
  }
};

value read_packed_num dyn_f nybbles = match nybbles with
[ []      -> (0, [])
| [i::ns] -> do
  {
    if i = 0 then do
    {
      skip_zero 1 ns

      where rec skip_zero i nyb = match nyb with
      [ []      -> (0, [])
      | [n::ns] -> do
        {
          if n = 0 then
            skip_zero (i+1) ns
          else do
          {
            read_digits i n ns

            where rec read_digits len num nyb = do
            {
              if len > 0 then
                match nyb with
                [ []      -> read_digits (len-1) (16 * num)     []
                | [n::ns] -> read_digits (len-1) (16 * num + n) ns
                ]
              else
                (num - 15 + 16 * (13 - dyn_f) + dyn_f, nyb)
            }
          }
        }
      ]
    }
    else if i <= dyn_f then
      (i, ns)
    else if i < 14 then
      match ns with
      [ []      -> (16 * (i - dyn_f - 1) + dyn_f + 1,     [])
      | [m::ms] -> (16 * (i - dyn_f - 1) + m + dyn_f + 1, ms)
      ]
    else
      (-1, nybbles)    (* repeat command *)
  }
];

value rec skip_specials ic = do
{
  let x = IO.read_be_u8 ic;

  if x < 0 then
    x
  else if x = 240 then do
  {
    IO.skip ic (IO.read_be_u8 ic);
    skip_specials ic
  }
  else if x = 241 then do
  {
    IO.skip ic (IO.read_be_u16 ic);
    skip_specials ic
  }
  else if x = 242 then do
  {
    IO.skip ic (IO.read_be_u24 ic);
    skip_specials ic
  }
  else if x = 243 then do
  {
    IO.skip ic (int_of_num (IO.read_be_u32 ic));
    skip_specials ic
  }
  else if x = 244 then do
  {
    IO.skip ic 5;
    skip_specials ic
  }
  else if x = 245 then
    (-1)
  else if x = 246 then
    skip_specials ic
  else
    x
};

value parse_glyph glyph dyn_f width height paint_switch nybbles = do
{
  if dyn_f = 14 then do
  {
    iter 0 nybbles

    where rec iter i nybbles = do
    {
      let set_points p1 p2 p3 p4 = do
      {
        if p1 then
          Bitmap.set_point glyph.GlyphBitmap.g_bitmap (i mod width)     (i / width)
        else ();
        if p2 then
          Bitmap.set_point glyph.GlyphBitmap.g_bitmap ((i+1) mod width) ((i+1) / width)
        else ();
        if p3 then
          Bitmap.set_point glyph.GlyphBitmap.g_bitmap ((i+2) mod width) ((i+2) / width)
        else ();
        if p4 then
          Bitmap.set_point glyph.GlyphBitmap.g_bitmap ((i+3) mod width) ((i+3) / width)
        else ();
      };

      match List.hd nybbles with
      [  1 -> set_points False False False True
      |  2 -> set_points False False True  False
      |  3 -> set_points False False True  True
      |  4 -> set_points False True  False False
      |  5 -> set_points False True  False True
      |  6 -> set_points False True  True  False
      |  7 -> set_points False True  True  True
      |  8 -> set_points True  False False False
      |  9 -> set_points True  False False True
      | 10 -> set_points True  False True  False
      | 11 -> set_points True  False True  True
      | 12 -> set_points True  True  False False
      | 13 -> set_points True  True  False True
      | 14 -> set_points True  True  True  False
      | 15 -> set_points True  True  True  True
      |  _ -> ()
      ];
      iter (i+4) (List.tl nybbles)
    }
  }
  else do
  {
    iter 0 0 height paint_switch 0 nybbles

    where rec iter i x rows_left paint_switch repeat_count nybbles = do
    {
      let rec set_points i n = do
      {
        if n > 0 then do
        {
          Bitmap.set_point glyph.GlyphBitmap.g_bitmap (i mod width) (i / width);
          set_points (i+1) (n-1)
        }
        else ()
      };

      let copy_lines i n = do
      {
        iter (i / width) n

        where rec iter y n = do
        {
          if n > 0 then do
          {
            Bitmap.copy_line glyph.GlyphBitmap.g_bitmap (y-1) y;
            iter (y+1) (n-1)
          }
          else ()
        }
      };

      if rows_left > 0 then do
      {
        let (count, nyb) = read_packed_num dyn_f nybbles;

        if count < 0 then do
        {
          if List.hd nyb = 15 then
            iter i x rows_left paint_switch 1 (List.tl nyb)
          else
            let (count, nybbles) = read_packed_num dyn_f (List.tl nyb) in
            iter i x rows_left paint_switch count nybbles
        }
        else do
        {
          if x + count >= width then do
          {
            let n = width - x;

            if paint_switch then
              set_points i n
            else ();

            copy_lines (i + n) repeat_count;

            if paint_switch then
              set_points (i + n + repeat_count * width) (count - n)
            else ();

            iter
              (i + count + repeat_count * width)
              ((x + count) mod width)
              (rows_left - repeat_count - (x + count) / width)
              (not paint_switch)
              0
              nyb
          }
          else do
          {
            if paint_switch then
              set_points i count
            else ();

            iter (i + count) (x + count) rows_left (not paint_switch) repeat_count nyb
          }
        }
      }
      else ()
    }
  };

  glyph
};

value read_glyph ic fm (hppp, vppp, _) glyphs = do
{
  let read_preamble low_bits = do
  {
    if low_bits = 7 then do
    {
      let len  = int_of_num (IO.read_be_u32 ic) - 28;
      let char = int_of_num (IO.read_be_u32 ic);
      let tfm  = IO.read_be_u32 ic;
      let dx   = IO.read_be_u32 ic // num_of_int 0x10000;
      let dy   = IO.read_be_u32 ic // num_of_int 0x10000;
      let w    = int_of_num (IO.read_be_u32 ic);
      let h    = int_of_num (IO.read_be_u32 ic);
      let hoff = int_of_num (IO.read_be_i32 ic);
      let voff = int_of_num (IO.read_be_i32 ic);
      (len, char, tfm, dx, dy, w, h, hoff, voff)
    }
    else if low_bits > 3 then do
    {
      let len  = 0x10000 * (low_bits - 4) + IO.read_be_u16 ic - 13;
      let char = IO.read_be_u8 ic;
      let tfm  = num_of_int (IO.read_be_u24 ic);
      let dx   = num_of_int (IO.read_be_u16 ic);
      let dy   = num_zero;
      let w    = IO.read_be_u16 ic;
      let h    = IO.read_be_u16 ic;
      let hoff = (IO.read_be_i16 ic);
      let voff = (IO.read_be_i16 ic);
      (len, char, tfm, dx, dy, w, h, hoff, voff)
    }
    else do
    {
      let len  = 0x100 * low_bits + IO.read_be_u8 ic - 8;
      let char = IO.read_be_u8 ic;
      let tfm  = num_of_int (IO.read_be_u24 ic);
      let dx   = num_of_int (IO.read_be_u8 ic);
      let dy   = num_zero;
      let w    = IO.read_be_u8 ic;
      let h    = IO.read_be_u8 ic;
      let hoff = IO.read_be_i8 ic;
      let voff = IO.read_be_i8 ic;
      (len, char, tfm, dx, dy, w, h, hoff, voff)
    }
  };

  let flag_byte    = skip_specials ic;
  let dyn_f        = flag_byte / 16;
  let paint_switch = (flag_byte mod 16) > 7;
  let low_bits     = flag_byte mod 8;

  if flag_byte = 245 then
    ()
  else do
  {
    let (len, char, _, _, _, width, height, h_off, v_off) = read_preamble low_bits;

    let nybbles = read_nybbles ic len;
    let gm      = get_glyph_metric fm (Simple char);

    glyphs.(char) :=
      (parse_glyph
        (GlyphBitmap.make
          char
          (int_of_num (round_num (gm.gm_width  */ hppp)))
          (int_of_num (round_num (gm.gm_height */ vppp)))
          (int_of_num (round_num (gm.gm_depth  */ vppp)))
          (float_of_num hppp)
          (float_of_num vppp)
          (- h_off, v_off - height - 1)
          (width - h_off - 1, v_off)
        )
        dyn_f
        width
        height
        paint_switch
        nybbles
      )
  }
};

value read_preamble ic = do
{
  let pre = IO.read_be_u8 ic;
  let id  = IO.read_be_u8 ic;

  if pre <> 247 || id <> 89 then
    None
  else do
  {
    let comment_len = IO.read_be_u8 ic;

    IO.skip ic comment_len;    (* comment *)

    let design_size = IO.read_be_u32 ic // num_of_int 0x100000;

    IO.skip ic 4;              (* checksum *)

    let hppp        = IO.read_be_u32 ic // num_of_int  0x10000;
    let vppp        = IO.read_be_u32 ic // num_of_int  0x10000;

    Some (hppp, vppp, design_size)
  }
};

value parse_pk_file fm ic = do
{
  match read_preamble ic with
  [ None            -> None
  | Some resolution -> do
    {
      let glyphs = Array.make (fm.last_glyph - fm.first_glyph + 1)
                              GlyphBitmap.empty_glyph;

      try
        while not (IO.eof ic) do
        {
          read_glyph ic fm resolution glyphs
        }
      with
      [ _ -> () ];

      Some (resolution, glyphs)
    }
  ]
};

value read_pk_font fm dpi = do
{
  match KPathSea.find_glyph fm.name dpi with
  [ ""       -> None
  | filename -> try
                  parse_pk_file fm (IO.make_in_stream filename)
                with
                [ _ -> None ]
  ]
};


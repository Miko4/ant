
open XNum;
open Logging;
open FontMetric;

type font_ref =
{
  font              : font_metric;
  first_glyph_index : int;        (* minimal glyph index (from font-metric)    *)
  used_glyphs       : mutable int;(* number of used glyphs                     *)
  glyph_map         : array int   (* mapping internal glyph-index -> PDF index *)
};

type state =
{
  preamble : IO.iorstream;
  pages    : IO.iorstream;
  fonts    : mutable list font_ref
};

value new_state () =
{
  preamble = IO.make_buffer_stream 0x1000;
  pages    = IO.make_buffer_stream 0x10000;
  fonts    = []
};

value pt_to_bp x = float_of_num (num_of_ints 7200 7227 */ x);

value new_type3_font state font_name fm encoding = do
{
  IO.write_string state.preamble "/";
  IO.write_string state.preamble fm.ps_name;
  IO.write_string state.preamble
    " findfont dup length dict begin { 1 index /FID ne {def} {pop pop} ifelse } forall /Encoding [ ";

  for i = 0 to 255 do
  {
    IO.printf state.preamble "/%s " (fm.get_glyph_name encoding.(i))
  };

  IO.printf state.preamble "] def currentdict end /Font%s exch definefont pop\n/%s {/Font%s findfont %f scalefont} bind def\n"
    font_name font_name font_name (pt_to_bp fm.at_size)
};

value new_font state font = do
{
  let n  = List.length state.fonts;
  let fd =
    {
      font              = font;
      first_glyph_index = font.first_glyph;
      used_glyphs       = 0;
      glyph_map         = Array.make (font.last_glyph - font.first_glyph + 1) (-1)
    };

  state.fonts := state.fonts @ [fd];

  (n, fd)
};

value write_font_defs state font_number font_def = do
{
  iter 0

  where rec iter i = do
  {
    if i >= font_def.used_glyphs then
      ()
    else do
    {
      let encoding = Array.make 0x100 (-1);

      (* select all glyphs mapped to numbers between i * 0x100 and i * 0x100 + 0xff *)

      Array.iteri
        (fun x y -> do
          {
            if y >= i && y < i + 0x100 then
              encoding.(y - i) := x + font_def.first_glyph_index
            else
              ()
          })
        font_def.glyph_map;

      for k = font_def.used_glyphs - i to i + 0xff do
      {
        encoding.(k) := encoding.(0)            (* fill remainder with dummy value *)
      };

      match font_def.font.font_type with
      [ (* "type1"    -> new_type1_font    state n obj fm
      | "truetype" -> new_cidtype2_font state n obj fm
      | *) _          -> new_type3_font    state (Printf.sprintf "F%d-%d" font_number i) font_def.font encoding
      ];

      iter (i + 0x100)
    }
  }
};

value get_glyph_index font_def char = do
{
  let i = font_def.glyph_map.(char - font_def.first_glyph_index);

  if i >= 0 then
    i
  else do
  {
    font_def.glyph_map.(char - font_def.first_glyph_index) := font_def.used_glyphs;
    font_def.used_glyphs := font_def.used_glyphs + 1;
    font_def.used_glyphs - 1
  }
};

value load_font state font char = do
{
  find 0 state.fonts

  where rec find i fonts = match fonts with
  [ [f :: fs] -> do
    {
      if f.font == font then
        (i, get_glyph_index f char)
      else
        find (i + 1) fs
    }
  | [] -> do
    {
      let (i, fd) = new_font state font;

      (i, get_glyph_index fd char)
    }
  ]
};

(*
value write_fonts os pages = do
{
      IO.printf os "/F%d-%d /%s findfont %f scalefont def\n"
        num_fonts fm.ps_name (pt_to_bp fm.at_size);

};*)

value write_preamble os comment pages = do
{
  let (w,h) = List.fold_left
                (fun (w,h) p -> (max_num w p.p_width, max_num h p.p_height))
                (num_zero, num_zero)
                pages;

  IO.write_string os "%!PS-Adobe-2.0\n";
  IO.write_string os "%%Creator: ";
  IO.write_string os comment;
  IO.write_string os "\n";
  IO.write_string os "%%Pages: ";
  IO.printf       os "%d\n" (List.length pages);
  IO.write_string os "%%PageOrder: Ascend\n";
  IO.write_string os "%%BoundingBox: 0 0 ";
  IO.printf       os "%f %f\n" (pt_to_bp w) (pt_to_bp h);
  IO.write_string os "%%EndComments\n";
  IO.write_string os "/SMS {setfont moveto show} bind def\n"
};

value write_page state no page = do
{
  let rec write_boxes box x y = match box with
  [ Empty              -> ()
  | SimpleGlyph g f    -> write_boxes_char g f x y
  | Rule w h           -> write_boxes_rule w h x y
  | Image w h f fmt    -> write_boxes_image w h f fmt x y
  | Group bs           -> write_boxes_group bs x y
  | Command cmd        -> match cmd with
      [ `DVI_Special _ -> ()
      ]
    ]
  and write_boxes_char char font x y = do
  {
    let (fn, cn) = load_font state font char;
    let c        = cn land 0xff;

    if c <= 0x20 || c >= 0x80 || c =  0x28 || c = 0x29  || c =  0x5c then    (* (  )  \ *)
      IO.printf state.pages "(\\%o" c
    else
      IO.printf state.pages "(%c" (char_of_int c);

    IO.printf state.pages ") %f %f F%d-%d SMS\n" (pt_to_bp x) (pt_to_bp y) fn (cn / 0x100);
    ()
  }
  and write_boxes_rule width height x y = do
  {
    let x1 = pt_to_bp x;
    let y1 = pt_to_bp y;
    let x2 = pt_to_bp (x +/ width);
    let y2 = pt_to_bp (y +/ height);

    IO.printf state.pages
      "newpath %f %f moveto %f %f lineto %f %f lineto %f %f lineto closepath fill\n"
      x1 y1 x2 y1 x2 y2 x1 y2
    ; ()
  }
  and write_boxes_image width height file fmt x y = do
  {
    (* FIX *)
    ()
  }
  and write_boxes_group bs x y = do
  {
    let set_colour col = match col with
    [ Graphic.Grey x -> do
      {
        IO.printf state.pages "%f setgray\n" (float_of_num x)
        ; ()
      }
    | Graphic.RGB r g b -> do
      {
        IO.printf state.pages "%f %f %f setrgbcolor\n"
                     (float_of_num r) (float_of_num g) (float_of_num b)
        ; ()
      }
    | Graphic.CMYK c m y k -> do
      {
        IO.printf state.pages "%f %f %f %f setcmykcolor\n"
                     (float_of_num c) (float_of_num m) (float_of_num y) (float_of_num k)
        ; ()
      }
    ];
    let write_path path_cmd path state = do
    {
      let rec draw_path cur_x cur_y path = match path with
      [ [] -> match path_cmd with
              [ Graphic.Stroke -> IO.write_string state.pages " stroke"
              | Graphic.Fill   -> IO.write_string state.pages " fill"
              | Graphic.Clip   -> IO.write_string state.pages " clip"
              ]
      | [(ax,ay,bx,by,cx,cy,dx,dy) :: ps] -> do
        {
          if ax <>/ cur_x || ay <>/ cur_y then
            IO.printf state.pages " %f %f moveto" (pt_to_bp ax) (pt_to_bp ay)
          else ();

          IO.printf state.pages " %f %f %f %f %f %f curveto"
            (pt_to_bp bx) (pt_to_bp by)
            (pt_to_bp cx) (pt_to_bp cy)
            (pt_to_bp dx) (pt_to_bp dy);

          draw_path dx dy ps
        }
      ];

      match path with
      [ [] -> ()
      | [(ax,_,_,_,_,_,_,_) :: _] -> do
        {
          IO.write_string state.pages " newpath";

          (* Choose arbitrary coordinates for the current point.
             Just make sure they are different from the first point of the path. *)
          draw_path (ax -/ num_one) num_zero path;
        }
      ]
    };
    let set_alpha _ = do
    {
      log_string "Warning: PostScript does not support transparency."
    };
    let set_line_width w = do
    {
      IO.printf state.pages " %f setlinewidth" (pt_to_bp w)
    };
    let set_line_cap c = match c with
    [ Graphic.Butt   -> IO.write_string state.pages "0 setlinecap"
    | Graphic.Circle -> IO.write_string state.pages "1 setlinecap"
    | Graphic.Square -> IO.write_string state.pages "2 setlinecap"
    ];
    let set_line_join j = match j with
    [ Graphic.Miter -> IO.write_string state.pages "0 setlinejoin"
    | Graphic.Round -> IO.write_string state.pages "1 setlinejoin"
    | Graphic.Bevel -> IO.write_string state.pages "2 setlinejoin"
    ];
    let set_miter_limit l = do
    {
      IO.printf state.pages "%f setmiterlimit" (float_of_num l)
    };

    let write_gfx_cmd cmd = match cmd with
    [ Graphic.PutBox bx by box -> write_boxes box (x +/ bx) (y +/ by)
    | Graphic.Draw pc p        -> write_path pc p state
    | Graphic.SetColour col    -> set_colour col
    | Graphic.SetAlpha a       -> set_alpha a
    | Graphic.SetBgColour _    -> assert False
    | Graphic.SetLineWidth  w  -> set_line_width  w
    | Graphic.SetLineCap    c  -> set_line_cap    c
    | Graphic.SetLineJoin   j  -> set_line_join   j
    | Graphic.SetMiterLimit l  -> set_miter_limit l
    ];

    IO.write_string state.pages "gsave\n";
    List.iter write_gfx_cmd bs;
    IO.write_string state.pages "grestore\n"
  };

  IO.write_string state.pages "%%Page: ";
  IO.printf       state.pages "%d %d\n" page.p_number no;

  write_boxes page.p_contents num_zero page.p_height;

  IO.write_string state.pages "showpage\n";
};

value write_postscript_file name comment pages = do
{
  let state = new_state ();

  write_preamble state.preamble comment pages;

  List.fold_left
    (fun no p -> do { write_page state no p; no + 1 })
    1
    pages;

  IO.write_string state.pages "\n%%EOF\n";

  List.fold_left
    (fun i f -> do { write_font_defs state i f; i+1 })
    0
    state.fonts;

  let oc = open_out_bin name;

  IO.to_channel state.preamble oc;
  IO.to_channel state.pages    oc
};



open XNum;
open Unicode;
open GlyphMetric;
open FontMetric;
open Logging;

type font_ref =
{
  font              : font_metric;
  first_glyph_index : int;        (* minimal glyph index (from font-metric)    *)
  used_glyphs       : mutable int;(* number of used glyphs                     *)
  glyph_map         : array int   (* mapping internal glyph-index -> PDF index *)
};

type image_ref = (string * int);  (* filename, object id    *)

type state =
{
  os      : mutable IO.ostream;
  fonts   : mutable list font_ref;
  images  : mutable list image_ref;
  clip    : mutable int
};

value new_state name =
{
  os      = IO.make_out_stream name;
  fonts   = [];
  images  = [];
  clip    = 0
};

value pt_to_bp x = float_of_num (num_of_ints 7200 7227 */ x);

value new_clip_path state = do
{
  state.clip := state.clip + 1;
  state.clip
};

value get_clip_path state = do
{
  state.clip
};

value select_image state file = do
{
  ""
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
value write_bitmap cs bm = do
{
  IO.printf cs "BI /IM true /D [1 0] /W %d /H %d ID "
    bm.Bitmap.bm_width
    bm.Bitmap.bm_height;
  IO.write_string cs bm.Bitmap.bm_data;
  IO.write_string cs "EI "
};

value bitmap_to_type3_glyph state fm g = do
{
  let gm = fm.glyph_metric.(g - fm.first_glyph);
  let g  = fm.get_glyph_bitmap fm g;

  let cs = IO.make_buffer_stream 0x100;

  let hdpi  = 72.27 /. 72.0 *. g.Glyph.g_hdpp;
  let vdpi  = 72.27 /. 72.0 *. g.Glyph.g_vdpp;
  let min_x = float_of_int g.Glyph.g_min_x /. hdpi;
  let min_y = float_of_int g.Glyph.g_min_y /. vdpi;
  let max_x = float_of_int g.Glyph.g_max_x /. hdpi;
  let max_y = float_of_int g.Glyph.g_max_y /. vdpi;

  IO.printf cs "%f 0 %f %f %f %f d1 "
    (pt_to_bp gm.gm_width) min_x min_y max_x max_y;
  IO.printf cs "1 0 0 1 %f %f cm %f 0 0 %f 0 0 cm "
    min_x min_y
    (float_of_int (g.Glyph.g_max_x - g.Glyph.g_min_x + 1) /. hdpi)
    (float_of_int (g.Glyph.g_max_y - g.Glyph.g_min_y + 1) /. vdpi);
  write_bitmap cs g.Glyph.g_bitmap;

  let contents = PDF.alloc_object state.pdf;

  PDF.set_object state.pdf contents (PDF.Stream [] cs);

  ((min_x, min_y, max_x, max_y), PDF.Reference contents 0)
};

value font_encoding fm encoding = do
{
  let rec get_name_list i list = do
  {
    if i < 0 then
      list
    else
      get_name_list (i-1) [ PDF.Symbol (fm.get_glyph_name encoding.(i)) :: list ]
  }
  in

  PDF.Dictionary
    [("Type",        PDF.Symbol "Encoding");
     ("Differences", PDF.Array [PDF.Int 0
                                 :: get_name_list (Array.length encoding - 1) [] ])]
};

value new_type3_font state font_name fm encoding = do
{
  let obj   = PDF.alloc_object state.pdf;
  let scale = pt_to_bp (num_one // fm.at_size);

  let width_array = Array.map
    (fun gm -> PDF.Float (pt_to_bp gm.gm_width))
    fm.glyph_metric
  in

  let rec calc_glyph_data g dict ((min_x, min_y, max_x, max_y) as bounds) = do
  {
    if g > fm.last_glyph then
      (bounds, dict)
    else do
    {
      let ((x1, y1, x2, y2), bitmap) = bitmap_to_type3_glyph state fm g;

      calc_glyph_data
        (g + 1)
        [ (fm.get_glyph_name g, bitmap)
          :: dict]
        (min min_x x1, min min_y y1, max max_x x2, max max_y y2)
    }
  }
  in
  let ((min_x, min_y, max_x, max_y), char_procs) =
        calc_glyph_data fm.first_glyph [] (0.0, 0.0, 0.0, 0.0)
  in


value new_type1_font state font_name fm encoding = do
{
  let obj   = PDF.alloc_object state.pdf;
  let scale x = float_of_num (x */ num_of_int 1000 // fm.at_size);

  let width_array = Array.map
    (fun gm -> PDF.Float (scale gm.gm_width))
    fm.glyph_metric
  in

  let (max_width, max_height, max_depth) =
    Array.fold_left
      (fun (w,h,d) gm ->
        (max_num w gm.gm_width,
         max_num h gm.gm_height,
         max_num d gm.gm_depth))
      (num_zero, num_zero, num_zero)
      fm.glyph_metric
  in

  let fd = PDF.alloc_object state.pdf;
  let ff = PDF.alloc_object state.pdf;

  let file      = IO.make_in_stream fm.file_name;
  let font_data = IO.to_buffer file;

  IO.free file;

  (* FIX: assume CFF format *)
  PDF.set_object state.pdf ff
    (PDF.Stream [("Subtype", PDF.Symbol "Type1C")] font_data);

  PDF.set_object state.pdf fd
    (PDF.Dictionary
      [
        ("Type",           PDF.Symbol "FontDescriptor");
        ("FontName",       PDF.Symbol fm.ps_name);
        ("Flags",          PDF.Int 0x4);
        ("FontBBox",       PDF.Array [PDF.Int 0;
                                      PDF.Float (scale (minus_num max_depth));
                                      PDF.Float (scale max_width);
                                      PDF.Float (scale max_height)]);
        ("ItalicAngle",    PDF.Int 0);
        ("Ascent",         PDF.Float (scale max_height));
        ("Descent",        PDF.Float (scale (minus_num max_depth)));
        ("CapHeight",      PDF.Float (scale max_height));
        ("StemV",          PDF.Float (scale fm.parameter.rule_thickness));
        ("FontFile3",      PDF.Reference ff 0)
      ]);

  PDF.set_object state.pdf obj
    (PDF.Dictionary
      [
        ("Type",           PDF.Symbol "Font");
        ("Name",           PDF.Symbol font_name);
        ("Subtype",        PDF.Symbol "Type1");
        ("BaseFont",       PDF.Symbol fm.ps_name);
                                                 (* font subset: "AAAAAA+" ^ fm.ps_name *)
        ("FirstChar",      PDF.Int    fm.first_glyph);
        ("LastChar",       PDF.Int    fm.last_glyph);
        ("Widths",         PDF.Array  (Array.to_list width_array));
        ("Encoding",       font_encoding fm encoding);
        ("FontDescriptor", PDF.Reference fd 0)
      ]);

  obj
};

value new_truetype_font state font_name fm encoding = do
{
  let obj     = PDF.alloc_object state.pdf;
  let scale x = float_of_num (x */ num_of_int 1000 // fm.at_size);

  let width_array = Array.map
    (fun gm -> PDF.Float (scale gm.gm_width))
    fm.glyph_metric
  in

  let (max_width, max_height, max_depth) =
    Array.fold_left
      (fun (w,h,d) gm ->
        (max_num w gm.gm_width,
         max_num h gm.gm_height,
         max_num d gm.gm_depth))
      (num_zero, num_zero, num_zero)
      fm.glyph_metric
  in

  let fd = PDF.alloc_object state.pdf;
  let ff = PDF.alloc_object state.pdf;

  let file      = IO.make_in_stream fm.file_name;
  let font_data = IO.to_buffer file;

  IO.free file;

  PDF.set_object state.pdf ff
    (PDF.Stream [] font_data);

  PDF.set_object state.pdf fd
    (PDF.Dictionary
      [
        ("Type",           PDF.Symbol "FontDescriptor");
        ("FontName",       PDF.Symbol fm.ps_name);
        ("Flags",          PDF.Int 0x4);
        ("FontBBox",       PDF.Array [PDF.Int 0;
                                      PDF.Float (scale (minus_num max_depth));
                                      PDF.Float (scale max_width);
                                      PDF.Float (scale max_height)]);
        ("ItalicAngle",    PDF.Int 0);
        ("Ascent",         PDF.Float (scale max_height));
        ("Descent",        PDF.Float (scale (minus_num max_depth)));
        ("CapHeight",      PDF.Float (scale max_height));
        ("StemV",          PDF.Float (scale fm.parameter.rule_thickness));
        ("FontFile2",      PDF.Reference ff 0)
      ]);

  PDF.set_object state.pdf obj
    (PDF.Dictionary
      [
        ("Type",           PDF.Symbol "Font");
        ("Name",           PDF.Symbol font_name);
        ("Subtype",        PDF.Symbol "TrueType");
        ("BaseFont",       PDF.Symbol fm.ps_name);
                                                 (* font subset: "AAAAAA+" ^ fm.ps_name *)
        ("FirstChar",      PDF.Int    fm.first_glyph);
        ("LastChar",       PDF.Int    fm.last_glyph);
        ("Widths",         PDF.Array  (Array.to_list width_array));
        ("Encoding",       font_encoding fm encoding);
(*        ("CIDSystemInfo",  PDF.Dictionary
                             [ ("Registry",   PDF.String (IO.from_string "unknown"));
                               ("Ordering",   PDF.String (IO.from_string "unknown"));
                               ("Supplement", PDF.Int 0) ]);*)
        ("FontDescriptor", PDF.Reference fd 0)
      ]);

  obj
};

value make_font_obj state font_number font_def = do
{
  iter 0 []

  where rec iter i objs = do
  {
    if i >= font_def.used_glyphs then
      objs
    else do
    {
      let encoding = Array.make (min 0x100 (font_def.used_glyphs - i)) (-1);

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

      let new_font = match font_def.font.font_type with
(*
      [ "type1"    -> new_type1_font
      | "truetype" -> new_truetype_font
*)
      [ _          -> new_type3_font
      ]
      in

      let obj = new_font state (Printf.sprintf "F%d.%d" font_number i) font_def.font encoding;

      iter (i + 0x100) [obj :: objs]
    }
  }
};

(* images *)

value new_image state file = do
{
  let conv_id cs s = do
  {
    IO.write_string cs s
  }
  in
  let conv_rgba cs s = do
  {
    for i = 0 to String.length s / 4 - 1 do
    {
      IO.write_char cs s.[3*i];
      IO.write_char cs s.[3*i+1];
      IO.write_char cs s.[3*i+2]
    }
  }
  in
  let get_colourmap x = do
  {
    let cs = IO.make_buffer_stream 0x100;
    let cm = x.CamlImages.Index8.colormap;

    for i = 0 to cm.CamlImages.Color.max do
    {
      IO.write_byte cs cm.CamlImages.Color.map.(i).CamlImages.Color.r;
      IO.write_byte cs cm.CamlImages.Color.map.(i).CamlImages.Color.g;
      IO.write_byte cs cm.CamlImages.Color.map.(i).CamlImages.Color.b
    };
    cs
  }
  in
  let image_info img = match img with
  [ CamlImages.Images.Index8 x  -> (PDF.Array [PDF.Symbol "Indexed";
                                               PDF.Symbol "DeviceRGB";
                                               PDF.Int 255;
                                               PDF.String (get_colourmap x)],
                                    8,
                                    CamlImages.Index8.get_scanline x,
                                    conv_id)
  | CamlImages.Images.Index16 x -> (PDF.Symbol "DeviceRGB",
                                    8,
                                    CamlImages.Rgb24.get_scanline (CamlImages.Index16.to_rgb24 x),
                                    conv_id)
  | CamlImages.Images.Rgb24 x   -> (PDF.Symbol "DeviceRGB",
                                    8,
                                    CamlImages.Rgb24.get_scanline x,
                                    conv_id)
  | CamlImages.Images.Rgba32 x  -> (PDF.Symbol "DeviceRGB",
                                    8,
                                    CamlImages.Rgba32.get_scanline x,
                                    conv_rgba)
  | CamlImages.Images.Cmyk32 x  -> (PDF.Symbol "DeviceCMYK",
                                    8,
                                    CamlImages.Cmyk32.get_scanline x,
                                    conv_id)
  ]
  in
  let image_data scan conv img = do
  {
    let (w, h) = CamlImages.Images.size img;
    let cs     = IO.make_buffer_stream (w * h / 8);


    for i = 0 to h - 1 do
    {
      let s = scan i;

      conv cs s
    };
    cs
  }
  in
  let obj = PDF.alloc_object state.pdf;
  let n   = List.length state.images;

  state.images := state.images @ [(file, obj)];

  let img             = CamlImages.Images.load file [];
  let (width, height) = CamlImages.Images.size img;
  let (colsp, bits, scan, conv) = image_info img;

  PDF.set_object state.pdf obj
    (PDF.Stream
      [
        ("Type",             PDF.Symbol "XObject");
        ("Subtype",          PDF.Symbol "Image");
        ("Name",             PDF.Symbol (Printf.sprintf "G%d" n));
        ("Width",            PDF.Int width);
        ("Height",           PDF.Int height);
        ("ColorSpace",       colsp);
        ("BitsPerComponent", PDF.Int bits)
      ]
      (image_data scan conv img));

  (n, obj)
};

value load_image state file = do
{
  find 0 state.images

  where rec find n images = match images with
  [ [(i, _) :: is] -> do
    {
      if i = file then
        n
      else
        find (n + 1) is
    }
  | [] -> fst (new_image state file)
  ]
};

value select_image state file = do
{
  let n = load_image state file;

  IO.printf state.os " /G%d " n
};

value create_pdf state pages = do
{
  let rec get_image_refs n images = match images with
  [ []               -> []
  | [(_, obj) :: is] -> [( Printf.sprintf "G%d" n, PDF.Reference obj 0 )
                          :: get_image_refs (n + 1) is]
  ]
  in

  let root_obj  = PDF.alloc_object state.pdf;
  let pages_obj = PDF.alloc_object state.pdf;
  let page_refs = List.map (create_page state pages_obj) pages;

  state.pdf.PDF.root := PDF.Reference root_obj 0;

  PDF.set_object state.pdf pages_obj
    (PDF.Dictionary
      [
        ("Type",      PDF.Symbol "Pages");
        ("Resources", PDF.Dictionary
                        [ ("Font",    PDF.Dictionary (get_font_refs  0 state.fonts));
                          ("XObject", PDF.Dictionary (get_image_refs 0 state.images))
                        ]);
        ("Count",     PDF.Int (List.length pages));
        ("Kids",      PDF.Array (List.map (fun obj -> PDF.Reference obj 0) page_refs))
      ]);

  PDF.set_object state.pdf root_obj
    (PDF.Dictionary
      [
        ("Type",  PDF.Symbol "Catalog");
        ("Pages", PDF.Reference pages_obj 0)
      ])
};
*)

value write_bitmap state bmap = do
{
  for y = 0 to bmap.Bitmap.bm_height - 1 do
  {
    iter 0

    where rec iter x = do
    {
      if x >= bmap.Bitmap.bm_width then
        ()
      else do
      {
        if Bitmap.point bmap x y then do
        {
          scan (x+1)

          where rec scan z = do
          {
            if z >= bmap.Bitmap.bm_width || not (Bitmap.point bmap z y) then
              IO.printf state.os "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"1\"/>" x y (z - x)
            else
              iter (z+1)
          }
        }
        else
          iter (x+1)
      }
    }
  }
};

value write_glyph_bitmap state fm glyph = do
{
  let g       = fm.get_glyph_bitmap fm glyph;
  let h_scale = 72.0 /. (72.27 *. g.GlyphBitmap.g_hdpp);
  let v_scale = 72.0 /. (72.27 *. g.GlyphBitmap.g_vdpp);

  IO.printf state.os "<g transform=\"scale(%f,%f)\"><g transform=\"translate(%d,%d)\">"
    h_scale v_scale g.GlyphBitmap.g_min_x g.GlyphBitmap.g_min_y;

  write_bitmap state g.GlyphBitmap.g_bitmap;

  IO.write_string state.os "</g></g>"
};

value write_bitmap_font state font_number font_def = do
{
  IO.printf state.os "<font><font-face font-family=\"F%d\"/>\n" font_number;

  Array.iteri
    (fun g idx -> do
     {
       if idx < 0 then
         ()
       else do
       {
         IO.printf          state.os "<glyph unicode=\"&#%d;\">" (100 + idx);

         write_glyph_bitmap state font_def.font g;

         IO.write_string    state.os "</glyph>\n"
       }
     })
    font_def.glyph_map;

  IO.write_string state.os "</font>\n";
};

value write_vector_font state font_number font_def = do
{
  IO.printf state.os "<font><font-face font-family=\"F%d\" units-per-em=\"%d\"/>\n" font_number 1000(*XXX*);

  for i = 0 to font_def.used_glyphs - 1 do
  {
    IO.printf          state.os "<glyph unicode=\"&#x%d;\" d=\"" (100 + i);
    IO.write_string    state.os "\"/>\n"
  };

  IO.write_string state.os "</font>\n";
};

value write_font state font_number font_def = match font_def.font.font_type with
(*
[ "type1"    -> write_vector_font state font_number font_def
| "truetype" -> write_vector_font state font_number font_def
*)
[ _          -> write_bitmap_font state font_number font_def
];

value rec write_box state x y box = match box with
[ Empty           -> ()
| Rule w h        -> write_rule  state x y w h
| SimpleGlyph g f -> write_char  state x y g f
| Image w h f fmt -> write_image state x y w h f fmt
| Group bs        -> write_group state x y bs
| Command cmd     -> match cmd with
    [ `DVI_Special _ -> ()
    ]
]
and write_rule state x y width height = do
{
  IO.printf state.os "<rect x=\"%fpt\" y=\"%fpt\" width=\"%fpt\" height=\"%fpt\"/>"
      (pt_to_bp x) (pt_to_bp y) (pt_to_bp width) (pt_to_bp height);
}
and write_char state x y c f = do
{
  let (fn, cn) = load_font state f c;

  IO.printf state.os "<text x=\"%fpt\" y=\"%fpt\" font-family=\"F%d\" font-size=\"%fpt\">"
    (pt_to_bp x) (pt_to_bp y) fn (pt_to_bp f.at_size);
  IO.printf state.os "&#%d;" (100+cn);
  IO.write_string    state.os "</text>"
}
and write_image state x y width height file _fmt = do
{
  IO.printf state.os "<image x=\"%fpt\" y=\"%fpt\" width=\"%fpt\" height=\"%fpt\" xlink:href=\"%s\"/>"
    (pt_to_bp x) (pt_to_bp y) (pt_to_bp width) (pt_to_bp height) file
}
and write_path state path_cmd path = do
{
  let rec draw_path cur_x cur_y path = match path with
  [ [] -> match path_cmd with
          [ Graphic.Stroke -> IO.write_string state.os "Z fill=\"none\" stroke=\"currentColor\" "
          | Graphic.Fill   -> IO.write_string state.os "Z fill=\"currentColor\" stroke=\"none\" "
          | Graphic.Clip   -> IO.write_string state.os "Z "
          ]
  | [(ax,ay,bx,by,cx,cy,dx,dy) :: ps] -> do
    {
      if ax <>/ cur_x || ay <>/ cur_y then
        IO.printf state.os "M %fpt %fpt " (pt_to_bp ax) (pt_to_bp ay)
      else match path_cmd with
        [ Graphic.Stroke -> IO.write_string state.os "Z fill=\"none\" stroke=\"currentColor\" "
        | Graphic.Fill   -> IO.write_string state.os "Z fill=\"currentColor\" stroke=\"none\" "
        | Graphic.Clip   -> IO.write_string state.os "Z "
        ];

      IO.printf state.os "C %fpt %fpt %fpt %fpt %fpt %fpt "
        (pt_to_bp bx) (pt_to_bp by)
        (pt_to_bp cx) (pt_to_bp cy)
        (pt_to_bp dx) (pt_to_bp dy);

      draw_path dx dy ps
    }
  ];

  match path_cmd with
  [ Graphic.Stroke
  | Graphic.Fill   -> ()
  | Graphic.Clip   -> IO.printf state.os "<clipPath id=\"cp%d\">" (new_clip_path state)
  ];

  IO.write_string state.os "<path d=\"";

  match path with
  [ [] -> ()
  | [(ax,_,_,_,_,_,_,_) :: _] -> do
    {
      (* Choose arbitrary coordinates for the current point.
         Just make sure they are different from the first point of the path. *)
      draw_path (ax -/ num_one) num_zero path;
    }
  ];

  IO.write_string state.os "\"/>";

  match path_cmd with
  [ Graphic.Stroke
  | Graphic.Fill   -> ()
  | Graphic.Clip   -> IO.write_string state.os "</clipPath>\n"
  ]
}
and write_group state x y gfx_cmds = do
{
  let set_colour col = match col with
  [ Graphic.Grey x       -> do
    {
      let s = int_of_num (round_num (num_of_int 255 */ x));

      IO.printf state.os "<g color=\"%02x%02x%02x\">" s s s
    }
  | Graphic.RGB r g b    -> do
    {
      let rr = int_of_num (round_num (num_of_int 255 */ r));
      let gg = int_of_num (round_num (num_of_int 255 */ g));
      let bb = int_of_num (round_num (num_of_int 255 */ b));

      IO.printf state.os "<g color=\"%02x%02x%02x\">" rr gg bb
    }
  | Graphic.CMYK c m y k -> do
    {
      let r  = num_one -/ c -/ k;
      let g  = num_one -/ m -/ k;
      let b  = num_one -/ y -/ k;
      let rr = int_of_num (round_num (num_of_int 255 */ r));
      let gg = int_of_num (round_num (num_of_int 255 */ g));
      let bb = int_of_num (round_num (num_of_int 255 */ b));

      IO.printf state.os "<g color=\"%02x%02x%02x\">" rr gg bb
    }
  ];
  let set_alpha a = do
  {
    (* FIX *)
    ()
  };
  let set_line_width w = do
  {
    IO.printf state.os "<g stroke-width=\"%fpt\">" (pt_to_bp w)
  };
  let set_line_cap c = match c with
  [ Graphic.Butt   -> IO.write_string state.os "<g stroke-linecap=\"butt\">"
  | Graphic.Circle -> IO.write_string state.os "<g stroke-linecap=\"round\">"
  | Graphic.Square -> IO.write_string state.os "<g stroke-linecap=\"square\">"
  ];
  let set_line_join j =  match j with
  [ Graphic.Miter -> IO.write_string state.os "<g stroke-linejoin=\"miter\">"
  | Graphic.Round -> IO.write_string state.os "<g stroke-linejoin=\"round\">"
  | Graphic.Bevel -> IO.write_string state.os "<g stroke-linejoin=\"bevel\">"
  ];
  let set_miter_limit l = do
  {
    IO.printf state.os "<g stroke-miterlinit=\"%fpt\">" (pt_to_bp l)
  };

  let write_gfx nest cmd = match cmd with
  [ Graphic.PutBox dx dy b  -> do { write_box state (x +/ dx) (y +/ dy) b; nest }
  | Graphic.Draw pc p       -> do
    {
      write_path state pc p;

      match pc with
      [ Graphic.Stroke
      | Graphic.Fill   -> nest
      | Graphic.Clip   -> do
        {
          IO.printf state.os "<g clip-path=\"url(#cp%d)\">\n" (get_clip_path state);
          nest + 1
        }
      ]
    }
  | Graphic.SetColour col   -> do { set_colour    col; nest + 1 }
  | Graphic.SetAlpha a      -> do { set_alpha       a; nest + 1 }
  | Graphic.SetBgColour _   -> assert False
  | Graphic.SetLineWidth  w -> do { set_line_width  w; nest + 1 }
  | Graphic.SetLineCap    c -> do { set_line_cap    c; nest + 1 }
  | Graphic.SetLineJoin   j -> do { set_line_join   j; nest + 1 }
  | Graphic.SetMiterLimit l -> do { set_miter_limit l; nest + 1 }
  ];

  let nest = List.fold_left write_gfx 0 gfx_cmds;

  for i = 1 to nest do
  {
    IO.write_string state.os "</g>"
  }
};

value write_page state page = do
{
  IO.printf state.os "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"%fpt\" height=\"%fpt\">\n"
    (pt_to_bp page.p_width) (pt_to_bp page.p_height);

  write_box state num_zero page.p_height page.p_contents;

};

value write_preamble state comment = do
{
  IO.write_string state.os "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n";
  IO.write_string state.os "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.0//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n";
  IO.printf       state.os "<!-- %s -->\n" comment
};

value write_postamble state = do
{
  IO.write_string state.os "<defs>\n";

  iter 0 state.fonts

  where rec iter n fonts = match fonts with
  [ []        -> IO.write_string state.os "</defs>\n"
  | [f :: fs] -> do
    {
      write_font state n f;
      iter (n+1) fs
    }
  ]
};

value write_svg_file name comment pages = do
{
  let state = new_state name;

  write_preamble state comment;

  List.iter (write_page state) pages;

  write_postamble state ;
  
  IO.write_string state.os "</svg>\n";
};


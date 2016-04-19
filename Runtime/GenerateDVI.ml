
open XNum;
open Unicode.Types;
open Logging;
open Dim;
open Substitute;
open GlyphMetric;
open FontMetric;

type dvi_format = [ DVI | XDVI ];

type state 'a =
{
  os     : mutable IO.ostream;
  format : dvi_format;
  data   : mutable 'a
};

type draw_info =
{
  pos_h        : num;
  pos_v        : num;
  current_font : font_metric;
  loaded_fonts : list (font_metric * int);
  stack_depth  : int
};

type page_info =
{
  page_lengths : list int;
  font_defs    : list (font_metric * int);
  max_width    : num;
  max_height   : num;
  max_stack    : int
};

type draw_state = state draw_info;
type page_state = state page_info;

value clear_stack state = do
{
  state.data := { (state.data) with stack_depth = 0 }
};

value rat_to_fixed r = do
{
  round_num (r */ num_of_int 0x10000);
};

value float_to_fixed r = do
{
  num_of_int (int_of_float (r *. 65536.0));
};

(* Convert ant points to PostScript points. *)

value pt_to_bp x = float_of_num (num_of_ints 7200 7227 */ x);

value write_rat os r = do
{
  IO.write_be_i32 os (rat_to_fixed r)
};

value write_special os string = do
{
  let len = String.length string;

  if len < 0x100 then do
  {
    IO.write_be_u8  os 239;
    IO.write_be_u8  os len;
    IO.write_string os string;
  }
  else if len < 0x10000 then do
  {
    IO.write_be_u8  os 240;
    IO.write_be_u16 os len;
    IO.write_string os string;
  }
  else do
  {
    IO.write_be_u8  os 242;
    IO.write_be_u32 os (num_of_int len);
    IO.write_string os string;
  }
};

value write_preamble state comment = do
{
  IO.write_be_u8 state.os 247;

  match state.format with
  [ DVI  -> IO.write_be_u8 state.os 2
  | XDVI -> IO.write_be_u8 state.os 5
  ];

  IO.write_be_u32 state.os (num_of_int 25400000);
  IO.write_be_u32 state.os (num_of_int 473628672);
  IO.write_be_u32 state.os (num_of_int 1000);
  IO.write_be_u8  state.os (String.length comment);
  IO.write_string state.os comment
};

(* |load_tex_font <channel> <font> <font-index>| outputs the DVI-commands to load the given font. *)

value load_tex_font state font idx = do
{
  if idx < 0x100 then do
  {
    IO.write_be_u8 state.os 243;
    IO.write_be_u8 state.os idx;
  }
  else if idx < 0x1000 then do
  {
    IO.write_be_u8  state.os 244;
    IO.write_be_u16 state.os idx;
  }
  else do
  {
    IO.write_be_u8  state.os 245;
    IO.write_be_u32 state.os (num_of_int idx);
  };

  IO.write_be_u32 state.os font.check_sum;
  write_rat       state.os font.at_size;
  write_rat       state.os font.design_size;
  IO.write_be_u8  state.os 0;
  IO.write_be_u8  state.os (String.length font.name);
  IO.write_string state.os font.name
};

value load_native_font state font idx = do
{
  IO.write_be_u8  state.os 252;
  IO.write_be_u32 state.os (num_of_int idx);
  write_rat       state.os font.at_size;
  IO.write_be_u16 state.os 2;
  IO.write_be_u8  state.os (String.length font.file_name + 2);
  IO.write_be_u8  state.os 0;
  IO.write_be_u8  state.os 0;
  IO.write_string state.os "[";
  IO.write_string state.os font.file_name;
  IO.write_string state.os "]";
};

value load_font state font idx = do
{
  if state.format = XDVI && font.font_type <> Other then
    load_native_font state font idx
  else
    load_tex_font state font idx
};

value rec write_font_defs state font_defs = match font_defs with
[ []      -> ()
| [(f,i)::fs] -> do
  {
    load_font state f i;
    write_font_defs state fs
  }
];

value write_postamble state = do
{
  let pos = IO.bytes_written state.os;

  IO.write_be_u8  state.os 248;
  IO.write_be_u32 state.os (num_of_int (pos - List.hd state.data.page_lengths));
  IO.write_be_u32 state.os (num_of_int 25400000);
  IO.write_be_u32 state.os (num_of_int 473628672);
  IO.write_be_u32 state.os (num_of_int 1000);
  write_rat       state.os (state.data.max_height -/ inch);
  write_rat       state.os (state.data.max_width -/ inch);
  IO.write_be_u16 state.os state.data.max_stack;
  IO.write_be_u16 state.os (List.length state.data.page_lengths);

  write_font_defs state state.data.font_defs;

  IO.write_be_u8  state.os 249;
  IO.write_be_u32 state.os (num_of_int pos);

  match state.format with
  [ DVI  -> IO.write_be_u8 state.os 2
  | XDVI -> IO.write_be_u8 state.os 5
  ];

  IO.write_be_u8  state.os 223;
  IO.write_be_u8  state.os 223;
  IO.write_be_u8  state.os 223;
  IO.write_be_u8  state.os 223
};

value move_right state x = do
{
  if num_of_int (-0x80) <=/ x && num_of_int 0x7f >=/ x then do
  {
    IO.write_be_u8 state.os 143;
    IO.write_be_i8 state.os (int_of_num x)
  }
  else if num_of_int (-0x8000) <=/ x && num_of_int 0x7fff >=/ x then do
  {
    IO.write_be_u8  state.os 144;
    IO.write_be_i16 state.os (int_of_num x)
  }
  else do
  {
    IO.write_be_u8  state.os 146;
    IO.write_be_i32 state.os x
  }
};

value move_down state x = do
{
  if num_of_int (-0x80) <=/ x && num_of_int 0x7f >=/ x then do
  {
    IO.write_be_u8 state.os 157;
    IO.write_be_i8 state.os (int_of_num x)
  }
  else if num_of_int (-0x8000) <=/ x && num_of_int 0x7fff >=/ x then do
  {
    IO.write_be_u8  state.os 158;
    IO.write_be_i16 state.os (int_of_num x)
  }
  else do
  {
    IO.write_be_u8  state.os 160;
    IO.write_be_i32 state.os x
  }
};

value rec write_pages state pages = do
{
  let rec write_boxes box box_h box_v state = match box with
  [ Empty           -> clear_stack state
  | SimpleGlyph g f -> write_boxes_char g f box_h box_v state
  | Rule w h        -> write_boxes_rule w h box_h box_v state
  | Image w h f fmt -> write_boxes_image w h f fmt box_h box_v state
  | Group bs        -> write_boxes_group bs box_h box_v state
  | Command cmd     -> match cmd with
      [ `DVI_Special str -> do
        {
          write_special state.os str;
          clear_stack state
        }
      ]
  ]
  and write_boxes_char char font box_h box_v state = do
  {
    let rec choose_font font loaded_fonts = do
    {
      if not (List.mem_assq font loaded_fonts) then do
      {
        let idx = List.length loaded_fonts;

        load_font state font idx;

        choose_font font [(font, idx) :: loaded_fonts]
      }
      else do
      {
        let idx = List.assq font loaded_fonts;

        if idx < 64 then do
        {
          IO.write_be_u8 state.os (idx + 171)
        }
        else if idx < 0x100 then do
        {
          IO.write_be_u8 state.os 235;
          IO.write_be_u8 state.os idx
        }
        else if idx < 0x10000 then do
        {
          IO.write_be_u8  state.os 236;
          IO.write_be_u16 state.os idx
        }
        else do
        {
          IO.write_be_u8  state.os 238;
          IO.write_be_u32 state.os (num_of_int idx)
        }
      }
    };

    let glyph_width glyph font = do
    {
      (FontMetric.get_glyph_metric font (Simple glyph)).gm_width
    };

    let delta_h = rat_to_fixed (glyph_width char font);

    if state.data.current_font != font then do
    {
      let new_loaded_fonts = if List.mem_assq font state.data.loaded_fonts then
                               state.data.loaded_fonts
                             else
                               [(font, List.length state.data.loaded_fonts)
                                 :: state.data.loaded_fonts];

      choose_font font state.data.loaded_fonts;

      state.data := { (state.data) with current_font = font;
                                        loaded_fonts = new_loaded_fonts };

      write_boxes_char char font box_h box_v state
    }
    else if state.format = XDVI && font.font_type <> Other then do
    {
      IO.write_be_u8 state.os 254;
      IO.write_be_u32 state.os delta_h;
      IO.write_be_u16 state.os 1;
      IO.write_be_u32 state.os (num_of_int 0);
      IO.write_be_u16 state.os char;
      state.data := { (state.data) with pos_h = state.data.pos_h +/ delta_h;
                                        stack_depth = 0 }
    }
    else if char < 0x80 then do
    {
      IO.write_be_u8 state.os char;

      state.data := { (state.data) with pos_h = state.data.pos_h +/ delta_h;
                                        stack_depth = 0 }
    }
    else if char < 0x100 then do
    {
      IO.write_be_u8 state.os 128;
      IO.write_be_u8 state.os char;

      state.data := { (state.data) with pos_h = state.data.pos_h +/ delta_h;
                                        stack_depth = 0 }
    }
    else if char < 0x10000 then do
    {
      IO.write_be_u8  state.os 129;
      IO.write_be_u16 state.os char;

      state.data := { (state.data) with pos_h = state.data.pos_h +/ delta_h;
                                        stack_depth = 0 }
    }
    else do
    {
      IO.write_be_u8  state.os 131;
      IO.write_be_u32 state.os (num_of_int char);

      state.data := { (state.data) with pos_h = state.data.pos_h +/ delta_h;
                                        stack_depth = 0 }
    }
  }
  and write_boxes_rule width height _box_h _box_v state = do
  {
    IO.write_be_u8 state.os 137;
    write_rat      state.os height;
    write_rat      state.os width;

    clear_stack state
  }
  and write_boxes_image width height file_name fmt _box_h _box_v state = do
  {
    match fmt with
    [ LoadImage.PostScript -> do
      {
        let w = pt_to_bp width;
        let h = pt_to_bp height;

        let (x0, y0, x1, y1) = match LoadImage.get_bounding_box file_name with
        [ Some (x0, y0, x1, y1) -> (float_of_int x0, float_of_int y0,
                                    float_of_int x1, float_of_int y1)
        | None                  -> (0.0, 0.0, w, h)
        ];

        move_right state (float_to_fixed (~-. x0));
        move_down  state (float_to_fixed y0);

        write_special state.os
          (Printf.sprintf "PSfile=\"%s\" llx=%f lly=%f urx=%f ury=%f rwi=%f rhi=%f"
                          file_name x0 y0 x1 y1 w h)
      }
    | LoadImage.Bmp ->
        write_special state.os
          (Printf.sprintf "em: graph %s, %f pt, %f pt"
                          file_name
                          (float_of_num width)
                          (float_of_num height))
    | _ -> do
      {
        log_string "\nWarning: Unsupported image format!"
      }
    ];
    clear_stack state
  }
  and write_boxes_group bs box_h box_v state = do
  {
    let write_box bh bv box state = do
    {
      let delta_h = rat_to_fixed (bh +/ box_h) -/ state.data.pos_h;
      let delta_v = rat_to_fixed (bv +/ box_v) -/ state.data.pos_v;

      if delta_h <>/ num_zero then
        move_right state delta_h
      else ();

      if delta_v <>/ num_zero then
        move_down state (minus_num delta_v)
      else ();

      state.data := { (state.data) with pos_h = state.data.pos_h +/ delta_h;
                                        pos_v = state.data.pos_v +/ delta_v };

      let old_stack_depth = state.data.stack_depth;

      write_boxes box (box_h +/ bh) (box_v +/ bv) state;

      state.data :=
        {
          (state.data)

          with

          stack_depth = max old_stack_depth state.data.stack_depth
        }
    };
    let write_path path_cmd path state = do
    {
      let str = IO.make_buffer_stream 1024;

      let rec draw_path cur_x cur_y path = match path with
      [ [] -> match path_cmd with
              [ Graphic.Stroke -> IO.write_string str " stroke"
              | Graphic.Fill   -> IO.write_string str " fill"
              | Graphic.Clip   -> IO.write_string str " clip"
              ]
      | [(ax,ay,bx,by,cx,cy,dx,dy) :: ps] -> do
        {
          if ax <>/ cur_x || ay <>/ cur_y then
            IO.printf str " %f %f moveto" (pt_to_bp ax) (pt_to_bp ay)
          else ();

          IO.printf str " %f %f %f %f %f %f curveto"
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
          IO.write_string str "\" newpath";

          (* Choose arbitrary coordinates for the current point.
             Just make sure they are different from the first point of the path. *)
          draw_path (ax -/ num_one) num_zero path;

          write_special state.os (IO.to_string str)
        }
      ]
    };
    let reset_colour state = do
    {
      write_special state.os "color pop"
    };
    let set_colour colour_changed col state = do
    {
      if colour_changed then
        reset_colour state
      else ();

      let str n = string_of_float (float_of_num n);

      let color_spec = match col with
      [ Graphic.Grey x       -> "gray " ^ str x
      | Graphic.RGB r g b    -> "rgb " ^ str r ^ " " ^ str g ^ " " ^ str b
      | Graphic.CMYK c m y k -> "cmyk " ^ str c ^ " " ^ str m ^ " " ^ str y ^ " " ^ str k
      ];

      write_special state.os ("color push " ^ color_spec)
    };
    let set_alpha _col _state = do
    {
      log_string "Warning: The DVI driver does not support transparency!"
    };
    let set_line_width w state = do
    {
      write_special state.os ("\" " ^ string_of_float (pt_to_bp w) ^ " setlinewidth")
    };
    let set_line_cap c state = do
    {
      let cap = match c with
      [ Graphic.Butt   -> "0"
      | Graphic.Circle -> "1"
      | Graphic.Square -> "2"
      ];

      write_special state.os ("\" " ^ cap ^ " setlinecap")
    };
    let set_line_join j state = do
    {
      let join = match j with
      [ Graphic.Miter -> "0"
      | Graphic.Round -> "1"
      | Graphic.Bevel -> "2"
      ];

      write_special state.os ("\" " ^ join ^ " setlinejoin")
    };
    let set_miter_limit l state = do
    {
      write_special state.os ("\" " ^ string_of_float (float_of_num l) ^ " setmiterlimit")
    };

    IO.write_be_u8 state.os 141;

    let old_pos_h = state.data.pos_h;
    let old_pos_v = state.data.pos_v;

    clear_stack state;

    iter False bs

    where rec iter colour_changed gfx_cmds = match gfx_cmds with
    [ []      -> do
      {
        if colour_changed then
          reset_colour state
        else ();
      }
    | [c::cs] -> match c with
      [ Graphic.PutBox x y b    -> do { write_box x y b state;     iter colour_changed cs }
      | Graphic.Draw pc p       -> do { write_path pc p state;     iter colour_changed cs }
      | Graphic.SetColour col   -> do { set_colour colour_changed col state; iter True cs }
      | Graphic.SetAlpha a      -> do { set_alpha a state;         iter colour_changed cs }
      | Graphic.SetBgColour _   -> assert False
      | Graphic.SetLineWidth  w -> do { set_line_width  w state; iter colour_changed cs }
      | Graphic.SetLineCap    c -> do { set_line_cap    c state; iter colour_changed cs }
      | Graphic.SetLineJoin   j -> do { set_line_join   j state; iter colour_changed cs }
      | Graphic.SetMiterLimit l -> do { set_miter_limit l state; iter colour_changed cs }
      ]
    ];

    IO.write_be_u8 state.os 142;

    state.data := {
                    (state.data)

                    with

                    pos_h = old_pos_h;
                    pos_v = old_pos_v;
                    stack_depth = state.data.stack_depth + 1
                  }
  };

  let start_pos = IO.bytes_written state.os;

  match pages with
  [ []      -> ()
  | [p::ps] -> do
    {
      IO.write_be_u8  state.os 139;
      IO.write_be_u32 state.os (num_of_int p.p_number);
      IO.write_be_u32 state.os num_zero;
      IO.write_be_u32 state.os num_zero;
      IO.write_be_u32 state.os num_zero;
      IO.write_be_u32 state.os num_zero;
      IO.write_be_u32 state.os num_zero;
      IO.write_be_u32 state.os num_zero;
      IO.write_be_u32 state.os num_zero;
      IO.write_be_u32 state.os num_zero;
      IO.write_be_u32 state.os num_zero;

      match state.data.page_lengths with
      [ []     ->  IO.write_be_i32 state.os (num_of_int (-1))
      | [l::_] ->  IO.write_be_i32 state.os (num_of_int (start_pos - l))
      ];

      let draw_state =
        {
          (state)

          with

          data = {
                   pos_h        = rat_to_fixed inch;
                   pos_v        = rat_to_fixed inch;
                   current_font = empty_font;
                   loaded_fonts = state.data.font_defs;
                   stack_depth  = 0
                 }
        };

      write_boxes
        (Group [Graphic.PutBox (minus_num inch) inch p.p_contents])  (* translate the origin *)
        inch inch
        draw_state;

      state.os := draw_state.os;

      IO.write_be_u8 state.os 140;

      state.data := {
                      page_lengths = [IO.bytes_written state.os - start_pos
                                       :: state.data.page_lengths];
                      font_defs    = draw_state.data.loaded_fonts;
                      max_width    = max state.data.max_width  p.p_width;
                      max_height   = max state.data.max_height p.p_height;
                      max_stack    = max state.data.max_stack  draw_state.data.stack_depth
                    };

      write_pages state ps
    }
  ]
};

value write_file format name comment pages = do
{
  let state =
  {
    os     = (IO.make_out_stream name :> IO.ostream);
    format = format;
    data   =
      {
        page_lengths = [];
        font_defs    = [];
        max_width    = num_zero;
        max_height   = num_zero;
        max_stack    = 0
      }
  };

  write_preamble  state comment;
  write_pages     state pages;
  write_postamble state;

  IO.free state.os
};

value write_dvi_file name comment pages = do
{
  write_file DVI name comment pages
};

value write_xdvi_file name comment pages = do
{
  write_file XDVI name comment pages
};


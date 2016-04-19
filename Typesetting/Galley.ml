
open XNum;
open Runtime;
open Dim;
open Box;

type line_params =
{
  baseline_skip      : dim;               (* amount of glue between baselines           *)
  line_skip_limit    : num;               (* minimal amout of glue between lines        *)
  line_skip          : dim;               (* the amout if the lines are closer together *)
  leading            : box -> box -> line_params -> dim;
  club_widow_penalty : int -> int -> num  (* penalty between two lines                  *)
};

type space_params =
{
  space_factor      : num;
  space_skip        : option dim;
  xspace_skip       : option dim;
  victorian_spacing : bool
};

type graphics_params =
{
  gp_colour    : Graphic.colour;
  gp_bg_colour : Graphic.colour;
  gp_alpha     : num
};

type galley =
{
  lines                     : list box;      (* list of all lines and glue in reverse order *)
  glue                      : list box;      (* glue at the end of the galley               *)
  measure                   : num;           (* the width of the galley                     *)
  graphics_params           : graphics_params;
  current_par_params        : ParLayout.par_params;
  current_line_params       : line_params;
  current_line_break_params : ParLayout.line_break_params;
  current_hyphen_params     : JustHyph.hyphen_params;
  current_space_params      : space_params;
  current_math_params       : MathLayout.math_params;
  par_params                : ParLayout.par_params;
  line_params               : line_params;
  line_break_params         : ParLayout.line_break_params;
  hyphen_params             : JustHyph.hyphen_params;
  space_params              : space_params;
  math_params               : MathLayout.math_params
};

(*
  |skyline_dist <line 1> <line 2>| determines the distance between the baselines of <line 1>
  and <line 2> if they were set without glue between them.
*)

value skyline_dist line1 line2 = do
{
  (* <off> is the vertical offset of <b2> w.r.t. <b1> *)

  let rec dist off b1 b2 = match (b1.b_contents, b2.b_contents) with
  [ (CompBox c1, CompBox _)  -> dist_comp_comp     off c1 b2
  | (CompBox c1, _)          -> dist_comp_simple   off c1 b2
  | (_,          CompBox c2) -> dist_simple_comp   off b1 c2
  | (_,          _)          -> dist_simple_simple off b1 b2
  ]
  and dist_simple_simple off b1 b2 = do
  {
    if off >=/ b2.b_width.d_base then
      dim_max b1.b_depth b2.b_height    (* boxes do not intersect *)
    else
      dim_add b1.b_depth b2.b_height
  }
  and dist_simple_comp off b1 c2 = match c2 with
  [ [] -> b1.b_depth
  | [Graphic.PutBox x y b :: cs] -> do
    {
      let d = dim_add (dist (off +/ x.d_base) b1 b) y;
      dim_max d (dist_simple_comp off b1 cs)
    }
  | [_ :: cs] -> dist_simple_comp off b1 cs
  ]
  and dist_comp_simple off c1 b2 = match c1 with
  [ [] -> b2.b_height
  | [Graphic.PutBox x y b :: cs] -> do
    {
      let d = dim_sub (dist (off -/ x.d_base) b b2) y;
      dim_max d (dist_comp_simple off cs b2)
    }
  | [_ :: cs] -> dist_comp_simple off cs b2
  ]
  and dist_comp_comp off c1 b2 = match c1 with
  [ [] -> dim_zero
  | [Graphic.PutBox x y b :: cs] -> do
    {
      let d = dim_sub (dist (off -/ x.d_base) b b2) y;
      dim_max d (dist_comp_comp off cs b2)
    }
  | [_ :: cs] -> dist_comp_comp off cs b2
  ];

  dist num_zero line1 line2
};

(*
  |leading_<version> <line 1> <line 2> <line-params>| determines the amount of glue that has to be inserted
  between two lines.

  |fixed| the distance between baselines is <baseline-skip>,
  |register| the distance is a mutliple of <baseline-skip>,
  |TeX| uses the TeX-algorithm,
  |skyline| uses the TeX-algorithm with the skyline distance.
*)

value leading_fixed line1 line2 line_params = do
{
  let dist = dim_add line1.b_depth line2.b_height;

  dim_sub line_params.baseline_skip dist;
};

value leading_register line1 line2 line_params = do
{
  let dist = dim_add line1.b_depth line2.b_height;
  let fac  = ceiling_num ((dist.d_base +/ line_params.line_skip_limit)
                        // line_params.baseline_skip.d_base);

  dim_sub (fixed_dim (fac */ line_params.baseline_skip.d_base)) dist
};

value leading_TeX line1 line2 line_params = do
{
  let dist = dim_add line1.b_depth line2.b_height;

  if line_params.baseline_skip.d_base >=/
     dist.d_base +/ line_params.line_skip_limit then
    dim_sub line_params.baseline_skip dist
  else
    line_params.line_skip
};

value leading_skyline line1 line2 line_params = do
{
  let simple_dist = dim_add line1.b_depth line2.b_height;

  if line_params.baseline_skip.d_base >=/
     simple_dist.d_base +/ line_params.line_skip_limit then
    dim_sub line_params.baseline_skip simple_dist
  else do
  {
    let dist = skyline_dist line1 line2;

    if line_params.baseline_skip.d_base >=/ dist.d_base +/ line_params.line_skip_limit then
      dim_sub line_params.baseline_skip simple_dist
    else
      dim_sub line_params.line_skip (dim_sub simple_dist dist)
  }
};

(* galleys *)

value new_galley measure line_params par_params line_break_params hyphen_params space_params math_params =
{
  lines                     = [];
  glue                      = [];
  measure                   = measure;
  par_params                = { (par_params) with ParLayout.measure = measure };
  line_params               = line_params;
  line_break_params         = line_break_params;
  hyphen_params             = hyphen_params;
  space_params              = space_params;
  math_params               = math_params;
  current_par_params        = { (par_params) with ParLayout.measure = measure };
  current_line_params       = line_params;
  current_line_break_params = line_break_params;
  current_hyphen_params     = hyphen_params;
  current_space_params      = space_params;
  current_math_params       = math_params;
  graphics_params           =
    {
      gp_colour    = Graphic.Grey num_zero;
      gp_bg_colour = Graphic.Grey num_one;
      gp_alpha     = num_zero
    }
};

value lines    galley   = List.rev (galley.glue @ galley.lines);
value get_line galley i = List.nth (galley.glue @ galley.lines) i;

value keep_lines galley lines =
{
  (galley)

  with

  lines = List.rev lines
};

value last_line galley = match galley.lines with
[ []     -> empty_box
| [b::_] -> b
];

value modify_glue galley f =
{
  (galley)

  with

  glue = f galley.glue
};

value measure                   galley = galley.measure;
value graphics_params           galley = galley.graphics_params;
value par_params                galley = galley.par_params;
value line_params               galley = galley.line_params;
value line_break_params         galley = galley.line_break_params;
value hyphen_params             galley = galley.hyphen_params;
value space_params              galley = galley.space_params;
value math_params               galley = galley.math_params;
value current_par_params        galley = galley.current_par_params;
value current_line_params       galley = galley.current_line_params;
value current_line_break_params galley = galley.current_line_break_params;
value current_hyphen_params     galley = galley.current_hyphen_params;
value current_space_params      galley = galley.current_space_params;
value current_math_params       galley = galley.current_math_params;

value set_line_params               galley p = { (galley) with line_params               = p };
value set_line_break_params         galley p = { (galley) with line_break_params         = p };
value set_hyphen_params             galley p = { (galley) with hyphen_params             = p };
value set_space_params              galley p = { (galley) with space_params              = p };
value set_math_params               galley p = { (galley) with math_params               = p };
value set_graphics_params           galley p = { (galley) with graphics_params           = p };
value set_current_line_params       galley p = { (galley) with current_line_params       = p };
value set_current_line_break_params galley p = { (galley) with current_line_break_params = p };
value set_current_hyphen_params     galley p = { (galley) with current_hyphen_params     = p };
value set_current_space_params      galley p = { (galley) with current_space_params      = p };
value set_current_math_params       galley p = { (galley) with current_math_params       = p };

value set_par_params galley p =
{
  (galley)

  with

  par_params = { (p) with ParLayout.measure = galley.measure }
};

value set_current_par_params galley p =
{
  (galley)

  with

  current_par_params = { (p) with ParLayout.measure = galley.measure }
};

value copy_params galley from_galley =
{
  (galley)

  with

  graphics_params           = from_galley.graphics_params;
  par_params                = { (from_galley.par_params)
                                with
                                ParLayout.measure = galley.measure };
  line_params               = from_galley.line_params;
  line_break_params         = from_galley.line_break_params;
  hyphen_params             = from_galley.hyphen_params;
  space_params              = from_galley.space_params;
  math_params               = from_galley.math_params;
  current_par_params        = { (from_galley.current_par_params)
                                with
                                ParLayout.measure = galley.measure };
  current_line_params       = from_galley.current_line_params;
  current_line_break_params = from_galley.current_line_break_params;
  current_hyphen_params     = from_galley.current_hyphen_params;
  current_space_params      = from_galley.current_space_params;
  current_math_params       = from_galley.current_math_params
};

value reset_params galley =
{
  (galley)

  with

  current_par_params        = { (galley.par_params)
                                with
                                ParLayout.measure = galley.measure };
  current_line_params       = galley.line_params;
  current_line_break_params = galley.line_break_params;
  current_hyphen_params     = galley.hyphen_params;
  current_space_params      = galley.space_params;
  current_math_params       = galley.math_params
};

(* add a line to the galley *)

value add_line galley line = do
{
  (* We need to keep track of graphic state changes. *)

  let update_gfx_cmds ((fg, bg, alpha) as gfx) c = match c with
  [ Graphic.SetColour   _ -> (Some c, bg, alpha)
  | Graphic.SetBgColour _ -> (fg, Some c, alpha)
  | Graphic.SetAlpha    _ -> (fg, bg, Some c)
  | _                     -> gfx
  ];

  (* |make_gfx_cmd_boxes <state>| returns a list of command boxes that sets the right graphics state. *)

  let make_gfx_cmd_boxes (fg, bg, alpha) = do
  {
    (match fg with
     [ Some c -> [new_command_box (`GfxCmd c)]
     | None   -> []
     ])
  @ (match bg with
     [ Some c -> [new_command_box (`GfxCmd c)]
     | None   -> []
     ])
  @ (match alpha with
     [ Some c -> [new_command_box (`GfxCmd c)]
     | None   -> []
     ])
  };

  let leading = match galley.lines with
                [ []     -> dim_zero
                | [b::_] -> galley.current_line_params.leading b line galley.current_line_params
                ];
  let gfx     = match line.b_contents with
                [ CompBox bs -> List.fold_left update_gfx_cmds (None, None, None) bs
                | _          -> (None, None, None)
                ];

  {
    (galley)

    with

    lines = [line;
             new_glue_box dim_zero leading True True
              :: galley.glue @ galley.lines];
    glue  = make_gfx_cmd_boxes gfx
  }
};

(* add glue and control boxes to the galley *)

value add_glue galley box =
{
  (galley)

  with

  glue = [box :: galley.glue]
};

(* add a paragraph to the galley *)

value add_paragraph galley loc items = do
{
  (* search for v-insert boxes *)

  let rec extract_inserts boxes result above below = match boxes with
  [ []      -> (ListBuilder.get result, List.rev above, List.rev below)
  | [b::bs] -> match b.b_contents with
    [ CommandBox (`ParCmd (VInsert below_flag contents)) ->
        if below_flag then
          extract_inserts bs result above (List.rev contents @ below)
        else
          extract_inserts bs result (List.rev contents @ above) below
    | _ -> do
      {
        ListBuilder.add result b;
        extract_inserts bs result above below
      }
    ]
  ];

  let lines =
    ParLayout.break_paragraph
      loc
      items
      galley.current_par_params
      galley.current_line_break_params
      galley.current_hyphen_params;

  (* Move this function to ParLayout? *)

  let rec box_lines result line_no boxes = match boxes with
  [ []      -> (line_no, ListBuilder.get result)
  | [b::bs] -> do
    {
      let (body, above, below) = extract_inserts b (ListBuilder.make ()) [] [];

      ListBuilder.add result
        (ParLayout.layout_line galley.measure line_no body galley.current_par_params, above, below);

      box_lines
       result
       (line_no + 1)
       bs
    }
  ];

  let insert_break galley penalty = do
  {
    add_glue galley (new_break_box penalty False [] [] [])
  };

  let insert_par_skip galley = do
  {
    add_glue
      (insert_break galley num_zero)
      (new_glue_box dim_zero galley.current_par_params.ParLayout.par_skip True True)
  };

  let rec insert_insertion galley boxes = match boxes with
  [ []      -> galley
  | [b::bs] -> insert_insertion (add_glue galley b) bs
  ];
  let add_line_and_insertions galley line above below = do
  {
    insert_insertion
      (add_line
        (insert_insertion galley above)
        line)
      below
  };

  match box_lines (ListBuilder.make ()) 0 lines with
  [ (_,   [])                           -> galley
  | (num, [(line, above, below) :: ls]) -> do
    {
      iter 1 (num-1) ls
           (add_line_and_insertions (insert_par_skip galley) line above below)

      where rec iter lines_above lines_below lines galley = match lines with
      [ []                           -> galley
      | [(line, above, below) :: ls] ->
          iter (lines_above + 1) (lines_below - 1) ls
               (add_line_and_insertions
                 (insert_break
                   galley
                   (galley.current_line_params.club_widow_penalty lines_above lines_below))
                 line above below)
      ]
    }
  ]
};

(*
  |put_in_vbox <galley>| and |put_in_vtop <galley>| return a box containing the entire material of the
  <galley>.
*)

value put_in_vbox galley = do
{
  VBox.make (List.rev galley.lines)
};

value put_in_vtop galley = do
{
  VBox.make_top (List.rev galley.lines)
};


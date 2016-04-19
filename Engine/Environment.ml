
open XNum;
open Runtime;
open Logging;
open Unicode;
open Types;
open Dim;
open Typesetting;
open Box;
open Fonts;

type font_data =
{
  fd_font               : font;
  fd_composer           : glyph_composer;
  fd_math_fonts         : array (font * font * font);
  fd_math_font_params   : MathLayout.math_font_params;
  fd_script_size        : num;
  fd_script_script_size : num;
  fd_script_lang        : uc_string;
  fd_features           : SymbolSet.t
};

(* the environment *)

type environment =
{
  (* stack to save and restore environemnts *)

  old_environment : environment;

  (* tables: galleys, page layouts *)

  galleys         : PTable.table Galley.galley;
  page_layouts    : PTable.table PageLayout.page_layout;

  float_misplacement_demerits : num;

  (* finished pages *)

  page_no      : int;
  pages        : list FontMetric.page;

  (* math *)

  math_style   : MathLayout.math_style;

  (* fonts *)

  font_table   : font_table;
  font_data    : font_data
};

type env_cmd = UCStream.location -> environment -> environment;

type skip_arg = environment -> num;
type dim_arg  = environment -> dim;

type line_param_arg =
  (dim_arg * skip_arg * dim_arg * (box -> box -> Galley.line_params -> dim) * (int -> int -> num));
type par_param_arg  =
  (num * dim_arg * dim_arg * dim_arg * dim_arg * (environment -> int -> (num * num)) * dim_arg *
   (environment -> list extended_glyph_item) *
   (environment -> list extended_glyph_item) *
   (environment -> list box -> list box));
type line_break_param_arg = (num * num * int * num * num * num * num * skip_arg * num * skip_arg * bool);
type hyphen_param_arg = (uc_string * num * num * int * int * uc_string);
type space_param_arg = (num * option dim_arg * option dim_arg * bool);
type math_param_arg =
  (dim_arg * dim_arg * dim_arg * dim_arg * num * num * num * skip_arg * dim_arg);

type par_param_modifier  =
  (option num * option dim_arg * option dim_arg * option dim_arg * option dim_arg *
   option (environment -> int -> (num * num)) * option dim_arg *
   option (environment -> list extended_glyph_item) *
   option (environment -> list extended_glyph_item) *
   option (environment -> list box -> list box));
type line_param_modifier =
  (option dim_arg * option skip_arg * option dim_arg * option (box -> box -> Galley.line_params -> dim) *
   option (int -> int -> num));
type line_break_param_modifier =
  (option num * option num * option int * option num * option num * option num *
   option num * option skip_arg * option num * option skip_arg * option bool);
type hyphen_param_modifier =
  (option uc_string * option num * option num * option int * option int * option uc_string);
type space_param_modifier =
  (option num * option dim_arg * option dim_arg * option bool);
type math_param_modifier =
  (option dim_arg * option dim_arg * option dim_arg * option dim_arg *
  option num * option num * option num * option skip_arg * option dim_arg);

type font_spec = (option uc_string * option uc_string * option uc_string * option num * option uc_string * option (list uc_string));

(* accessors *)

value galley_table                        env = env.galleys;
value current_galley                      env = PTable.current env.galleys;
value page_layout_table                   env = env.page_layouts;
value current_page_layout                 env = PTable.current env.page_layouts;
value current_page_number                 env = env.page_no;
value current_float_misplacement_demerits env = env.float_misplacement_demerits;
value current_math_style                  env = env.math_style;
value current_font                        env = env.font_data.fd_font;
value current_math_fonts                  env = env.font_data.fd_math_fonts;
value current_math_font_params            env = env.font_data.fd_math_font_params;
value current_script_size                 env = env.font_data.fd_script_size;
value current_script_script_size          env = env.font_data.fd_script_script_size;
value current_font_metric                 env = env.font_data.fd_font.f_metric;
value current_composer                    env = env.font_data.fd_composer;
value get_pages                           env = env.pages;

value set_math_style env math_style = { (env) with math_style = math_style };

value sync_tables env =
{
  (env)

  with

  galleys      = PTable.sync env.galleys;
  page_layouts = PTable.sync env.page_layouts
};

value save_environment env =
{
  (env)

  with

  old_environment = env
};

value restore_environment env = do
{
  let old_env = sync_tables env.old_environment;

  let restore name galley = do
  {
    try
      let old_galley = DynUCTrie.find_string name (PTable.table old_env.galleys);

      let g = Galley.copy_params galley old_galley;
      g
    with
    [ Not_found -> galley ]
  };
  let new_galley_table = PTable.select
                           (PTable.mapi env.galleys restore)
                           (PTable.key old_env.galleys);
  {
    (old_env)

    with

    galleys      = new_galley_table;
    font_table   = env.font_table;
    page_layouts = env.page_layouts;
    page_no      = env.page_no;
    pages        = env.pages
  }
};

(* hyphen tables *)

value get_hyphen_table loc language = do
{
  try
    List.assoc language HyphenTable.tables
  with
  [ Not_found -> do
    {
      log_warn loc "Hyphenation rules for language `";
      log_uc_string language;
      log_string "' not found!";
      snd (List.hd HyphenTable.tables)
    }
  ]
};

(* galleys *)

value new_galley name measure _ env = do
{
  let cur_galley = current_galley env;
  let new_galley = Galley.new_galley
                     measure
                     (Galley.line_params       cur_galley)
                     (Galley.par_params        cur_galley)
                     (Galley.line_break_params cur_galley)
                     (Galley.hyphen_params     cur_galley)
                     (Galley.space_params      cur_galley)
                     (Galley.math_params       cur_galley);
  {
    (env)

    with

    galleys = PTable.add env.galleys name new_galley
  }
};

value select_galley name loc env = do
{
  try
    {
      (env)

      with

      galleys = PTable.select env.galleys name
    }
  with
  [ Not_found -> do
    {
      log_warn loc "Unknown galley ";
      log_uc_string name;
      log_string "!";

      env
    }
  ]
};

(* modifies the current galley *)

value set_galley_table env galleys = { (env) with galleys = galleys };

value set_galley galley _ env = set_galley_table env (PTable.set env.galleys galley);

value modify_par_params
  (measure, par_indent, par_fill_skip, left_skip, right_skip, par_shape, par_skip, pre_break, post_break, post_process_line)
  env p =
{
  ParLayout.measure = match measure with
    [ Some x -> x
    | None   -> p.ParLayout.measure
    ];
  ParLayout.par_indent = match par_indent with
    [ Some x -> x env
    | None   -> p.ParLayout.par_indent
    ];
  ParLayout.par_fill_skip = match par_fill_skip with
    [ Some x -> x env
    | None   -> p.ParLayout.par_fill_skip
    ];
  ParLayout.left_skip = match left_skip with
    [ Some x -> x env
    | None   -> p.ParLayout.left_skip
    ];
  ParLayout.right_skip = match right_skip with
    [ Some x -> x env
    | None   -> p.ParLayout.right_skip
    ];
  ParLayout.par_shape = match par_shape with
    [ Some x -> x env
    | None   -> p.ParLayout.par_shape
    ];
  ParLayout.par_skip = match par_skip with
    [ Some x -> x env
    | None   -> p.ParLayout.par_skip
    ];
  ParLayout.pre_break = match pre_break with
    [ Some x -> x env
    | None   -> p.ParLayout.pre_break
    ];
  ParLayout.post_break = match post_break with
    [ Some x -> x env
    | None   -> p.ParLayout.post_break
    ];
  ParLayout.post_process_line = match post_process_line with
    [ Some x -> x env
    | None   -> p.ParLayout.post_process_line
    ]
};

value set_par_params params loc env = do
{
  let galley = current_galley env;
  let p      = Galley.par_params         galley;
  let cp     = Galley.current_par_params galley;

  set_galley
    (Galley.set_current_par_params
      (Galley.set_par_params
        galley
        (modify_par_params params env p))
      (modify_par_params params env cp))
  loc
  env
};

value set_current_par_params params loc env = do
{
  let galley = current_galley env;
  let cp     = Galley.current_par_params galley;

  set_galley
    (Galley.set_current_par_params
      galley
      (modify_par_params params env cp))
  loc
  env
};

value modify_line_params
  (baseline_skip, line_skip_limit, line_skip, leading, club_widow_penalty)
  env l =
{
  Galley.baseline_skip = match baseline_skip with
    [ Some x -> x env
    | None   -> l.Galley.baseline_skip
    ];
  Galley.line_skip_limit = match line_skip_limit with
    [ Some x -> x env
    | None   -> l.Galley.line_skip_limit
    ];
  Galley.line_skip = match line_skip with
    [ Some x -> x env
    | None   -> l.Galley.line_skip
    ];
  Galley.leading = match leading with
    [ Some x -> x
    | None   -> l.Galley.leading
    ];
  Galley.club_widow_penalty = match club_widow_penalty with
    [ Some x -> x
    | None   -> l.Galley.club_widow_penalty
    ]
};

value set_line_params params loc env = do
{
  let galley = current_galley env;
  let l      = Galley.line_params         galley;
  let cl     = Galley.current_line_params galley;

  set_galley
    (Galley.set_current_line_params
      (Galley.set_line_params
        galley
        (modify_line_params params env l))
      (modify_line_params params env cl))
  loc
  env
};

value set_current_line_params params loc env = do
{
  let galley = current_galley env;
  let cl     = Galley.current_line_params galley;

  set_galley
    (Galley.set_current_line_params
      galley
      (modify_line_params params env cl))
  loc
  env
};

value modify_line_break_params
  (pre_tolerance, tolerance, looseness, line_penalty, adj_demerits,
   double_hyphen_demerits, final_hyphen_demerits, emergency_stretch,
   river_demerits, river_threshold, simple_breaking)
  env l =
{
  ParLayout.pre_tolerance = match pre_tolerance with
    [ Some x -> x
    | None   -> l.ParLayout.pre_tolerance
    ];
  ParLayout.tolerance = match tolerance with
    [ Some x -> x
    | None   -> l.ParLayout.tolerance
    ];
  ParLayout.looseness = match looseness with
    [ Some x -> x
    | None   -> l.ParLayout.looseness
    ];
  ParLayout.line_penalty = match line_penalty with
    [ Some x -> x
    | None   -> l.ParLayout.line_penalty
    ];
  ParLayout.adj_demerits = match adj_demerits with
    [ Some x -> x
    | None   -> l.ParLayout.adj_demerits
    ];
  ParLayout.double_hyphen_demerits = match double_hyphen_demerits with
    [ Some x -> x
    | None   -> l.ParLayout.double_hyphen_demerits
    ];
  ParLayout.final_hyphen_demerits = match final_hyphen_demerits with
    [ Some x -> x
    | None   -> l.ParLayout.final_hyphen_demerits
    ];
  ParLayout.emergency_stretch = match emergency_stretch with
    [ Some x -> x env
    | None   -> l.ParLayout.emergency_stretch
    ];
  ParLayout.river_demerits = match river_demerits with
    [ Some x -> x
    | None   -> l.ParLayout.river_demerits
    ];
  ParLayout.river_threshold = match river_threshold with
    [ Some x -> x env
    | None   -> l.ParLayout.river_threshold
    ];
  ParLayout.simple_breaking = match simple_breaking with
    [ Some x -> x
    | None   -> l.ParLayout.simple_breaking
    ]
};

value set_line_break_params params loc env = do
{
  let galley = current_galley env;
  let l      = Galley.line_break_params         galley;
  let cl     = Galley.current_line_break_params galley;

  set_galley
    (Galley.set_current_line_break_params
      (Galley.set_line_break_params
        galley
        (modify_line_break_params params env l))
      (modify_line_break_params params env cl))
  loc
  env
};

value set_current_line_break_params params loc env = do
{
  let galley = current_galley env;
  let cl     = Galley.current_line_break_params galley;

  set_galley
    (Galley.set_current_line_break_params
      galley
      (modify_line_break_params params env cl))
  loc
  env
};

value modify_hyphen_params
  (hyphen_table, hyphen_penalty, ex_hyphen_penalty,
   left_hyphen_min, right_hyphen_min, script_lang)
  loc h =
{
  JustHyph.hyphen_table = match hyphen_table with
    [ Some x -> get_hyphen_table loc x
    | None   -> h.JustHyph.hyphen_table
    ];
  JustHyph.hyphen_penalty = match hyphen_penalty with
    [ Some x -> x
    | None   -> h.JustHyph.hyphen_penalty
    ];
  JustHyph.ex_hyphen_penalty = match ex_hyphen_penalty with
    [ Some x -> x
    | None   -> h.JustHyph.ex_hyphen_penalty
    ];
  JustHyph.left_hyphen_min = match left_hyphen_min with
    [ Some x -> x
    | None   -> h.JustHyph.left_hyphen_min
    ];
  JustHyph.right_hyphen_min = match right_hyphen_min with
    [ Some x -> x
    | None   -> h.JustHyph.right_hyphen_min
    ];
  JustHyph.script_lang = match script_lang with
    [ Some x -> x
    | None   -> h.JustHyph.script_lang
    ]
};

value set_hyphen_params params loc env = do
{
  let galley = current_galley env;
  let h      = Galley.hyphen_params         galley;
  let ch     = Galley.current_hyphen_params galley;

  set_galley
    (Galley.set_current_hyphen_params
      (Galley.set_hyphen_params
        galley
        (modify_hyphen_params params loc h))
      (modify_hyphen_params params loc ch))
  loc
  env
};

value set_current_hyphen_params params loc env = do
{
  let galley = current_galley env;
  let ch     = Galley.current_hyphen_params galley;

  set_galley
    (Galley.set_current_hyphen_params
      galley
      (modify_hyphen_params params loc ch))
  loc
  env
};

value modify_space_params
  (space_factor, space_skip, xspace_skip, victorian_spacing)
  env s =
{
  Galley.space_factor = match space_factor with
    [ Some x -> x
    | None   -> s.Galley.space_factor
    ];
  Galley.space_skip = match space_skip with
    [ Some x -> Some (x env)
    | None   -> s.Galley.space_skip
    ];
  Galley.xspace_skip = match xspace_skip with
    [ Some x -> Some (x env)
    | None   -> s.Galley.xspace_skip
    ];
  Galley.victorian_spacing = match victorian_spacing with
    [ Some x -> x
    | None   -> s.Galley.victorian_spacing
    ]
};

value set_space_params params loc env = do
{
  let galley = current_galley env;
  let s      = Galley.space_params         galley;
  let cs     = Galley.current_space_params galley;

  set_galley
    (Galley.set_current_space_params
      (Galley.set_space_params
        galley
        (modify_space_params params env s))
      (modify_space_params params env cs))
  loc
  env
};

value set_current_space_params params loc env = do
{
  let galley = current_galley env;
  let cs     = Galley.current_space_params galley;

  set_galley
    (Galley.set_current_space_params
      galley
      (modify_space_params params env cs))
  loc
  env
};

value modify_math_params
  (thin_math_skip, med_math_skip, thick_math_skip, script_space, rel_penalty, binop_penalty,
  delimiter_factor, delimiter_shortfall, null_delimiter_space)
  env m =
{
  MathLayout.thin_math_skip = match thin_math_skip with
    [ Some x -> x env
    | None   -> m.MathLayout.thin_math_skip
    ];
  MathLayout.med_math_skip = match med_math_skip with
    [ Some x -> x env
    | None   -> m.MathLayout.med_math_skip
    ];
  MathLayout.thick_math_skip = match thick_math_skip with
    [ Some x -> x env
    | None   -> m.MathLayout.thick_math_skip
    ];
  MathLayout.script_space = match script_space with
    [ Some x -> x env
    | None   -> m.MathLayout.script_space
    ];
  MathLayout.rel_penalty = match rel_penalty with
    [ Some x -> x
    | None   -> m.MathLayout.rel_penalty
    ];
  MathLayout.binop_penalty = match binop_penalty with
    [ Some x -> x
    | None   -> m.MathLayout.binop_penalty
    ];
  MathLayout.delimiter_factor = match delimiter_factor with
    [ Some x -> x
    | None   -> m.MathLayout.delimiter_factor
    ];
  MathLayout.delimiter_shortfall = match delimiter_shortfall with
    [ Some x -> x env
    | None   -> m.MathLayout.delimiter_shortfall
    ];
  MathLayout.null_delimiter_space = match null_delimiter_space with
    [ Some x -> x env
    | None   -> m.MathLayout.null_delimiter_space
    ]
};

value set_math_params params loc env = do
{
  let galley = current_galley env;
  let m      = Galley.math_params         galley;
  let cm     = Galley.current_math_params galley;

  set_galley
    (Galley.set_current_math_params
      (Galley.set_math_params
        galley
        (modify_math_params params env m))
      (modify_math_params params env cm))
  loc
  env
};

value set_current_math_params params loc env = do
{
  let galley = current_galley env;
  let cm     = Galley.current_math_params galley;

  set_galley
    (Galley.set_current_math_params
      galley
      (modify_math_params params env cm))
  loc
  env
};

value set_colour c loc env = do
{
  set_galley
    (Galley.set_graphics_params
      (current_galley env)
      {
        (Galley.graphics_params (current_galley env))

        with

        Galley.gp_colour = c
      })
  loc
  env
};

(*
  |adjust_graphics_state <old_env> <new_env>| returns a list of graphics commands to turn
  the graphics state of <old_env> into the one of <new_env>.
*)

value adjust_graphics_state old_env new_env = do
{
  let galley_name = PTable.key new_env.galleys;
  let new_gfx = Galley.graphics_params (current_galley new_env);
  let old_gfx = Galley.graphics_params (PTable.get old_env.galleys galley_name);

    (if Graphic.compare_colour old_gfx.Galley.gp_colour new_gfx.Galley.gp_colour <> 0 then
       [new_command_box (`GfxCmd (Graphic.SetColour new_gfx.Galley.gp_colour))]
     else
       [])
  @ (if Graphic.compare_colour old_gfx.Galley.gp_bg_colour new_gfx.Galley.gp_bg_colour <> 0 then
       [new_command_box (`GfxCmd (Graphic.SetBgColour new_gfx.Galley.gp_bg_colour))]
     else
       [])
  @ (if old_gfx.Galley.gp_alpha <>/ new_gfx.Galley.gp_alpha then
       [new_command_box (`GfxCmd (Graphic.SetAlpha new_gfx.Galley.gp_alpha))]
     else
       [])
};

(* page_layout *)


value new_page_layout name width height _ env = do
{
  let new_layout = {
                     PageLayout.pl_width  = width;
                     PageLayout.pl_height = height;
                     PageLayout.pl_areas  = [| |]
                   };
  {
    (env)

    with

    page_layouts = PTable.add env.page_layouts name new_layout
  }
};

value select_page_layout name loc env = do
{
  try
    {
      (env)

      with

      page_layouts = PTable.select env.page_layouts name
    }
  with
  [ Not_found -> do
    {
      log_warn loc "Unknown page layout ";
      log_uc_string name;
      log_string "!";

      env
    }
  ]
};

value set_page_layout layout _ env =
{
  (env)

  with

  page_layouts = PTable.set env.page_layouts layout
};

(* pages *)

(*
  |add_pages <env> <page-no> <pages> adds <pages> to the list of pages and sets the page number to
  <page-no>.
*)

value add_pages page_no pages _ env =
{
  (env)

  with

  page_no = page_no;
  pages   = env.pages @ pages
};

(* fonts *)

value declare_font name family series shape size params _ env =
{
  (env)

  with

  font_table =
    Fonts.declare_font
      env.font_table
      name
      family
      series
      shape
      size
      params
};

value set_font (family, series, shape, size, script, features) loc env = do
{
  let cur_font = current_font env;

  let fam = match family with
            [ Some x -> x
            | None   -> cur_font.f_font_def.fd_family
            ]
  and ser = match series with
            [ Some x -> x
            | None   -> cur_font.f_font_def.fd_series
            ]
  and sha = match shape with
            [ Some x -> x
            | None   -> cur_font.f_font_def.fd_shape
            ]
  and siz = match size with
            [ Some x -> x
            | None   -> cur_font.f_size
            ]
  and scr = match script with
            [ Some x -> x
            | None   -> env.font_data.fd_script_lang
            ]
  and fea = match features with
            [ Some x -> List.fold_left
                          (fun set f -> SymbolSet.add (SymbolTable.string_to_symbol f) set)
                          SymbolSet.empty
                          x
            | None   -> env.font_data.fd_features
            ];

  match get_font env.font_table fam ser sha siz with
  [ None -> do
    {
      log_warn loc "Font (";
      log_uc_string fam;
      log_string "/";
      log_uc_string ser;
      log_string "/";
      log_uc_string sha;
      log_string "/";
      log_num siz;
      log_string ") not found!";

      (* suppress further warnings by binding the font to some random value *)
      (* FIX: choose the "best approximation" instead of the current font   *)

      {
        (env)

        with

        font_table =
          Fonts.declare_font
            env.font_table
            cur_font.f_font_def.fd_name
            fam ser sha (siz, siz)
            cur_font.f_font_def.fd_data
      }
    }
  | Some f ->
    {
      (env)

      with

      font_data = { (env.font_data)
                    with
                    fd_font        = f;
                    fd_composer    = FontMetric.get_glyph_composer f.f_metric scr fea;
                    fd_script_lang = scr;
                    fd_features    = fea
                  }
    }
  ]
};

(* Checks whether <family> is within range. *)

value check_math_family env family = do
{
  if family >= Array.length env.font_data.fd_math_fonts then
    0
  else
    family
};

value scale_font font_table loc font size = do
{
  match get_font font_table font.f_font_def.fd_family font.f_font_def.fd_series font.f_font_def.fd_shape size with
  [ None -> do
    {
      log_warn loc "Font (";
      log_uc_string font.f_font_def.fd_family;
      log_string "/";
      log_uc_string font.f_font_def.fd_series;
      log_string "/";
      log_uc_string font.f_font_def.fd_shape;
      log_string "/";
      log_num size;
      log_string ") not found!";
      font
    }
  | Some f -> f
  ]
};

(* |get_math_font <env> <style> <family>| returns the font number corresponding to <style> and <family>. *)

value get_math_font env style family = do
{
  let (t, s, ss) = env.font_data.fd_math_fonts.(check_math_family env family);

  match style with
  [ MathLayout.Display | MathLayout.CrampedDisplay
  | MathLayout.Text    | MathLayout.CrampedText    -> t.f_metric
  | MathLayout.Script  | MathLayout.CrampedScript  -> s.f_metric
  | MathLayout.Script2 | MathLayout.CrampedScript2 -> ss.f_metric
  ]
};

value set_math_font (math_family, family, series, shape, text_size, script_size, script2_size) loc env = do
{
  let (old_t_font, _, _) = env.font_data.fd_math_fonts.(0);
  let old_t  = old_t_font.f_size;
  let old_s1 = current_script_size env;
  let old_s2 = current_script_script_size env;

  let (s1, s2) = match (text_size, script_size, script2_size) with
  [ (Some t, Some s, Some ss) -> (s // t,     ss // t    )
  | (Some t, Some s, None   ) -> (s // t,     old_s2     )
  | (Some t, None,   Some ss) -> (old_s1,     ss // t    )
  | (None,   Some s, Some ss) -> (s // old_t, ss // old_t)
  | (None,   Some s, None   ) -> (s // old_t, old_s2     )
  | (None,   None,   Some ss) -> (old_s1,     ss // old_t)
  | (Some _, None,   None   ) -> (old_s1,     old_s2     )
  | (None,   None,   None   ) -> (old_s1,     old_s2     )
  ];

  let get_fonts mfam = do
  {
    let (old_font, _, _) = env.font_data.fd_math_fonts.(check_math_family env mfam);

    let fam = match family with
              [ Some x -> x
              | None   -> old_font.f_font_def.fd_family
              ]
    and ser = match series with
              [ Some x -> x
              | None   -> old_font.f_font_def.fd_series
              ]
    and sha = match shape with
              [ Some x -> x
              | None   -> old_font.f_font_def.fd_shape
              ]
    and siz = match text_size with
              [ Some x -> x
              | None   -> old_font.f_size
              ];

    let text_font = match get_font env.font_table fam ser sha siz with
    [ None -> do
      {
        log_warn loc "Font (";
        log_uc_string fam;
        log_string "/";
        log_uc_string ser;
        log_string "/";
        log_uc_string sha;
        log_string "/";
        log_num siz;
        log_string ") not found!";

        let (t,_,_) = env.font_data.fd_math_fonts.(check_math_family env mfam);

        t
      }
    | Some f -> f
    ];

    (text_font,
     scale_font env.font_table loc text_font (siz */ s1),
     scale_font env.font_table loc text_font (siz */ s2))
  };

  (* update math-font array *)

  let new_math_fonts = match math_family with
  [ Some mf -> Array.init
                 (max (mf + 1) (Array.length env.font_data.fd_math_fonts))
                 (fun f ->
                   if f = mf then
                     get_fonts mf
                   else
                     env.font_data.fd_math_fonts.(check_math_family env f)
                 )
  | None    -> Array.init
                 (Array.length env.font_data.fd_math_fonts)
                 get_fonts
  ];

  (* update math-font parameter *)

  let (font_sym_t, font_sym_s, font_sym_ss) =
    if Array.length new_math_fonts > 2 then do
    {
      let (t,s,ss) = new_math_fonts.(2);
      (t.f_metric, s.f_metric, ss.f_metric)
    }
    else
      (FontMetric.empty_font, FontMetric.empty_font, FontMetric.empty_font);
  let font_ex =
    if Array.length new_math_fonts > 3 then do
    {
      let (t,_,_) = new_math_fonts.(3);
      t.f_metric
    }
    else
      FontMetric.empty_font;

  {
    (env)

    with

    font_data =
    {
      (env.font_data)

      with

      fd_math_fonts         = new_math_fonts;
      fd_math_font_params   =
      (
        MathLayout.make_font_params font_sym_t  font_ex,
        MathLayout.make_font_params font_sym_s  font_ex,
        MathLayout.make_font_params font_sym_ss font_ex
      );
      fd_script_size        = s1;
      fd_script_script_size = s2
    }
  }
};

value adapt_fonts_to_math_style loc env = match env.math_style with
[ MathLayout.Script  | MathLayout.CrampedScript  -> do
  {
    let new_font =
      scale_font
        env.font_table
        loc
        (current_font env)
        (current_script_size env */ (current_font env).f_size)
    and new_math_fonts =
      Array.map
        (fun (_,s,ss) -> (s,ss,ss))
        (current_math_fonts env);

    { (env)

      with

      font_data =
        {
          fd_font               = new_font;
          fd_composer           = FontMetric.get_glyph_composer
                                    new_font.f_metric
                                    env.font_data.fd_script_lang
                                    env.font_data.fd_features;
          fd_math_fonts         = new_math_fonts;
          fd_script_size        = current_script_script_size env // current_script_size env;
          fd_script_script_size = current_script_script_size env // current_script_size env;
          fd_math_font_params   = current_math_font_params env;
          fd_script_lang        = env.font_data.fd_script_lang;
          fd_features           = env.font_data.fd_features
        }
    }
  }
| MathLayout.Script2 | MathLayout.CrampedScript2 -> do
  {
    let new_font =
      scale_font
        env.font_table
        loc
        (current_font env)
        (current_script_script_size env */ (current_font env).f_size)
    and new_math_fonts =
      Array.map
        (fun (_,_,ss) -> (ss,ss,ss))
        (current_math_fonts env);

    { (env)

      with

      font_data =
        {
          fd_font               = new_font;
          fd_composer           = FontMetric.get_glyph_composer
                                    new_font.f_metric
                                    env.font_data.fd_script_lang
                                    env.font_data.fd_features;
          fd_math_fonts         = new_math_fonts;
          fd_script_size        = num_one;
          fd_script_script_size = num_one;
          fd_math_font_params   = current_math_font_params env;
          fd_script_lang        = env.font_data.fd_script_lang;
          fd_features           = env.font_data.fd_features
        }
    }
  }
| _ -> env
];

value set_space_factor env sf = do
{
  let galley = current_galley env;
  let cs     = Galley.current_space_params galley;

  set_galley
    (Galley.set_current_space_params
      galley
      { (cs) with Galley.space_factor = sf })
  ("", 0)
  env
};

(* hard-wired space factors of characters *)
(* FIX: use charmap *)

value get_space_factor env char = do
{
  if not (Galley.current_space_params (current_galley env)).Galley.victorian_spacing then
    num_one
  else if char >= 65 && char <= 90 then          (* upper case characters *)
    num_of_ints 999 1000
  else if char >= 128 && char <= 156 then
    num_of_ints 999 1000
  else if char >= 192 && char <= 223 then
    num_of_ints 999 1000
  else if char = 41 || char = 39 || char = 93 then   (* ) ' ] *)
    num_zero
  else match char with
  [ 46 | 33 | 63 -> num_of_int 3         (* . ? ! *)
  | 58           -> num_of_int 2         (* : *)
  | 59           -> num_of_ints  15  10  (* ; *)
  | 44           -> num_of_ints 125 100  (* , *)
  | _            -> num_one
  ]
};

(* set the space factor according to the given character *)

value adjust_space_factor char _ env = do
{
  let sf = get_space_factor env char;

  if sf = num_one then
    set_space_factor env num_one
  else if sf </ num_one then do
  {
    if sf >/ num_zero then
      set_space_factor env sf
    else
      env
  }
  else if (Galley.current_space_params (current_galley env)).Galley.space_factor </ num_one then
    set_space_factor env num_one
  else
    set_space_factor env sf
};


(* initialisation *)

value initialise_environment () = do
{
  let par_params =
    {
      ParLayout.measure           = num_of_int 345;
      ParLayout.par_indent        = dim_12pt;
      ParLayout.par_fill_skip     = dim_fil;
      ParLayout.par_skip          = dim_zero;
      ParLayout.left_skip         = dim_zero;
      ParLayout.right_skip        = dim_zero;
      ParLayout.par_shape         = fun _ -> (num_zero, num_zero);
      ParLayout.pre_break         = [];
      ParLayout.post_break        = [];
      ParLayout.post_process_line = fun l -> l
    };
  let line_params =
    {
      Galley.baseline_skip      = dim_12pt;
      Galley.line_skip_limit    = num_zero;
      Galley.line_skip          = dim_1pt;
      Galley.leading            = Galley.leading_skyline;
      Galley.club_widow_penalty = fun _ k -> if k > 1 then
                                    num_zero
                                  else
                                    num_of_int 1000
    };
  let line_break_params =
    {
      ParLayout.pre_tolerance          = num_of_int   100;
      ParLayout.tolerance              = num_of_int   200;
      ParLayout.looseness              =                0;
      ParLayout.line_penalty           = num_of_int    10;
      ParLayout.adj_demerits           = num_of_int 10000;
      ParLayout.double_hyphen_demerits = num_of_int 10000;
      ParLayout.final_hyphen_demerits  = num_of_int  5000;
      ParLayout.emergency_stretch      = num_zero;
      ParLayout.river_demerits         = num_zero;
      ParLayout.river_threshold        = num_one;
      ParLayout.simple_breaking        = True
    };
  let hyphen_params =
    {
      JustHyph.hyphen_table      = try
                                     List.assoc
                                       (UString.uc_string_of_ascii "british")
                                       HyphenTable.tables
                                   with
                                   [ Not_found -> do
                                     {
                                       log_string "\nError: British hyphenation patterns not found!";
                                       exit 1
                                     }
                                   ];
      JustHyph.hyphen_penalty    = num_of_int 50;
      JustHyph.ex_hyphen_penalty = num_of_int 50;
      JustHyph.left_hyphen_min   = 2;
      JustHyph.right_hyphen_min  = 3;
      JustHyph.script_lang       = [||]
    };
  let space_params =
    {
      Galley.space_factor      = num_of_int 1;
      Galley.space_skip        = None;
      Galley.xspace_skip       = None;
      Galley.victorian_spacing = False
    };
  let math_params =
    {
      MathLayout.thin_math_skip  = fixed_dim (num_of_int 3);
      MathLayout.med_math_skip   = {
                                     d_base           = num_of_int 4;
                                     d_stretch_factor = num_of_int 2;
                                     d_stretch_order  = 0;
                                     d_shrink_factor  = num_of_int 4;
                                     d_shrink_order   = 0
                                   };
      MathLayout.thick_math_skip = {
                                     d_base           = num_of_int 5;
                                     d_stretch_factor = num_of_int 5;
                                     d_stretch_order  = 0;
                                     d_shrink_factor  = num_zero;
                                     d_shrink_order   = 0
                                   };
      MathLayout.script_space         = fixed_dim (num_of_ints 1 2);
      MathLayout.rel_penalty          = num_of_int 500;
      MathLayout.binop_penalty        = num_of_int 700;
      MathLayout.delimiter_factor     = num_of_ints 901 1000;
      MathLayout.delimiter_shortfall  = num_of_int 5;
      MathLayout.null_delimiter_space = fixed_dim (num_of_ints 12 10)
    };
  let main_galley =
    Galley.new_galley
      (num_of_int 345)
      line_params
      par_params
      line_break_params
      hyphen_params
      space_params
      math_params;
  let text_area =
    {
      PageLayout.ar_name  = UString.uc_string_of_ascii "main";
      PageLayout.ar_shape =
        {
          Page.as_pos_x    = num_of_ints 7227 100;
          Page.as_pos_y    = num_of_ints 8227 100;
          Page.as_width    = num_of_int   345;
          Page.as_height   = num_of_int   643;
          Page.as_top      = num_of_int    10;
          Page.as_bottom   = num_of_int     3
        };
      PageLayout.ar_contents =
        AreaGalley.contents_from_galley
          {
            AreaGalley.galley      = UString.uc_string_of_ascii "main";
            AreaGalley.top_skip    = num_of_int 10;
            AreaGalley.bottom_skip = num_of_int 10;
            AreaGalley.min_size    = num_of_int 60;
            AreaGalley.grid_size   = num_of_int 12
          }
    };
  let default_layout =
    {
      PageLayout.pl_width  = num_of_int 210 */ num_of_ints 7227 2540;
      PageLayout.pl_height = num_of_int 297 */ num_of_ints 7227 2540;
      PageLayout.pl_areas  = [| text_area |]
    };
  let font_table          = initialise_font_table ();
  let family_roman        = [| |]; (*UString.uc_string_of_ascii "Computer Modern Roman"*)
  let family_math         = [| |]; (*UString.uc_string_of_ascii "Computer Modern Math Italic"*)
  let family_symbols      = [| |]; (*UString.uc_string_of_ascii "Computer Modern Math Symbols"*)
  let family_extensions   = [| |]; (*UString.uc_string_of_ascii "Computer Modern Math Extensions"*)
  let series_medium       = [| |]; (*UString.uc_string_of_ascii "medium"*)
  let shape_upright       = [| |]; (*UString.uc_string_of_ascii "normal"*)
  let shape_italic        = [| |]; (*UString.uc_string_of_ascii "italic"*)
  let size_normal         = num_of_int 10;
  let size_script         = num_of_int  7;
  let size_tiny           = num_of_int  5;

  let lookup_font fam ser sha siz = Option.from_some (get_font font_table fam ser sha siz);

  try
    let text_font       = lookup_font family_roman      series_medium shape_upright size_normal;
    let math_font_ex    = lookup_font family_extensions series_medium shape_upright size_normal;
    let math_font_rm_t  = lookup_font family_roman      series_medium shape_upright size_normal;
    let math_font_mi_t  = lookup_font family_math       series_medium shape_italic  size_normal;
    let math_font_sy_t  = lookup_font family_symbols    series_medium shape_upright size_normal;
    let math_font_rm_s  = lookup_font family_roman      series_medium shape_upright size_script;
    let math_font_mi_s  = lookup_font family_math       series_medium shape_italic  size_script;
    let math_font_sy_s  = lookup_font family_symbols    series_medium shape_upright size_script;
    let math_font_rm_ss = lookup_font family_roman      series_medium shape_upright size_tiny;
    let math_font_mi_ss = lookup_font family_math       series_medium shape_italic  size_tiny;
    let math_font_sy_ss = lookup_font family_symbols    series_medium shape_upright size_tiny;
    let std_features = SymbolSet.add (SymbolTable.string_to_symbol (UString.uc_string_of_ascii "liga"))
                         (SymbolSet.add (SymbolTable.string_to_symbol (UString.uc_string_of_ascii "kern"))
                           SymbolSet.empty);
    let font_data =
    {
      fd_font       = text_font;
      fd_composer   = FontMetric.get_glyph_composer
                        text_font.f_metric
                        (UString.uc_string_of_ascii "latn")
                        std_features;
      fd_math_fonts =
        [|
          (math_font_rm_t, math_font_rm_s, math_font_rm_ss);
          (math_font_mi_t, math_font_mi_s, math_font_mi_ss);
          (math_font_sy_t, math_font_sy_s, math_font_sy_ss);
          (math_font_ex,   math_font_ex,   math_font_ex)
        |];
      fd_math_font_params =
      (
        MathLayout.make_font_params math_font_sy_t.f_metric  math_font_ex.f_metric,
        MathLayout.make_font_params math_font_sy_s.f_metric  math_font_ex.f_metric,
        MathLayout.make_font_params math_font_sy_ss.f_metric math_font_ex.f_metric
      );
      fd_script_size        = num_of_ints 7 10;
      fd_script_script_size = num_of_ints 5 10;
      fd_script_lang        = UString.uc_string_of_ascii "latn";
      fd_features           = std_features
    };

    let rec env =
    {
      old_environment   = env;
      galleys           = PTable.create (UString.uc_string_of_ascii "main")    main_galley;
      page_layouts      = PTable.create (UString.uc_string_of_ascii "default") default_layout;
      float_misplacement_demerits = num_of_int 5000;
      page_no           = 1;
      pages             = [];
      math_style        = MathLayout.Text;
      font_table        = font_table;
      font_data         = font_data
    };

    env
  with
  [ e -> do
    {
      log_string "\nError: Can't open default font!";
      raise e
    }
  ]
};


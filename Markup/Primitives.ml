
open XNum;
open Runtime;
open Unicode;
open Logging;
open Dim;
open Typesetting;
open Box;
open Engine;
open ParseState;
open ParseArgs;

(* string constants *******************************************************************************)

value str_adj_demerits           = UString.uc_string_of_ascii "adj-demerits";
value str_alignment              = UString.uc_string_of_ascii "alignment";
value str_baseline_skip          = UString.uc_string_of_ascii "baseline-skip";
value str_binop_penalty          = UString.uc_string_of_ascii "binop-penalty";
value str_bottom_skip            = UString.uc_string_of_ascii "bottom-skip";
value str_british                = UString.uc_string_of_ascii "british";
value str_club_penalty           = UString.uc_string_of_ascii "club-penalty";
value str_delimiter_factor       = UString.uc_string_of_ascii "delimiter-factor";
value str_delimiter_shortfall    = UString.uc_string_of_ascii "delimiter-shortfall";
value str_direct                 = UString.uc_string_of_ascii "direct";
value str_double_hyphen_demerits = UString.uc_string_of_ascii "double-hyphen-demerits";
value str_dpi                    = UString.uc_string_of_ascii "dpi";
value str_emergency_stretch      = UString.uc_string_of_ascii "emergency-stretch";
value str_ex_hyphen_penalty      = UString.uc_string_of_ascii "ex-hyphen-penalty";
value str_family                 = UString.uc_string_of_ascii "family";
value str_features               = UString.uc_string_of_ascii "features";
value str_final_hyphen_demerits  = UString.uc_string_of_ascii "final-hyphen-demerits";
value str_fixed                  = UString.uc_string_of_ascii "fixed";
value str_float                  = UString.uc_string_of_ascii "float";
value str_float_sep              = UString.uc_string_of_ascii "float-sep";
value str_footnote               = UString.uc_string_of_ascii "footnote";
value str_galley                 = UString.uc_string_of_ascii "galley";
value str_grid_size              = UString.uc_string_of_ascii "grid-size";
value str_height                 = UString.uc_string_of_ascii "height";
value str_hyphen                 = UString.uc_string_of_ascii "hyphen";
value str_hyphen_params          = UString.uc_string_of_ascii "hyphen-params";
value str_hyphen_penalty         = UString.uc_string_of_ascii "hyphen-penalty";
value str_hyphen_table           = UString.uc_string_of_ascii "hyphen-table";
value str_leading                = UString.uc_string_of_ascii "leading";
value str_left                   = UString.uc_string_of_ascii "left";
value str_left_annotation        = UString.uc_string_of_ascii "left-annotation";
value str_left_hyphen_min        = UString.uc_string_of_ascii "left-hyphen-min";
value str_left_par_shape         = UString.uc_string_of_ascii "left-par-shape";
value str_left_skip              = UString.uc_string_of_ascii "left-skip";
value str_line_break_params      = UString.uc_string_of_ascii "line-break-params";
value str_line_params            = UString.uc_string_of_ascii "line-params";
value str_line_penalty           = UString.uc_string_of_ascii "line-penalty";
value str_line_skip              = UString.uc_string_of_ascii "line-skip";
value str_line_skip_limit        = UString.uc_string_of_ascii "line-skip-limit";
value str_looseness              = UString.uc_string_of_ascii "looseness";
value str_math_family            = UString.uc_string_of_ascii "math-family";
value str_math_params            = UString.uc_string_of_ascii "math-params";
value str_measure                = UString.uc_string_of_ascii "measure";
value str_med_math_skip          = UString.uc_string_of_ascii "med-math-skip";
value str_min_size               = UString.uc_string_of_ascii "min-size";
value str_name                   = UString.uc_string_of_ascii "name";
value str_no_victorian_spacing   = UString.uc_string_of_ascii "no-victorian-spacing";
value str_null_delimiter_space   = UString.uc_string_of_ascii "null-delimiter-space";
value str_page                   = UString.uc_string_of_ascii "page";
value str_par_indent             = UString.uc_string_of_ascii "par-indent";
value str_par_fill_skip          = UString.uc_string_of_ascii "par-fill-skip";
value str_par_params             = UString.uc_string_of_ascii "par-params";
value str_par_skip               = UString.uc_string_of_ascii "par-skip";
value str_post_break             = UString.uc_string_of_ascii "post-break";
value str_post_process_line      = UString.uc_string_of_ascii "post-process-line";
value str_pre_break              = UString.uc_string_of_ascii "pre-break";
value str_pre_tolerance          = UString.uc_string_of_ascii "pre-tolerance";
value str_register               = UString.uc_string_of_ascii "register";
value str_rel_penalty            = UString.uc_string_of_ascii "rel-penalty";
value str_right                  = UString.uc_string_of_ascii "right";
value str_right_annotation       = UString.uc_string_of_ascii "right-annotation";
value str_right_hyphen_min       = UString.uc_string_of_ascii "right-hyphen-min";
value str_right_par_shape        = UString.uc_string_of_ascii "right-par-shape";
value str_right_skip             = UString.uc_string_of_ascii "right-skip";
value str_river_demerits         = UString.uc_string_of_ascii "river-demerits";
value str_river_threshold        = UString.uc_string_of_ascii "river-threshold";
value str_scale                  = UString.uc_string_of_ascii "scale";
value str_script_lang            = UString.uc_string_of_ascii "script-lang";
value str_script_size            = UString.uc_string_of_ascii "script-size";
value str_script2_size           = UString.uc_string_of_ascii "script-script-size";
value str_separator              = UString.uc_string_of_ascii "separator";
value str_series                 = UString.uc_string_of_ascii "series";
value str_shape                  = UString.uc_string_of_ascii "shape";
value str_size                   = UString.uc_string_of_ascii "size";
value str_simple_breaking        = UString.uc_string_of_ascii "simple-breaking";
value str_skew                   = UString.uc_string_of_ascii "skew";
value str_skyline                = UString.uc_string_of_ascii "skyline";
value str_space_factor           = UString.uc_string_of_ascii "space-factor";
value str_space_params           = UString.uc_string_of_ascii "space-params";
value str_space_skip             = UString.uc_string_of_ascii "space-skip";
value str_script_space           = UString.uc_string_of_ascii "script-space";
value str_table_entry_left       = UString.uc_string_of_ascii "table-entry:left";
value str_table_entry_right      = UString.uc_string_of_ascii "table-entry:right";
value str_table_entry_top        = UString.uc_string_of_ascii "table-entry:top";
value str_table_entry_baseline   = UString.uc_string_of_ascii "table-entry:baseline";
value str_table_entry_bottom     = UString.uc_string_of_ascii "table-entry:bottom";
value str_TeX                    = UString.uc_string_of_ascii "TeX";
value str_text_size              = UString.uc_string_of_ascii "text-size";
value str_thick_math_skip        = UString.uc_string_of_ascii "thick-math-skip";
value str_thin_math_skip         = UString.uc_string_of_ascii "thin-math-skip";
value str_tolerance              = UString.uc_string_of_ascii "tolerance";
value str_top_skip               = UString.uc_string_of_ascii "top-skip";
value str_vert                   = UString.uc_string_of_ascii "vert";
value str_victorian_spacing      = UString.uc_string_of_ascii "victorian-spacing";
value str_widow_penalty          = UString.uc_string_of_ascii "widow-penalty";
value str_width                  = UString.uc_string_of_ascii "width";
value str_xspace_skip            = UString.uc_string_of_ascii "xspace-skip";

(* parameter dictionaries *************************************************************************)

value lookup_in_dict dict name conv default = match DynUCTrie.lookup_string name dict with
[ Some (Some d) -> Some (conv d)
| Some None     -> default
| None          -> None
];

value lookup_list   dict name = lookup_in_dict dict name (fun x -> x)            None;
value lookup_string dict name = lookup_in_dict dict name Array.of_list           None;
value lookup_int    dict name = lookup_in_dict dict name Parser.str_to_int       None;
value lookup_num    dict name = lookup_in_dict dict name Parser.str_to_num       None;
value lookup_skip   dict name = lookup_in_dict dict name Parser.str_expr_to_skip None;
value lookup_dim    dict name = lookup_in_dict dict name Parser.str_expr_to_dim  None;
value lookup_bool   dict name = lookup_in_dict dict name Parser.str_to_bool      (Some True);

(*
  |check_keys <dict> <key-dict> <loc>| checks whether every key of <dict> appears in <key-dict>
  and prints out a warning otherwise.
*)

value check_keys dict key_dict loc = do
{
  DynUCTrie.iter
    (fun k _ ->
      if not (DynUCTrie.mem_string k key_dict) then do
      {
        log_warn loc "unknown key `";
        log_uc_string k;
        log_string "'!"
      }
      else ())
    dict;
};

(* |leading_dict| maps the name of a leading method to the corresponding function. *)

value leading_dict =
   DynUCTrie.add_string str_fixed    Galley.leading_fixed
  (DynUCTrie.add_string str_register Galley.leading_register
  (DynUCTrie.add_string str_TeX      Galley.leading_TeX
  (DynUCTrie.add_string str_skyline  Galley.leading_skyline
    DynUCTrie.empty)));

(* Supported keys for line-params *)

value line_param_dict =
   DynUCTrie.add_string str_baseline_skip   ()
  (DynUCTrie.add_string str_line_skip_limit ()
  (DynUCTrie.add_string str_line_skip       ()
  (DynUCTrie.add_string str_leading         ()
  (DynUCTrie.add_string str_club_penalty    ()
  (DynUCTrie.add_string str_widow_penalty   ()
    DynUCTrie.empty)))));

value parse_line_param loc dict = do
{
  check_keys dict line_param_dict loc;

  let baseline_skip   = lookup_dim    dict str_baseline_skip;
  let line_skip_limit = lookup_skip   dict str_line_skip_limit;
  let line_skip       = lookup_dim    dict str_line_skip;
  let leading_type    = lookup_string dict str_leading;
  let club_penalty    = lookup_num    dict str_club_penalty;
  let widow_penalty   = lookup_num    dict str_widow_penalty;

  let leading = match leading_type with
  [ None     -> None
  | Some str -> match DynUCTrie.lookup_string str leading_dict with
      [ Some l -> Some l
      | None   -> do
        {
          log_warn loc "unknown leading `";
          log_uc_string str;
          log_string "`.";
          Some Galley.leading_TeX
        }
      ]
  ];

  let simple_cw club_penalty widow_penalty above below = do
  {
    if above <= 1 then
      club_penalty
    else if below <= 1 then
      widow_penalty
    else
      num_zero
  };

  let club_widow_penalty = match (club_penalty, widow_penalty) with
  [ (None,   None)   -> None
  | (Some c, Some w) -> Some (simple_cw c w)
  | (Some c, None)   -> Some (simple_cw c num_zero)
  | (None,   Some w) -> Some (simple_cw num_zero w)
  ];

  (baseline_skip, line_skip_limit, line_skip, leading, club_widow_penalty)
};

value parse_par_shape left_stream right_stream = do
{
  let read_next stream = do
  {
    let (a,b) = Parser.read_range stream;

    if UCStream.next_char stream = 58 then do  (* : *)
    {
      UCStream.remove stream 1;
      Parser.skip_spaces stream;

      (int_of_num (ceiling_num a), (int_of_num (floor_num b)), Parser.read_skip stream)
    }
    else do
    {
      log_warn (UCStream.location stream) "Invalid par shape!";
      log_int (UCStream.next_char stream);
      (-1, -1, (fun _ -> num_zero))
    }
  };
  let rec parse_shape stream = do
  {
    Parser.skip_spaces stream;

    if UCStream.next_char stream >= 0 then do
    {
      let (a,b,s) = read_next stream;

      if a < 0 then
        []
      else do
      {
        Parser.skip_spaces stream;

        match UCStream.next_char stream with
        [ 44 | 59 -> do
          {
            UCStream.remove stream 1;
            [(a,b,s) :: parse_shape stream]
          }
        | (-1) -> [(a,b,s)]
        | _    -> do
          {
            log_warn (UCStream.location stream) "Invalid par shape!";
            [(a,b,s)]
          }
        ]
      }
    }
    else
      []
  };

  let left_indent  = parse_shape left_stream;
  let right_indent = parse_shape right_stream;

  let rec lookup n list = match list with
  [ []              -> fun _ -> num_zero
  | [(a,b,s) :: ls] -> if a <= n && n <= b then
                         s
                       else
                         lookup n ls
  ];

  let par_shape env n = (lookup n left_indent env, lookup n right_indent env);

  par_shape
};

value parse_boxes ps code env = do
{
  let nodes = execute_string_in_mode ps code `HBox;

  let (b, get) =
    Compose.hyph_only_builder
      (Environment.current_font_metric env)
      (Environment.current_composer    env)
      (Galley.hyphen_params (Environment.current_galley env));
  let _ = Evaluate.eval_node_list env b nodes;

  get ()
};

value parse_annotation ps left_annotation right_annotation = do
{
  let new_ps = duplicate ps;

  let annotate env boxes = do
  {
    (* <left> and <right> should evaluate to a box of width zero. *)

    let left  = execute_string_in_mode new_ps left_annotation  `HBox;
    let right = execute_string_in_mode new_ps right_annotation `HBox;

    let (b, get) = Builder.simple_builder
                     (Environment.current_font_metric env)
                     (Environment.current_composer    env);

    let _ = Evaluate.eval_node_list env b left;

    Builder.add_box_list b boxes;

    let _ = Evaluate.eval_node_list env b right;

    get ()
  };

  annotate
};

value par_param_dict =
   DynUCTrie.add_string str_measure           ()
  (DynUCTrie.add_string str_par_indent        ()
  (DynUCTrie.add_string str_par_fill_skip     ()
  (DynUCTrie.add_string str_left_skip         ()
  (DynUCTrie.add_string str_right_skip        ()
  (DynUCTrie.add_string str_left_par_shape    ()
  (DynUCTrie.add_string str_right_par_shape   ()
  (DynUCTrie.add_string str_par_skip          ()
  (DynUCTrie.add_string str_left_annotation   ()
  (DynUCTrie.add_string str_right_annotation  ()
  (DynUCTrie.add_string str_pre_break         ()
  (DynUCTrie.add_string str_post_break        ()
  (DynUCTrie.add_string str_post_process_line ()
    DynUCTrie.empty))))))))))));

value parse_par_param ps loc dict = do
{
  check_keys dict par_param_dict loc;

  let measure           = lookup_num    dict str_measure;
  let par_indent        = lookup_dim    dict str_par_indent;
  let par_fill_skip     = lookup_dim    dict str_par_fill_skip;
  let left_skip         = lookup_dim    dict str_left_skip;
  let right_skip        = lookup_dim    dict str_right_skip;
  let par_shape_left    = lookup_string dict str_left_par_shape;
  let par_shape_right   = lookup_string dict str_right_par_shape;
  let par_skip          = lookup_dim    dict str_par_skip;
  let left_ann          = lookup_list   dict str_left_annotation;
  let right_ann         = lookup_list   dict str_right_annotation;
  let pre_break         = lookup_list   dict str_pre_break;
  let post_break        = lookup_list   dict str_post_break;
  let post_process_line = None; (* FIX *)

  let par_shape = match (par_shape_left, par_shape_right) with
  [ (None, None)            -> None
  | (Some left, None)       -> Some (parse_par_shape (UCStream.of_string left) (UCStream.of_list []))
  | (None, Some right)      -> Some (parse_par_shape (UCStream.of_list [])     (UCStream.of_string right))
  | (Some left, Some right) -> Some (parse_par_shape (UCStream.of_string left) (UCStream.of_string right))
  ];

  let post_process = match (post_process_line, left_ann, right_ann) with
  [ (Some p, _,      _)      -> Some p
  | (None,   Some l, Some r) -> Some (parse_annotation ps l  r)
  | (None,   Some l, None)   -> Some (parse_annotation ps l  [])
  | (None,   None,   Some r) -> Some (parse_annotation ps [] r)
  | (None,   None,   None)   -> None
  ];
  let pre_break2 = match pre_break with
  [ Some p -> Some (parse_boxes ps p)
  | None   -> None
  ];
  let post_break2 = match post_break with
  [ Some p -> Some (parse_boxes ps p)
  | None   -> None
  ];

  (measure, par_indent, par_fill_skip, left_skip, right_skip, par_shape, par_skip, pre_break2, post_break2, post_process)
};

value line_break_param_dict =
   DynUCTrie.add_string str_pre_tolerance          ()
  (DynUCTrie.add_string str_tolerance              ()
  (DynUCTrie.add_string str_looseness              ()
  (DynUCTrie.add_string str_line_penalty           ()
  (DynUCTrie.add_string str_adj_demerits           ()
  (DynUCTrie.add_string str_double_hyphen_demerits ()
  (DynUCTrie.add_string str_final_hyphen_demerits  ()
  (DynUCTrie.add_string str_emergency_stretch      ()
  (DynUCTrie.add_string str_simple_breaking        ()
  (DynUCTrie.add_string str_river_demerits         ()
  (DynUCTrie.add_string str_river_threshold        ()
    DynUCTrie.empty))))))))));

value parse_line_break_param loc dict = do
{
  check_keys dict line_break_param_dict loc;

  let pre_tolerance          = lookup_num  dict str_pre_tolerance;
  let tolerance              = lookup_num  dict str_tolerance;
  let looseness              = lookup_int  dict str_looseness;
  let line_penalty           = lookup_num  dict str_line_penalty;
  let adj_demerits           = lookup_num  dict str_adj_demerits;
  let double_hyphen_demerits = lookup_num  dict str_double_hyphen_demerits;
  let final_hyphen_demerits  = lookup_num  dict str_final_hyphen_demerits;
  let emergency_stretch      = lookup_skip dict str_emergency_stretch;
  let river_demerits         = lookup_num  dict str_river_demerits;
  let river_threshold        = lookup_skip dict str_river_threshold;
  let simple_breaking        = lookup_bool dict str_simple_breaking;

  (pre_tolerance, tolerance, looseness, line_penalty, adj_demerits,
   double_hyphen_demerits, final_hyphen_demerits, emergency_stretch,
   river_demerits, river_threshold, simple_breaking)
};

value hyphen_param_dict =
   DynUCTrie.add_string str_hyphen_table      ()
  (DynUCTrie.add_string str_hyphen_penalty    ()
  (DynUCTrie.add_string str_ex_hyphen_penalty ()
  (DynUCTrie.add_string str_left_hyphen_min   ()
  (DynUCTrie.add_string str_right_hyphen_min  ()
  (DynUCTrie.add_string str_script_lang       ()
    DynUCTrie.empty)))));

value parse_hyphen_param loc dict = do
{
  check_keys dict hyphen_param_dict loc;

  let hyphen_table      = lookup_string dict str_hyphen_table;
  let hyphen_penalty    = lookup_num    dict str_hyphen_penalty;
  let ex_hyphen_penalty = lookup_num    dict str_ex_hyphen_penalty;
  let left_hyphen_min   = lookup_int    dict str_left_hyphen_min;
  let right_hyphen_min  = lookup_int    dict str_right_hyphen_min;
  let script_lang       = lookup_string dict str_script_lang;

  (hyphen_table, hyphen_penalty, ex_hyphen_penalty,
   left_hyphen_min, right_hyphen_min, script_lang)
};

value font_dict =
   DynUCTrie.add_string str_family      ()
  (DynUCTrie.add_string str_series      ()
  (DynUCTrie.add_string str_shape       ()
  (DynUCTrie.add_string str_size        ()
  (DynUCTrie.add_string str_script_lang ()
  (DynUCTrie.add_string str_features    ()
    DynUCTrie.empty)))));

value parse_font_dict loc dict = do
{
  check_keys dict font_dict loc;

  let family   = lookup_string dict str_family;
  let series   = lookup_string dict str_series;
  let shape    = lookup_string dict str_shape;
  let size     = lookup_num    dict str_size;
  let script   = lookup_string dict str_script_lang;

  let features = match lookup_list dict str_features with
  [ None     -> None
  | Some str -> Some (List.map Array.of_list (Parser.str_to_list str))
  ];

  (family, series, shape, size, script, features)
};

value math_font_dict =
   DynUCTrie.add_string str_math_family  ()
  (DynUCTrie.add_string str_family       ()
  (DynUCTrie.add_string str_series       ()
  (DynUCTrie.add_string str_shape        ()
  (DynUCTrie.add_string str_text_size    ()
  (DynUCTrie.add_string str_script_size  ()
  (DynUCTrie.add_string str_script2_size ()
    DynUCTrie.empty))))));

value parse_math_font_dict loc dict = do
{
  check_keys dict math_font_dict loc;

  let math_family  = lookup_int    dict str_math_family;
  let family       = lookup_string dict str_family;
  let series       = lookup_string dict str_series;
  let shape        = lookup_string dict str_shape;
  let text_size    = lookup_num    dict str_text_size;
  let script_size  = lookup_num    dict str_script_size;
  let script2_size = lookup_num    dict str_script2_size;

  (math_family, family, series, shape, text_size, script_size, script2_size)
};

value space_param_dict =
   DynUCTrie.add_string str_space_factor         ()
  (DynUCTrie.add_string str_space_skip           ()
  (DynUCTrie.add_string str_xspace_skip          ()
  (DynUCTrie.add_string str_victorian_spacing    ()
  (DynUCTrie.add_string str_no_victorian_spacing ()
    DynUCTrie.empty))));

value parse_space_param loc dict = do
{
  check_keys dict space_param_dict loc;

  let space_factor = lookup_num  dict str_space_factor;
  let space_skip   = lookup_dim  dict str_space_skip;
  let xspace_skip  = lookup_dim  dict str_xspace_skip;
  let victorian    = lookup_bool dict str_victorian_spacing;
  let novictorian  = lookup_bool dict str_no_victorian_spacing;

  let vic = match victorian with
  [ Some _ -> victorian
  | None   -> novictorian
  ];

  (space_factor, space_skip, xspace_skip, vic)
};

value math_param_dict =
   DynUCTrie.add_string str_thin_math_skip       ()
  (DynUCTrie.add_string str_med_math_skip        ()
  (DynUCTrie.add_string str_thick_math_skip      ()
  (DynUCTrie.add_string str_script_space         ()
  (DynUCTrie.add_string str_rel_penalty          ()
  (DynUCTrie.add_string str_binop_penalty        ()
  (DynUCTrie.add_string str_delimiter_factor     ()
  (DynUCTrie.add_string str_delimiter_shortfall  ()
  (DynUCTrie.add_string str_null_delimiter_space ()
    DynUCTrie.empty))))))));

value parse_math_param loc dict = do
{
  check_keys dict math_param_dict loc;

  let thin_math_skip       = lookup_dim  dict str_thin_math_skip;
  let med_math_skip        = lookup_dim  dict str_med_math_skip;
  let thick_math_skip      = lookup_dim  dict str_thick_math_skip;
  let script_space         = lookup_dim  dict str_script_space;
  let rel_penalty          = lookup_num  dict str_rel_penalty;
  let binop_penalty        = lookup_num  dict str_binop_penalty;
  let delimiter_factor     = lookup_num  dict str_delimiter_factor;
  let delimiter_shortfall  = lookup_skip dict str_delimiter_shortfall;
  let null_delimiter_space = lookup_dim  dict str_null_delimiter_space;

  (thin_math_skip, med_math_skip, thick_math_skip, script_space, rel_penalty, binop_penalty,
   delimiter_factor, delimiter_shortfall, null_delimiter_space)
};

value galley_area_dict =
   DynUCTrie.add_string str_name        ()
  (DynUCTrie.add_string str_top_skip    ()
  (DynUCTrie.add_string str_bottom_skip ()
  (DynUCTrie.add_string str_min_size    ()
  (DynUCTrie.add_string str_grid_size   ()
    DynUCTrie.empty))));

value parse_galley_area_dict loc dict = do
{
  check_keys dict galley_area_dict loc;

  let name = lookup_string dict str_name;
  let top  = lookup_skip   dict str_top_skip;
  let bot  = lookup_skip   dict str_bottom_skip;
  let min  = lookup_skip   dict str_min_size;
  let grid = lookup_skip   dict str_grid_size;

  (Option.from_option [||]                               name,
   Option.from_option (Evaluate.const_em num_one)        top,
   Option.from_option (Evaluate.const_em num_one)        bot,
   Option.from_option (Evaluate.const_em (num_of_int 5)) min,
   Option.from_option (Evaluate.const_em num_one)        grid)
};

value float_area_dict =
   DynUCTrie.add_string str_alignment   ()
  (DynUCTrie.add_string str_top_skip    ()
  (DynUCTrie.add_string str_bottom_skip ()
  (DynUCTrie.add_string str_float_sep   ()
    DynUCTrie.empty)));

value parse_float_area_dict loc dict = do
{
  check_keys dict float_area_dict loc;

  let align = match DynUCTrie.lookup_string str_alignment dict with
  [ Some (Some str) -> match UString.to_string str with
      [ "top"    -> FloatVertical.Top
      | "bottom" -> FloatVertical.Bottom
      | _        -> do
        {
          log_warn loc "unknown alignment `";
          log_uc_list str;
          log_string "'!";
          FloatVertical.Top
        }
      ]
  | _ -> FloatVertical.Top
  ];
  let top = lookup_skip dict str_top_skip;
  let bot = lookup_skip dict str_bottom_skip;
  let sep = lookup_dim  dict str_float_sep;

  (align,
   Option.from_option (Evaluate.const_em num_one) top,
   Option.from_option (Evaluate.const_em num_one) bot,
   Option.from_option (Evaluate.const_fixed_dim (Evaluate.const_em num_one))
                      sep)
};

value footnote_area_dict =
   DynUCTrie.add_string str_separator         ()
  (DynUCTrie.add_string str_top_skip          ()
  (DynUCTrie.add_string str_bottom_skip       ()
  (DynUCTrie.add_string str_grid_size         ()
  (DynUCTrie.add_string str_line_params       ()
  (DynUCTrie.add_string str_par_params        ()
  (DynUCTrie.add_string str_line_break_params ()
  (DynUCTrie.add_string str_hyphen_params     ()
  (DynUCTrie.add_string str_space_params      ()
  (DynUCTrie.add_string str_math_params       ()
    DynUCTrie.empty)))))))));

value parse_footnote_area_dict ps loc dict = do
{
  let lookup_key_val dict name = match lookup_list dict name with
  [ None     -> DynUCTrie.empty
  | Some str -> Parser.str_to_key_val str
  ];

  check_keys dict footnote_area_dict loc;

  let separator = match lookup_list dict str_separator with
  [ None     -> []
  | Some str -> do
    {
      let new_ps = duplicate ps;
      execute_string_in_mode new_ps str `VBox
    }
  ];

  let top               = lookup_skip    dict str_top_skip;
  let bot               = lookup_skip    dict str_bottom_skip;
  let grid              = lookup_skip    dict str_grid_size;
  let line_params       = lookup_key_val dict str_line_params;
  let par_params        = lookup_key_val dict str_par_params;
  let line_break_params = lookup_key_val dict str_line_break_params;
  let hyphen_params     = lookup_key_val dict str_hyphen_params;
  let space_params      = lookup_key_val dict str_space_params;
  let math_params       = lookup_key_val dict str_math_params;

  (separator,
   Option.from_option (Evaluate.const_em num_one) top,
   Option.from_option (Evaluate.const_em num_one) bot,
   Option.from_option (Evaluate.const_em num_one) grid,
   parse_line_param       loc line_params,
   parse_par_param     ps loc par_params,
   parse_line_break_param loc line_break_params,
   parse_hyphen_param     loc hyphen_params,
   parse_space_param      loc space_params,
   parse_math_param       loc math_params)
};

(* primitive commands *****************************************************************************)

(* parameters *)

value set_param ps = do
{
  let param = arg_expanded ps;
  let loc   = location ps;
  let val   = arg_key_val  ps;

  let add_cmd cmd parse = add_node ps (Node.Command loc (cmd (parse loc val)));

  match UString.to_string param with
  [ "font"             -> add_cmd Environment.set_font                      parse_font_dict
  | "paragraph"        -> add_cmd Environment.set_par_params                (parse_par_param ps)
  | "line"             -> add_cmd Environment.set_line_params               parse_line_param
  | "line-break"       -> add_cmd Environment.set_line_break_params         parse_line_break_param
  | "hyphenation"      -> add_cmd Environment.set_hyphen_params             parse_hyphen_param
  | "space"            -> add_cmd Environment.set_space_params              parse_space_param
  | "math"             -> add_cmd Environment.set_math_params               parse_math_param
  | "this-paragraph"   -> add_cmd Environment.set_current_par_params        (parse_par_param ps)
  | "this-line"        -> add_cmd Environment.set_current_line_params       parse_line_param
  | "this-line-break"  -> add_cmd Environment.set_current_line_break_params parse_line_break_param
  | "this-hyphenation" -> add_cmd Environment.set_current_hyphen_params     parse_hyphen_param
  | "this-space"       -> add_cmd Environment.set_current_space_params      parse_space_param
  | "this-math"        -> add_cmd Environment.set_current_math_params       parse_math_param
  | _ -> do
    {
      log_warn loc ("Unknown dictionary parameter " ^ UString.to_string param ^ "!")
    }
  ]
};

value set_math_font ps = do
{
  let font = parse_math_font_dict (location ps) (arg_key_val ps);

  add_node ps (Node.Command (location ps) (Environment.set_math_font font))
};

value set_mark ps = do
{
  let mark = Array.of_list (arg_expanded ps);
  let val  = Array.of_list (arg_expanded ps);

  add_node ps (Node.CommandBox (location ps) (`PageCmd (Box.SetMark mark val)))
};

(* boxes and glue *)

value hskip ps = do
{
  Mode.ensure_par_mode ps;

  let width = arg_TeX_dim ps;

  add_node ps (Node.Glue (location ps) width (fun _ -> dim_zero) False True)
};

value vskip ps = do
{
  Mode.leave_par_mode ps;

  let height = arg_TeX_dim ps;

  add_node ps (Node.Glue (location ps) (fun _ -> dim_zero) height False True)
};

value kern ps = do
{
  let skip = arg_TeX_dim ps;

  match current_mode ps with
  [ `Galley    | `VBox      -> add_node ps (Node.Glue (location ps) (fun _ -> dim_zero) skip True True)
  | `Paragraph | `HBox
  | `LRBox | `RLBox | `Math -> add_node ps (Node.Glue (location ps) skip (fun _ -> dim_zero) True True)
  | _                       -> ()
  ]
};

value hbox dir ps = do
{
  let close_hbox               contents = add_node ps (Node.HBox       (location ps) dir        contents);
  let close_hbox_to     width  contents = add_node ps (Node.HBoxTo     (location ps) dir width  contents);
  let close_hbox_spread amount contents = add_node ps (Node.HBoxSpread (location ps) dir amount contents);

  let mode = match dir with
  [ `LR      -> `LRBox
  | `RL      -> `RLBox
  | `Default -> `HBox
  ];

  let open_hbox close_command = do
  {
    if CharCode.cat_code (UCStream.next_char ps.input_stream) = CharCode.BeginGroup then do
    {
(*      Mode.ensure_par_mode ps; *)
      close_command (arg_execute ps mode)
    }
    else
      log_warn (location ps) "syntax error in \\hbox!";
  };

  Parser.skip_blanks ps.input_stream;

  if Parser.match_substring ps.input_stream [116; 111] 0 then do        (* to *)
  {
    UCStream.remove ps.input_stream 2;

    Parser.skip_spaces ps.input_stream;

    let width = Parser.read_simple_skip_expression ps.input_stream;

    open_hbox (fun c -> close_hbox_to width c)
  }
  else if Parser.match_substring ps.input_stream [115; 112; 114; 101; 97; 100] 0 then do  (* spread *)
  {
    UCStream.remove ps.input_stream 6;

    Parser.skip_spaces ps.input_stream;

    let amount = Parser.read_simple_skip_expression ps.input_stream;

    open_hbox (fun c -> close_hbox_spread amount c)
  }
  else
    open_hbox close_hbox
};

value vbox ps = do
{
  let close_vbox               contents = add_node ps (Node.VBox       (location ps)        contents);
  let close_vbox_to     height contents = add_node ps (Node.VBoxTo     (location ps) height contents);
  let close_vbox_spread amount contents = add_node ps (Node.VBoxSpread (location ps) amount contents);

  let open_vbox close_command = do
  {
    if CharCode.cat_code (UCStream.next_char ps.input_stream) = CharCode.BeginGroup then
      close_command (arg_execute ps `VBox)
    else
      log_warn (location ps) "syntax error in \\vbox!";
  };

  Parser.skip_blanks ps.input_stream;

  if Parser.match_substring ps.input_stream [116; 111] 0 then do
  {
    UCStream.remove ps.input_stream 2;

    Parser.skip_spaces ps.input_stream;

    let height = Parser.read_simple_skip_expression ps.input_stream;

    open_vbox (fun c -> close_vbox_to height c)
  }
  else if Parser.match_substring ps.input_stream [115; 112; 114; 101; 97; 100] 0 then do
  {
    UCStream.remove ps.input_stream 6;

    Parser.skip_spaces ps.input_stream;

    let amount = Parser.read_simple_skip_expression ps.input_stream;

    open_vbox (fun c -> close_vbox_spread amount c)
  }
  else
    open_vbox close_vbox
};

value phantom ps = do
{
  let arg = arg_execute ps `HBox;

  add_node ps (Node.Phantom (location ps) True True arg)
};

value hphantom ps = do
{
  let arg = arg_execute ps `HBox;

  add_node ps (Node.Phantom (location ps) True False arg)
};

value vphantom ps = do
{
  let arg = arg_execute ps `HBox;

  add_node ps (Node.Phantom (location ps) False True arg)
};

value hleaders ps = do
{
  let width = arg_dim ps;
  let body  = arg_execute ps `HBox;

  add_node ps (Node.HLeaders (location ps) width body)
};

value vinsert ps = do
{
  let above = Parser.read_bool ps.input_stream;
  let arg   = arg_execute ps `VBox;

  add_node ps (Node.VInsert (location ps) (not above) arg)
};

value rule ps = do
{
  let width  = arg_dim ps;
  let height = arg_dim ps;
  let depth  = arg_dim ps;

  add_node ps (Node.Rule (location ps) width height depth)
};

value image ps = do
{
  let add_image file fmt width height = do
  {
    add_node ps (Node.Image (location ps) (UString.to_string file) fmt width height)
  };

  let get_scale options dpi = do
  {
    if dpi >/ num_zero then
      num_one
    else do
    {
      try
        match DynUCTrie.lookup_string str_dpi options with
        [ Some (Some str) -> Parser.str_expr_to_num str // num_of_int 100 
                             (* LoadImage assumed a default dpi value of 100. *)
        | _               -> raise Not_found
        ]
      with
      [ Not_found -> do
        {
          log_warn (location ps) "No dpi value specified!";
          num_one
        }
      ]
    }
  };
(*  let get_dpi options info = do
  {
    match CamlImages.Images.dpi info with
    [ Some dpi -> num_of_ints (int_of_float (10000.0 *. dpi)) 10000
    | None     -> do
      {
        try
          match DynUCTrie.lookup_string str_dpi options with
          [ Some (Some str) -> Parser.str_expr_to_num str
          | _               -> raise Not_found
          ]
        with
        [ Not_found -> do
          {
            log_warn (location ps) "No dpi value specified!";
            num_of_int 100
          }
        ]
      }
    ]
  };
  *)

  let options = opt_key_val  ps;
  let file    = arg_expanded ps;

  let page = match DynUCTrie.lookup_string str_page options with
  [ Some (Some p) -> do
    {
      let n = Parser.str_expr_to_num p;
      int_of_num (floor_num n) - 1
    }
  | _ -> 0
  ];

  try
    let (fmt, image_width, image_height, image_dpi) = LoadImage.get_dimensions (UString.bytes_to_string file) page;

    match DynUCTrie.lookup_string str_width options with
    [ Some (Some w) -> do
      {
        let width  = Parser.str_expr_to_skip w;
        let height = match DynUCTrie.lookup_string str_height options with
                     [ Some (Some h) -> Parser.str_expr_to_skip h
                     | _             -> fun e -> width e // image_width */ image_height
                     ];

        add_image file fmt width height
      }
    | _ -> match DynUCTrie.lookup_string str_height options with
      [ Some (Some h) -> do
        {
          let height = Parser.str_expr_to_skip h;
          let width  = match DynUCTrie.lookup_string str_height options with
                       [ Some (Some w) -> Parser.str_expr_to_skip w
                       | _             -> fun e -> height e // image_height */ image_width
                       ];

          add_image file fmt width height
        }
      | _ -> do
        {
          let scale  = get_scale options image_dpi;
          let width  = scale */ image_width;
          let height = scale */ image_height;

          add_image file fmt (fun _ -> width) (fun _ -> height)
        }
      ]
    ]
  with
  [ _ -> do
    {
      log_warn (location ps) "Cannot load image `";
      log_uc_list file;
      log_string "'!"
    }
  ]
(*
    let (_, header) = CamlImages.Images.file_format (UString.bytes_to_string file);

    match DynUCTrie.lookup_string str_width options with
    [ Some (Some w) -> do
      {
        let width  = Parser.str_expr_to_skip w;
        let height = match DynUCTrie.lookup_string str_height options with
                     [ Some (Some h) -> Parser.str_expr_to_skip h
                     | _ -> fun e -> width e // num_of_int header.CamlImages.Images.header_width
                                             */ num_of_int header.CamlImages.Images.header_height
                     ];

        add_image file width height
      }
    | _ -> match DynUCTrie.lookup_string str_height options with
      [ Some (Some h) -> do
        {
          let height = Parser.str_expr_to_skip h;
          let width  = match DynUCTrie.lookup_string str_height options with
                       [ Some (Some w) -> Parser.str_expr_to_skip w
                       | _ -> fun e -> height e // num_of_int header.CamlImages.Images.header_height
                                                */ num_of_int header.CamlImages.Images.header_width
                       ];

          add_image file width height
        }
      | _ -> do
        {
          let dpi    = get_dpi options header.CamlImages.Images.header_infos;
          let width  = inch */ num_of_int header.CamlImages.Images.header_width // dpi;
          let height = inch */ num_of_int header.CamlImages.Images.header_height // dpi;

          add_image file (fun _ -> width) (fun _ -> height)
        }
      ]
    ]
  with
  [ _ -> do
    {
      log_warn (location ps) "Cannot load image `";
      log_uc_list file;
      log_string "'!"
    }
  ]
*)
};

value parbox ps = do
{
  let pos    = opt_expanded ps [116];
  let width  = arg_skip     ps;

  open_node_list ps `Galley;

  execute_argument ps;

  Mode.leave_par_mode ps;

  let body = close_node_list ps `Galley;
  let name = Array.of_list (gen_unique_name ());

  add_node ps (Node.BeginGroup  (location ps));
  add_node ps (Node.NewGalley   (location ps) name width);
  add_node ps (Node.AddToGalley (location ps) name body);

  match pos with
  [ [ 98] -> add_node ps (Node.PutGalleyInVBox (location ps) False name)
(*  | [ 99] -> add_node ps (Node.PutGalleyInVBox (location ps) name); *)
  | [116] -> add_node ps (Node.PutGalleyInVBox (location ps) True name)
  | _     -> do
    {
      log_warn (location ps) "pos `";
      log_uc_list pos;
      log_string "' unsupported, assuming `t'!";
      add_node ps (Node.PutGalleyInVBox (location ps) True name)
    }
  ];
  add_node ps (Node.EndGroup (location ps))
};

value accent ps = do
{
  let acc = arg_int     ps;
  let chr = arg_execute ps `HBox;

  add_node ps (Node.Accent (location ps) acc chr)
};

(* tables *)

value begin_table_entry ps = do
{
  if current_mode ps <> `Table then
    log_warn (location ps) "ignoring table entry outside table!"
  else do
  {
    open_node_list ps `HBox;
  }
};

value end_table_entry ps = do
{
  let contents = close_node_list ps `HBox;

  let l  = get_counter ps str_table_entry_left;
  let r  = get_counter ps str_table_entry_right;
  let t  = get_counter ps str_table_entry_top;
  let bl = get_counter ps str_table_entry_baseline;
  let b  = get_counter ps str_table_entry_bottom;

  add_node ps (Node.TableEntry (location ps) l r t bl b contents);

  (* set size of the next entry *)

  set_counter ps str_table_entry_left     (r+1);
  set_counter ps str_table_entry_right    (r+1);
  set_counter ps str_table_entry_top      bl;
  set_counter ps str_table_entry_baseline bl;
  set_counter ps str_table_entry_bottom   bl
};

value new_table_entry ps = do
{
  end_table_entry   ps;
  begin_table_entry ps
};

value new_table_row ps = do
{
  end_table_entry ps;

  let bl = get_counter ps str_table_entry_baseline;

  set_counter ps str_table_entry_left     0;
  set_counter ps str_table_entry_right    0;
  set_counter ps str_table_entry_top      (bl+1);
  set_counter ps str_table_entry_baseline (bl+1);
  set_counter ps str_table_entry_bottom   (bl+1);

  begin_table_entry ps
};

value begin_table ps = do
{
  open_node_list ps `Table;

  set_counter ps str_table_entry_left     0;
  set_counter ps str_table_entry_right    0;
  set_counter ps str_table_entry_top      0;
  set_counter ps str_table_entry_baseline 0;
  set_counter ps str_table_entry_bottom   0;

  begin_table_entry ps
};

value end_table ps = do
{
  end_table_entry ps;

  add_node ps (Node.Table (location ps) (close_node_list ps `Table));
};

(* breaks *)

value penalty ps = do
{
  let arg = arg_num ps;

  add_node ps (Node.Break (location ps) (Some arg) False [] [] [])
};

value discretionary ps = do
{
  let hyph    = Parser.read_bool ps.input_stream;
  let penalty = match opt_expanded ps [] with
                [ [] -> None
                | p  -> Some (Parser.str_expr_to_num p)
                ];
  let pre     = arg_execute ps `HBox;
  let post    = arg_execute ps `HBox;
  let no      = arg_execute ps `HBox;

  add_node ps (Node.Break (location ps) penalty hyph pre post no)
};

(* commands and environments *)

value define_command ps = do
{
  let name = Parser.read_argument ps.input_stream;
  let tmpl = opt_expanded ps [];
  let args = Macro.parse_arg_template
               (location ps)
               tmpl;
  let body = Parser.read_argument ps.input_stream;

  if Parser.is_token name && List.hd name = CharCode.escape_char then
    ParseState.define_command ps name
      { execute = Macro.execute_macro args body;
        expand  = Macro.expand_macro  args body }
  else do
  {
    log_warn (location ps) "";
    log_uc_list name;
    log_string " is not a valid command sequence!";
  }
};

value define_pattern ps = do
{
  let name = Parser.read_argument ps.input_stream;
  let tmpl = opt_expanded ps [];
  let args = Macro.parse_arg_template
               (location ps)
               tmpl;
  let body = Parser.read_argument ps.input_stream;

  ParseState.define_pattern ps name
    { execute = Macro.execute_macro args body;
      expand  = Macro.expand_macro  args body }
};

value save_command ps = do
{
  let name = Parser.read_argument ps.input_stream;

  ParseState.save_command ps name
};

value restore_command ps = do
{
  let name = Parser.read_argument ps.input_stream;

  ParseState.restore_command ps name
};

value save_pattern ps = do
{
  let name = Parser.read_argument ps.input_stream;

  ParseState.save_pattern ps name
};

value restore_pattern ps = do
{
  let name = Parser.read_argument ps.input_stream;

  ParseState.restore_pattern ps name
};

value define_environment ps = do
{
  let name       = Parser.read_argument ps.input_stream;
  let tmpl       = opt_expanded ps [];
  let args       = Macro.parse_arg_template
                     (location ps)
                     tmpl;
  let begin_body = Parser.read_argument ps.input_stream;
  let end_body   = Parser.read_argument ps.input_stream;

  define_env ps name
    { execute = Macro.execute_begin_environment name args begin_body;
      expand  = Macro.expand_begin_environment  name args begin_body }
    { execute = Macro.execute_end_environment   end_body;
      expand  = Macro.expand_end_environment    end_body }
};

(* |begin_environment| and |end_environment| start and end an environment. *)

value begin_environment ps = do
{
  Macro.begin_env ps (Parser.read_argument ps.input_stream)
};

value end_environment ps = do
{
  Macro.end_env ps (Parser.read_argument ps.input_stream)
};

(* math *)

(* |toggle_math| toggles between math- and text-mode. This corresponds to |$| in \TeX. *)

value toggle_math ps = match current_mode ps with
[ `Math -> ignore (Mode.set_mode ps `Paragraph)
| _     -> ignore (Mode.set_mode ps `Math)
];

value begin_text ps = do
{
  if current_mode ps = `Math then do
  {
    open_node_list ps `HBox;
    add_node ps (Node.Command (location ps) Environment.adapt_fonts_to_math_style)
  }
  else
    open_node_list ps `HBox
};

value end_text ps = do
{
  let nodes = close_node_list ps `HBox;

  match current_mode ps with
  [ `Preamble | `Galley | `Paragraph -> do
    {
      Mode.ensure_par_mode ps;
      List.iter (add_node ps) nodes
    }
  | `HBox | `Table ->
      List.iter (add_node ps) nodes
  | `Math | `LRBox | `RLBox | `VBox  ->
      add_node ps (Node.HBox (location ps) `Default nodes)
  ]
};

(* Print a single math character. *)

value add_math_char ps math_code = do
{
  add_node ps (Node.MathChar (location ps) math_code)
};

(* commands for sub- and super-scripts. *)

value sub_script ps = do
{
  if current_mode ps <> `Math then
    log_warn (location ps) "subscript in text mode!"
  else do
  {
    match arg_execute ps `Math with
    [ []    -> log_warn (location ps) "empty subscript!"
    | nodes -> add_node ps (Node.SubScript (location ps) nodes)
    ]
  }
};

value super_script ps = do
{
  if current_mode ps <> `Math then
    log_warn (location ps) "superscript in text mode!"
  else do
  {
    match arg_execute ps `Math with
    [ []    -> log_warn (location ps) "empty superscript!"
    | nodes -> add_node ps (Node.SuperScript (location ps) nodes)
    ]
  }
};

value prime ps = do
{
  if current_mode ps <> `Math then
    log_warn (location ps) "prime in text mode!"
  else
    add_node ps (Node.SuperScript (location ps) [Node.MathChar (location ps) (Ordinary, (2, 2), (48, 48))])
};

value overline ps = do
{
  add_node ps (Node.Overline (location ps) (arg_execute ps `Math))
};

value underline ps = do
{
  add_node ps (Node.Underline (location ps) (arg_execute ps `Math))
};

value math_accent ps family char = do
{
  add_node ps (Node.MathAccent (location ps) family char (arg_execute ps `Math))
};

value put_root ps small_family small_char large_family large_char = do
{
  add_node ps (Node.Root (location ps) small_family small_char large_family large_char (arg_execute ps `Math))
};

value set_math_code ps code = do
{
  add_node ps (Node.MathCode (location ps) code (arg_execute ps `Math))
};

value left_delim ps = do
{
  open_node_list ps `Math;

  if UCStream.next_char ps.input_stream = 46 then do
  {
    UCStream.remove ps.input_stream 1;
    add_node ps (Node.Nodes [Node.MathChar (location ps) (Open, (-1, -1), (-1, -1))])
  }
  else
    add_node ps (Node.Nodes (arg_execute ps `Math));

  open_node_list ps `Math
};

value mid_delim ps = do
{
  add_node ps (Node.Nodes (close_node_list ps `Math));

  if UCStream.next_char ps.input_stream = 46 then do
  {
    UCStream.remove ps.input_stream 1;
    add_node ps (Node.Nodes [Node.MathChar (location ps) (Open, (-1, -1), (-1, -1))])
  }
  else
    add_node ps (Node.Nodes (arg_execute ps `Math));

  open_node_list ps `Math
};

value right_delim ps = do
{
  add_node ps (Node.Nodes (close_node_list ps `Math));

  if UCStream.next_char ps.input_stream = 46 then do
  {
    UCStream.remove ps.input_stream 1;
    add_node ps (Node.Nodes [Node.MathChar (location ps) (Close, (-1, -1), (-1, -1))])
  }
  else
    add_node ps (Node.Nodes (arg_execute ps `Math));

  let nodes =
    List.map
      (fun x -> match x with
       [ Node.Nodes ns -> ns
       | _             -> do { log_warn (location ps) "syntax error in left-right statement!"; [] }
       ])
      (close_node_list ps `Math);

  add_node ps (Node.LeftRight (location ps) nodes)
};

value fraction ps = do
{
  let num   = arg_execute ps `Math;
  let denom = arg_execute ps `Math;

  add_node ps (Node.Fraction
                 (location ps)
                 num
                 denom
                 (Node.MathChar (location ps) (Open,  (-1, -1), (-1, -1)))
                 (Node.MathChar (location ps) (Close, (-1, -1), (-1, -1)))
                 (fun _ -> num_of_int (-1)))
};

value general_fraction ps = do
{
  let left  = arg_execute ps `Math;
  let right = arg_execute ps `Math;
  let thick = arg_skip    ps;
  let num   = arg_execute ps `Math;
  let denom = arg_execute ps `Math;

  if List.length left <> 1 || List.length right <> 1 then
    log_warn (location ps) "invalid delimiter!"
  else
    add_node ps (Node.Fraction (location ps) num denom (List.hd left) (List.hd right) thick)
};

value set_math_style style ps = do
{
  add_node ps (Node.MathStyle (location ps) style)
};

value index_position ps = do
{
  let pos = Array.of_list (arg_expanded ps);

  if pos = str_left then
    add_node ps (Node.IndexPosition (location ps) LeftIndex)
  else if pos = str_right then
    add_node ps (Node.IndexPosition (location ps) RightIndex)
  else if pos = str_vert then
    add_node ps (Node.IndexPosition (location ps) VertIndex)
  else do
  {
    log_error (location ps) "unknown index position `";
    log_uc_string pos;
    log_string "'!"
  }
};

(* counters *)

value new_counter ps = do
{
  let super = Array.of_list (opt_expanded ps []);
  let ctr   = Array.of_list (arg_expanded ps);

  if Array.length super = 0 then
    new_counter ps ctr 0 None
  else
    new_counter ps ctr 0 (Some super)
};

value set_counter ps = do
{
  let ctr = Array.of_list (arg_expanded ps);
  let arg = arg_int ps;

  ParseState.set_counter ps ctr arg
};

value add_to_counter ps = do
{
  let ctr = Array.of_list (arg_expanded ps);
  let arg = arg_int ps;

  ParseState.set_counter ps ctr (get_counter ps ctr + arg)
};

value format_counter ps fmt ctr = do
{
  (* repeat the first argument of <str> <n> times *)

  let repeat n str = do
  {
    let str_stream = UCStream.of_list str;

    let arg = Parser.read_argument str_stream;

    iter n

    where rec iter i = do
    {
      if i <= 0 then
        []
      else
        arg @ iter (i-1)
    }
  };

  (* select the <n>-th argument of <str> *)

  let rec select n str = do
  {
    let str_stream = UCStream.of_list str;

    iter (Parser.read_argument str_stream) n

    where rec iter arg i = do
    {
      if i <= 0 then
        arg
      else
        iter (Parser.read_argument str_stream) (i-1)
    }
  };

  match fmt with
  [ [105] -> Format.num_to_roman      (num_of_int (get_counter ps ctr))   (* "i" *)
  | [73]  -> Format.num_to_ROMAN      (num_of_int (get_counter ps ctr))   (* "I" *)
  | [97]  -> Format.num_to_alphabetic (num_of_int (get_counter ps ctr))   (* "a" *)
  | [65]  -> Format.num_to_ALPHABETIC (num_of_int (get_counter ps ctr))   (* "A" *)
  | [114 :: str] -> repeat (get_counter ps ctr) str                  (* "r{...}" *)
  | [115 :: str] -> select (get_counter ps ctr) str             (* "s{..}..{..}" *)
  | _     -> Format.num_to_arabic 10  (num_of_int (get_counter ps ctr))   (* "1" *)
  ]
};

value get_counter ps = do
{
  let fmt = arg_expanded ps;
  let ctr = Array.of_list (arg_expanded ps);

  UCStream.insert_list ps.input_stream (format_counter ps fmt ctr)
};

value expand_get_counter ps _ = do
{
  let fmt = arg_expanded ps;
  let ctr = Array.of_list (arg_expanded ps);

  let val = format_counter ps fmt ctr;

  val @ Macro.expand ps
};

(* layout *)

value shipout_pages ps = do
{
  let number = opt_int ps 0;
  let even   = Array.of_list (arg_expanded ps);
  let odd    = Array.of_list (arg_expanded ps);

  add_node ps (Node.ShipOut (location ps) even odd (max 0 number))
};

value new_page_layout ps = do
{
  let name   = Array.of_list (arg_expanded ps);
  let width  = arg_skip ps;
  let height = arg_skip ps;

  add_node ps (Node.NewLayout (location ps) name width height)
};

value next_page_layout ps = do
{
  let layout = arg_expanded ps;

  add_node ps
    (Node.CommandBox (location ps)
      (`PageCmd (Box.SetNextLayout (Array.of_list layout))))
};

value new_area ps = do
{
  let name       = Array.of_list (arg_expanded ps);
  let pos_x      = arg_skip ps;
  let pos_y      = arg_skip ps;
  let width      = arg_skip ps;
  let height     = arg_skip ps;
  let max_top    = arg_skip ps;
  let max_bot    = arg_skip ps;
  let area_type  = Array.of_list (arg_expanded ps);

  if area_type = str_galley then do
  {
    let param = arg_key_val ps;

    add_node ps
      (Node.NewArea (location ps)
         name
         pos_x pos_y width height max_top max_bot
         (`Galley (parse_galley_area_dict (location ps) param)))
  }
  else if area_type = str_float then do
  {
    let param = arg_key_val ps;

    add_node ps
      (Node.NewArea (location ps)
         name
         pos_x pos_y width height max_top max_bot
         (`Float (parse_float_area_dict (location ps) param)))
  }
  else if area_type = str_footnote then do
  {
    let param = arg_key_val ps;

    add_node ps
      (Node.NewArea (location ps)
         name
         pos_x pos_y width height max_top max_bot
         (`Footnote (parse_footnote_area_dict ps (location ps) param)))
  }
  else if area_type = str_direct then do
  {
    let code       = Parser.read_argument ps.input_stream;
    let current_ps = duplicate ps;
    let stream     = UCStream.of_list code;

    let f pi _ = do
    {
      UCStream.assign current_ps.input_stream stream;

      ParseState.set_counter current_ps str_page pi.Box.pi_page_no;

      List.iter
        (fun (m,v) -> do
          {
            ALParseState.set_string_global current_ps
              (Array.of_list (UString.of_ascii "OldMark" @ Array.to_list m))
              v;
            ALParseState.set_string_global current_ps
              (Array.of_list (UString.of_ascii "NewMark" @ Array.to_list m))
              v
          })
        (List.rev pi.Box.pi_old_marks);
      List.iter
        (fun (m,v) ->
            ALParseState.set_string_global current_ps
              (Array.of_list (UString.of_ascii "NewMark" @ Array.to_list m))
              v
        )
        (List.rev pi.Box.pi_new_marks);

      ParseState.run_parser current_ps `VBox;
    };

    add_node ps (Node.NewArea (location ps) name pos_x pos_y width height max_top max_bot (`Direct f))
  }
  else do
  {
    log_warn (location ps) "unknown area type ";
    log_uc_string area_type;
    log_string "!\n"
  }
};

value new_galley ps = do
{
  let name    = Array.of_list (arg_expanded ps);
  let measure = arg_skip ps;

  add_node ps (Node.NewGalley (location ps) name measure)
};

value begin_galley ps = do
{
  let name = Array.of_list (arg_expanded ps);

  open_node_list ps `Galley;

  add_node ps (Node.AddToGalley (location ps) name []);
};

value end_galley ps = do
{
  Mode.leave_par_mode ps;

  match close_node_list ps `Galley with
  [ []      -> log_warn (location ps) "\\endgalley without \\begingalley!"
  | [n::ns] -> match n with
    [ Node.AddToGalley loc name [] -> add_node ps (Node.AddToGalley loc name ns)
    | _                            -> assert False
    ]
  ]
};

value float_box ps = do
{
  let name = Array.of_list (arg_expanded ps);
  let body = arg_execute ps `VBox;

  add_node ps (Node.Float (location ps) name body)
};

value float_par ps = do
{
  let name = Array.of_list (arg_expanded ps);
  let body = arg_execute ps `HBox;

  add_node ps (Node.Float (location ps) name body)
};

value float_galley ps = do
{
  let name = Array.of_list (arg_expanded ps);
  let body = arg_execute ps `Galley;

  add_node ps (Node.Float (location ps) name body)
};

(*
value annotate_paragraph ps = do
{
  (* <left> and <right> should evaluate to a box of width zero. *)

  let left  = arg_execute ps `HBox;
  let right = arg_execute ps `HBox;

  let annotate env boxes = do
  {
    let (b, get) = Builder.simple_builder ();
    let _        = Evaluate.eval_node_list env b left;

    Builder.add_box_list b boxes;

    let _ = Evaluate.eval_node_list env b right;

    get ()
  };

  add_node ps (Node.Command (location ps)
    (Environment.set_par_params
      (None, None, None, None, None, None, None, Some annotate)))
};

value set_par_shape ps = do
{
  let read_next stream = do
  {
    let (a,b) = Parser.read_range stream;

    if UCStream.next_char stream = 58 then do
    {
      UCStream.remove stream 1;
      Parser.skip_spaces stream;

      (int_of_num (ceiling_num a), (int_of_num (floor_num b)), Parser.read_skip stream)
    }
    else do
    {
      log_warn (location ps) "Invalid par shape!";
      log_int (UCStream.next_char stream);
      (-1, -1, (fun _ -> num_zero))
    }
  };
  let rec parse_shape stream = do
  {
    Parser.skip_spaces stream;

    if UCStream.next_char stream >= 0 then do
    {
      let (a,b,s) = read_next stream;

      if a < 0 then
        []
      else do
      {
        Parser.skip_spaces stream;

        match UCStream.next_char stream with
        [ 44 | 59 -> do
          {
            UCStream.remove stream 1;
            [(a,b,s) :: parse_shape stream]
          }
        | (-1) -> [(a,b,s)]
        | _    -> do
          {
            log_warn (location ps) "Invalid par shape!";
            [(a,b,s)]
          }
        ]
      }
    }
    else
      []
  };

  let left_shape  = arg_expanded ps;
  let right_shape = arg_expanded ps;

  let left_indent  = parse_shape (UCStream.of_list left_shape);
  let right_indent = parse_shape (UCStream.of_list right_shape);

  let rec lookup n list = match list with
  [ []              -> fun _ -> num_zero
  | [(a,b,s) :: ls] -> if a <= n && n <= b then
                         s
                       else
                         lookup n ls
  ];

  let par_shape env n = (lookup n left_indent env, lookup n right_indent env);

  add_node ps (Node.Command (location ps)
    (Environment.set_par_params
      (None, None, None, None, None, Some par_shape, None, None)))
};
*)

value no_indent ps = do
{
  Mode.ensure_par_mode ps;

  add_node ps (Node.Command (location ps)
    (Environment.set_current_par_params
      (None, Some (fun _ -> dim_zero), None, None, None, None, None, None, None, None)))
};

value indent ps = do
{
  Mode.ensure_par_mode ps;

  add_node ps (Node.Command (location ps)
    (fun loc env -> do
      {
        let p = Galley.par_params (Environment.current_galley env);
        Environment.set_current_par_params
          (None, Some (fun _ -> p.ParLayout.par_indent), None, None, None, None, None, None, None, None)
          loc env
      }))
};

value ensure_vskip ps = do
{
  let skip = arg_skip ps;

  let f env glue = do
  {
    let min_skip     = skip env;
    let current_skip = List.fold_left
                         (fun x b -> xdim_add_dim (xdim_add_dim x b.b_height) b.b_depth)
                         xdim_zero
                         glue;

    if current_skip.xd_base </ min_skip then
      [new_glue_box dim_zero (fixed_dim (min_skip -/ current_skip.xd_base)) False True
        :: glue]
    else
      glue
  };

  add_node ps (Node.ModifyGalleyGlue (location ps) f)
};

(* misc *)

value relax _ = ();

value expand_relax ps _ = do
{
  Macro.expand ps
};

(* The default command. Note that it has to remove itself from the input stream. *)

value cc_put_char ps = do
{
  Mode.ensure_par_mode ps;

  let c = UCStream.pop ps.input_stream;

  if current_mode ps = `Math then
    add_math_char ps (get_math_code ps c)
  else
    add_node ps (Node.Letter (location ps) c)
};

value put_char ps = do
{
  Mode.ensure_par_mode ps;

  let c = arg_int ps;

  if current_mode ps = `Math then
    add_math_char ps (get_math_code ps c)
  else
    add_node ps (Node.Letter (location ps) c)
};

value expand_put_char ps _ = do
{
  let char = arg_expanded ps;

  [Parser.str_to_int char :: Macro.expand ps]
};

value put_glyph ps = do
{
  Mode.ensure_par_mode ps;

  let c = arg_int ps;

  add_node ps (Node.Glyph (location ps) c)
};

value put_math_char ps = do
{
  Mode.ensure_par_mode ps;

  let mcode = match UString.to_string (arg_expanded ps) with
  [ "letter"      -> NoMath
  | "ordinary"    -> Ordinary
  | "binop"       -> BinOp
  | "relation"    -> Relation
  | "operator"    -> Operator
  | "punct"       -> Punct
  | "open"        -> Open
  | "close"       -> Close
  | "inner"       -> Inner
  | "subscript"   -> SubScript
  | "superscript" -> SuperScript
  | x             -> do
    {
      log_warn (location ps) ("unknown math code `" ^ x ^ "'!");
      NoMath
    }
  ];

  let f1 = arg_int ps;
  let c1 = arg_int ps;
  let f2 = arg_int ps;
  let c2 = arg_int ps;

  add_math_char ps (mcode, (f1, f2), (c1, c2))
};

value nobreak_space ps = do
{
  match current_mode ps with
  [ `Paragraph | `HBox
  | `LRBox | `RLBox    -> add_node ps (Node.Space (location ps))
  | _                  -> ()
  ];
};

value break_space ps = do
{
  match current_mode ps with
  [ `Paragraph | `HBox
  | `LRBox | `RLBox    -> do
    {
      add_node ps (Node.Break (location ps) None False [] [] []);
      add_node ps (Node.Space (location ps))
    }
  | _ -> ()
  ];

  Parser.skip_blanks ps.input_stream
};

value expand_break_space ps _ = do
{
  [32 :: Macro.expand ps]
};

value newline ps = do
{
  match current_mode ps with
  [ `Paragraph | `HBox
  | `LRBox | `RLBox    -> do
    {
      add_node ps (Node.Break (location ps) None False [] [] []);
      add_node ps (Node.Space (location ps))
    }
  | _ -> ()
  ];

  Parser.skip_spaces    ps.input_stream;
  Parser.newline_to_par ps.input_stream
};

value expand_newline ps _ = do
{
  [10 :: Macro.expand ps]
};

value literal ps = do
{
  let rec read_argument i = do
  {
    if UCStream.get_char ps.input_stream i = (-1) then
      []
    else if Parser.match_substring ps.input_stream (UString.of_ascii "\\endliteral") i then do
    {
      let arg = UCStream.take ps.input_stream i;

      UCStream.remove ps.input_stream (i + 11);

      arg
    }
    else
      read_argument (i+1)
  };

  let arg = read_argument 0;

  Mode.ensure_par_mode ps;

  List.iter (fun c -> add_node ps (Node.Letter (location ps) c)) arg
};

value expand_literal ps _ = do
{
  let rec read_argument i = do
  {
    if UCStream.get_char ps.input_stream i = (-1) then
      []
    else if Parser.match_substring ps.input_stream (UString.of_ascii "\\endliteral") i then do
    {
      let arg = UCStream.take ps.input_stream i;

      UCStream.remove ps.input_stream (i + 11);

      arg
    }
    else
      read_argument (i+1)
  };

  let arg = read_argument 0;

  arg @ Macro.expand ps
};

value comment ps = do
{
  Parser.skip_comment ps.input_stream
};

value escape ps = do
{
  Macro.execute_command ps (Parser.read_token_tail ps.input_stream)
};

(* An apostrophe is handled specially in math-mode. *)

value apostrophe ps = do
{
  if current_mode ps = `Math then
    prime ps
  else
    add_node ps (Node.Letter (location ps) 39)
};

value include_file ps = do
{
  let name = arg_expanded ps;

  UCStream.include_file ps.input_stream (UString.to_string name)
};

value end_input ps = do
{
  UCStream.clear ps.input_stream
};

(* colours *)

value set_grey_colour ps = do
{
  let bound x = do
  {
    if x </ num_zero then do
    {
      log_warn (location ps) "negative colour value set to 0!";
      num_zero
    }
    else if x >/ num_one then do
    {
      log_warn (location ps) "colour value set to 1!";
      num_one
    }
    else
      x
  };

  let x = bound (arg_num ps);

  add_node ps (Node.Command (location ps) (Environment.set_colour (Graphic.Grey x)))
};

value set_rgb_colour ps = do
{
  let bound x = do
  {
    if x </ num_zero then do
    {
      log_warn (location ps) "negative colour value set to 0!";
      num_zero
    }
    else if x >/ num_one then do
    {
      log_warn (location ps) "colour value set to 1!";
      num_one
    }
    else
      x
  };

  let r = bound (arg_num ps);
  let g = bound (arg_num ps);
  let b = bound (arg_num ps);

  add_node ps (Node.Command (location ps) (Environment.set_colour (Graphic.RGB r g b)))
};

value set_cmyk_colour ps = do
{
  let bound x = do
  {
    if x </ num_zero then do
    {
      log_warn (location ps) "negative colour value set to 0!";
      num_zero
    }
    else if x >/ num_one then do
    {
      log_warn (location ps) "colour value set to 1!";
      num_one
    }
    else
      x
  };

  let c = bound (arg_num ps);
  let m = bound (arg_num ps);
  let y = bound (arg_num ps);
  let k = bound (arg_num ps);

  add_node ps (Node.Command (location ps) (Environment.set_colour (Graphic.CMYK c m y k)))
};

(* al *)

value al_declarations ps = do
{
  let rec read_argument i = do
  {
    if UCStream.get_char ps.input_stream i < 0 then
      []
    else if Parser.match_substring ps.input_stream (UString.of_ascii "\\endALdeclarations") i then do
    {
      let arg = UCStream.take ps.input_stream i;

      UCStream.remove ps.input_stream (i + 18);

      arg
    }
    else
      read_argument (i+1)
  };

  (*
    After reading the |\beginALdeclarations| command the parser might have
    inserted a |\par|. We need to remove it again.
  *)

  Parser.skip_spaces ps.input_stream;

  if Parser.match_substring ps.input_stream (UString.of_ascii "\\par") 0 then
    UCStream.remove ps.input_stream 4
  else ();

  let loc = location ps;
  let arg = read_argument 0;
  let str = UCStream.of_list arg;

  UCStream.set_location str loc False;

  try
    VM.Machine.execute_declarations ps.al_scope str
  with
  [ VM.Types.Syntax_error loc msg -> log_warn loc (UString.to_string (Array.to_list msg)) ]
};

value al_macro ps = do
{
  let loc  = location ps;
  let expr = arg_expanded ps;
  let str  = UCStream.of_list expr;

  UCStream.set_location str loc True;

  try
    let result = VM.Machine.evaluate_string_expr "\\ALmacro" ps.al_scope str;

    UCStream.insert_list ps.input_stream result
  with
  [ VM.Types.Syntax_error loc msg -> log_warn loc (UString.to_string (Array.to_list msg))
  | VM.Types.Runtime_error msg    -> log_warn (location ps) (UString.to_string (Array.to_list msg))
  ]
};

value expand_al_macro ps _ = do
{
  let loc  = location ps;
  let expr = arg_expanded ps;
  let str  = UCStream.of_list expr;

  UCStream.set_location str loc True;

  let result = try
                 VM.Machine.evaluate_string_expr "\\ALmacro" ps.al_scope str
               with
               [ VM.Types.Syntax_error loc msg -> do
                 {
                   log_warn loc (UString.to_string (Array.to_list msg));
                   []
                 }
               | VM.Types.Runtime_error msg -> do
                 {
                   log_warn (location ps) (UString.to_string (Array.to_list msg));
                   []
                 }
               ];

  result @ Macro.expand ps
};

value al_command ps = do
{
  let loc  = location ps;
  let expr = Parser.read_argument ps.input_stream;
  let str  = UCStream.of_list expr;

  UCStream.set_location str loc False;

  ALParseState.execute_ps_command "\\ALcommand" str ps
};

(* math code table *)

(* all undefined characters are ordinary symbols of family 1 *)

value default_math_codes_xx =
[|
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0))
|];

value default_math_codes_00 =
[|
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath,   (0, 0), (0, 0));     (* space *)
  (Ordinary, (0, 0), (33, 33));   (* exclamation mark *)
  (Ordinary, (0, 0), (34, 34));   (* quotation mark *)
  (NoMath,   (0, 0), (0, 0));     (* number sign *)
  (NoMath,   (0, 0), (0, 0));     (* dollar sign *)
  (NoMath,   (0, 0), (0, 0));     (* percent sign *)
  (NoMath,   (0, 0), (0, 0));     (* ampersand *)
  (NoMath,   (0, 0), (0, 0));     (* apostrophe *)
  (Open,     (0, 3), (40,  0));   (* left parenthesis *)
  (Close,    (0, 3), (41,  1));   (* right parenthesis *)
  (BinOp,    (2, 2), ( 3,  3));   (* asterisk *)
  (BinOp,    (0, 0), (43, 43));   (* plus sign *)
  (Punct,    (1, 1), (59, 59));   (* comma *)
  (BinOp,    (2, 2), ( 0,  0));   (* hyphen-minus *)
  (Ordinary, (1, 1), (58, 58));   (* period *)
  (Ordinary, (1, 1), (61, 61));   (* slash *)
  (Ordinary, (0, 0), (48, 48));   (* digit zero *)
  (Ordinary, (0, 0), (49, 49));   (* digit one *)
  (Ordinary, (0, 0), (50, 50));   (* digit two *)
  (Ordinary, (0, 0), (51, 51));   (* digit three *)
  (Ordinary, (0, 0), (52, 52));   (* digit four *)
  (Ordinary, (0, 0), (53, 53));   (* digit five *)
  (Ordinary, (0, 0), (54, 54));   (* digit six *)
  (Ordinary, (0, 0), (55, 55));   (* digit seven *)
  (Ordinary, (0, 0), (56, 56));   (* digit eight *)
  (Ordinary, (0, 0), (57, 57));   (* digit nine *)
  (Relation, (0, 0), (58, 58));   (* colon *)
  (Punct,    (0, 0), (59, 59));   (* semicolon *)
  (Relation, (1, 1), (60, 60));   (* less-than sign *)
  (Relation, (0, 0), (61, 61));   (* equals sign *)
  (Relation, (1, 1), (62, 62));   (* greater-than sign *)
  (Ordinary, (0, 0), (63, 63));   (* question mark *)
  (Ordinary, (0, 0), (64, 64));   (* commercial at *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter a *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter b *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter c *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter d *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter e *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter f *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter g *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter h *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter i *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter j *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter k *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter l *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter m *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter n *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter o *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter p *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter q *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter r *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter s *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter t *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter u *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter v *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter w *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter x *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter y *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter z *)
  (Open,     (0, 3), (91,  2));   (* left square bracket *)
  (NoMath,   (0, 0), (0, 0));     (* backslash *)
  (Close,    (0, 3), (93,  3));   (* right square bracket *)
  (NoMath,   (0, 0), (0, 0));     (* circumflex accent *)
  (NoMath,   (0, 0), (0, 0));     (* underline *)
  (Ordinary, (0, 0), (96, 96));   (* grave accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter a *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter b *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter c *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter d *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter e *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter f *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter g *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter h *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter i *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter j *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter k *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter l *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter m *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter n *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter o *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter p *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter q *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter r *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter s *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter t *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter u *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter v *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter w *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter x *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter y *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter z *)
  (NoMath,   (0, 0), (0, 0));     (* left curly bracket *)
  (Ordinary, (2, 2), (106, 106)); (* vertical line *)
  (NoMath,   (0, 0), (0, 0));     (* right curly bracket *)
  (NoMath,   (0, 0), (0, 0));     (* tilde *)
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0)); (NoMath, (0, 0), (0, 0));
  (NoMath, (0, 0), (0, 0));
  (NoMath,   (0, 0), (0, 0));     (* no-break space *)
  (NoMath,   (0, 0), (0, 0));     (* inverted exclamation mark *)
  (NoMath,   (0, 0), (0, 0));     (* cent sign *)
  (NoMath,   (0, 0), (0, 0));     (* pound sign *)
  (NoMath,   (0, 0), (0, 0));     (* currency sign *)
  (NoMath,   (0, 0), (0, 0));     (* yen sign *)
  (NoMath,   (0, 0), (0, 0));     (* broken bar *)
  (NoMath,   (0, 0), (0, 0));     (* section sign *)
  (NoMath,   (0, 0), (0, 0));     (* diaeresis *)
  (NoMath,   (0, 0), (0, 0));     (* copyright sign *)
  (NoMath,   (0, 0), (0, 0));     (* feminine ordinal indicator *)
  (NoMath,   (0, 0), (0, 0));     (* left guillemet *)
  (NoMath,   (0, 0), (0, 0));     (* not sign *)
  (NoMath,   (0, 0), (0, 0));     (* soft hyphen *)
  (NoMath,   (0, 0), (0, 0));     (* registered trade mark sign *)
  (NoMath,   (0, 0), (0, 0));     (* macron, overline *)
  (NoMath,   (0, 0), (0, 0));     (* degree sign *)
  (NoMath,   (0, 0), (0, 0));     (* plus-minus sign *)
  (NoMath,   (0, 0), (0, 0));     (* superscript two *)
  (NoMath,   (0, 0), (0, 0));     (* superscript three *)
  (NoMath,   (0, 0), (0, 0));     (* acute accent *)
  (NoMath,   (0, 0), (0, 0));     (* micro sign *)
  (NoMath,   (0, 0), (0, 0));     (* paragraph sign *)
  (NoMath,   (0, 0), (0, 0));     (* middle dot, kana conjoctive *)
  (NoMath,   (0, 0), (0, 0));     (* cedilla *)
  (NoMath,   (0, 0), (0, 0));     (* superscript one *)
  (NoMath,   (0, 0), (0, 0));     (* masculine ordinal indicator *)
  (NoMath,   (0, 0), (0, 0));     (* right guillemet *)
  (NoMath,   (0, 0), (0, 0));     (* vulgar fraction one quarter *)
  (NoMath,   (0, 0), (0, 0));     (* vulgar fraction one half *)
  (NoMath,   (0, 0), (0, 0));     (* vulgar fraction three quarters *)
  (NoMath,   (0, 0), (0, 0));     (* inverted question mark *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter a with grave accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter a with acute accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter a with circumflex accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter a with tilde *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter a with diaeresis *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter a with ring above *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter a with e *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter c with cedilla *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter e with grave accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter e with acute accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter e with circumflex accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter e with diaeresis *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter i with grave accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter i with acute accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter i with circumflex accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter i with diaeresis *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter eth *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter n with tilde *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter o with grave accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter o with acute accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter o with circumflex accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter o with tilde *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter o with diaeresis *)
  (NoMath,   (0, 0), (0, 0));     (* multiplication sign *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter o with oblique stroke *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter u with grave accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter u with acute accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter u with circumflex accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter u with diaeresis *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter y with acute accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin capital letter thorn *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter sharp s *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter a with grave accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter a with acute accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter a with circumflex accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter a with tilde *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter a with diaeresis *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter a with ring above *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter a with e *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter c with cedilla *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter e with grave accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter e with acute accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter e with circumflex accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter e with diaeresis *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter i with grave accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter i with acute accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter i with circumflex accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter i with diaeresis *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter eth *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter n with tilde *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter o with grave accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter o with acute accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter o with circumflex accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter o with tilde *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter o with diaeresis *)
  (NoMath,   (0, 0), (0, 0));     (* division sign *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter o with oblique stroke *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter u with grave accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter u with acute accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter u with circumflex accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter u with diaeresis *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter y with acute accent *)
  (NoMath,   (0, 0), (0, 0));     (* latin small letter thorn *)
  (NoMath,   (0, 0), (0, 0))      (* latin small letter y with diaeresis *)
|];

value default_math_code_table = Charmap.build
[|
  default_math_codes_00; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx;
  default_math_codes_xx; default_math_codes_xx; default_math_codes_xx; default_math_codes_xx
|];

(* binding of primitive commands *****************************************************************)

value initialise ps = do
{
  let def_expandable_cmd name cmd exp = do
  {
    ParseState.define_command ps (UString.of_ascii name)
      { ParseState.execute = cmd; ParseState.expand  = exp }
  };
  let def_unexpandable_cmd name cmd = do
  {
    def_expandable_cmd name cmd Macro.noexpand
  };
  let def_expandable_pat name cmd exp = do
  {
    ParseState.define_pattern ps (UString.of_ascii name)
      { ParseState.execute = cmd; ParseState.expand  = exp }
  };
  let def_unexpandable_pat name cmd = do
  {
    def_expandable_pat name cmd Macro.noexpand
  };
  let def_macro name body = do
  {
    def_expandable_cmd name
      (Macro.execute_macro [] (UString.of_ascii body))
      (Macro.expand_macro [] (UString.of_ascii body))
  };
  let def_macro_arg name body = do
  {
    let t = Macro.parse_arg_template ("", 0, 0) [109];   (* m *)

    def_expandable_cmd name
      (Macro.execute_macro t (UString.of_ascii body))
      (Macro.expand_macro  t (UString.of_ascii body))
  };
  let def_math_char name code family char = do
  {
    def_unexpandable_cmd name
      (fun ps -> add_math_char ps (code, (family, family), (char, char)))
  };
  let def_math_delim name code small_family small_char large_family large_char = do
  {
    def_unexpandable_cmd name
      (fun ps -> add_math_char ps (code, (small_family, large_family), (small_char, large_char)))
  };

  let def_math_acc name family char = do
  {
    def_unexpandable_cmd name
      (fun ps -> math_accent ps family char)
  };

  ParseState.set_default_char_cmd ps
    { ParseState.execute = cc_put_char;
      ParseState.expand  = Macro.noexpand };

  ALBindings.add_primitives ps.ParseState.job ps.ParseState.al_scope;

  (* commands *)

  def_unexpandable_cmd "\\begingroup"        Group.begin_group;
  def_unexpandable_cmd "\\endgroup"          Group.end_group;
  def_unexpandable_pat "{"                   Group.begin_group;
  def_unexpandable_pat "}"                   Group.end_group;

  def_unexpandable_cmd "\\setparameter"      set_param;
  def_unexpandable_cmd "\\setmathfont"       set_math_font;
  def_unexpandable_cmd "\\setmark"           set_mark;

  def_unexpandable_cmd "\\endgraf"           Mode.leave_par_mode;
  def_unexpandable_cmd "\\par"               Mode.leave_par_mode;

  def_unexpandable_cmd "\\shipoutpages"      shipout_pages;
  def_unexpandable_cmd "\\newpagelayout"     new_page_layout;
  def_unexpandable_cmd "\\nextpagelayout"    next_page_layout;
  def_unexpandable_cmd "\\newpagearea"       new_area;
  def_unexpandable_cmd "\\newgalley"         new_galley;
  def_unexpandable_cmd "\\begingalley"       begin_galley;
  def_unexpandable_cmd "\\endgalley"         end_galley;
  def_unexpandable_cmd "\\floatbox"          float_box;
  def_unexpandable_cmd "\\floatpar"          float_par;
  def_unexpandable_cmd "\\floatgalley"       float_galley;
  def_unexpandable_cmd "\\noindent"          no_indent;
  def_unexpandable_cmd "\\indent"            indent;
  def_unexpandable_cmd "\\ensurevskip"       ensure_vskip;

  (* control *)

  def_expandable_cmd   "\\relax"               relax expand_relax;
  def_unexpandable_cmd "\\definecommand"       define_command;
  def_unexpandable_cmd "\\definepattern"       define_pattern;
  def_unexpandable_cmd "\\defineenvironment"   define_environment;
  def_unexpandable_cmd "\\savecommand"         save_command;
  def_unexpandable_cmd "\\restorecommand"      restore_command;
  def_unexpandable_cmd "\\savepattern"         save_pattern;
  def_unexpandable_cmd "\\restorepattern"      restore_pattern;
  def_unexpandable_cmd "\\begin"               begin_environment;
  def_unexpandable_cmd "\\end"                 end_environment;
  def_unexpandable_cmd "\\include"             include_file;
  def_unexpandable_cmd "\\endinput"            end_input;
  def_macro            "\\jobname"             ps.job.Job.jobname;
  def_unexpandable_cmd "\\beginALdeclarations" al_declarations;
  def_expandable_cmd   "\\ALmacro"             al_macro expand_al_macro;
  def_unexpandable_cmd "\\ALcommand"           al_command;

  (* charaters *)

  def_expandable_cmd   "\\char"              put_char    expand_put_char;
  def_unexpandable_cmd "\\glyph"             put_glyph;
  def_unexpandable_cmd "\\mathchar"          put_math_char;
  def_expandable_pat   "\000"                relax       expand_relax;
  def_expandable_pat   "\001"                break_space expand_break_space;
  def_expandable_pat   "\002"                break_space expand_break_space;
  def_expandable_pat   "\003"                break_space expand_break_space;
  def_expandable_pat   "\004"                break_space expand_break_space;
  def_expandable_pat   "\005"                break_space expand_break_space;
  def_expandable_pat   "\006"                break_space expand_break_space;
  def_expandable_pat   "\007"                break_space expand_break_space;
  def_expandable_pat   "\008"                break_space expand_break_space;
  def_expandable_pat   "\009"                break_space expand_break_space;
  def_expandable_pat   "\010"                newline     expand_newline;
  def_expandable_pat   "\011"                break_space expand_break_space;
  def_expandable_pat   "\012"                break_space expand_break_space;
  def_expandable_pat   "\013"                break_space expand_break_space;
  def_expandable_pat   "\014"                break_space expand_break_space;
  def_expandable_pat   "\015"                break_space expand_break_space;
  def_expandable_pat   "\016"                break_space expand_break_space;
  def_expandable_pat   "\017"                break_space expand_break_space;
  def_expandable_pat   "\018"                break_space expand_break_space;
  def_expandable_pat   "\019"                break_space expand_break_space;
  def_expandable_pat   "\020"                break_space expand_break_space;
  def_expandable_pat   "\021"                break_space expand_break_space;
  def_expandable_pat   "\022"                break_space expand_break_space;
  def_expandable_pat   "\023"                break_space expand_break_space;
  def_expandable_pat   "\024"                break_space expand_break_space;
  def_expandable_pat   "\025"                break_space expand_break_space;
  def_expandable_pat   "\026"                break_space expand_break_space;
  def_expandable_pat   "\027"                break_space expand_break_space;
  def_expandable_pat   "\028"                break_space expand_break_space;
  def_expandable_pat   "\029"                break_space expand_break_space;
  def_expandable_pat   "\030"                break_space expand_break_space;
  def_expandable_pat   "\031"                break_space expand_break_space;
  def_expandable_pat   "\032"                break_space expand_break_space;
  def_unexpandable_pat "%"                   comment;
  def_unexpandable_pat "\\"                  escape;
  def_unexpandable_pat "'"                   apostrophe;
  def_unexpandable_pat "~"                   nobreak_space;
  def_expandable_cmd   "\\beginliteral"      literal     expand_literal;

  (* counters *)

  def_unexpandable_cmd "\\newcounter"        new_counter;
  def_unexpandable_cmd "\\setcounter"        set_counter;
  def_unexpandable_cmd "\\addtocounter"      add_to_counter;
  def_expandable_cmd   "\\getcounter"        get_counter expand_get_counter;

  ParseState.new_counter ps
    (UString.uc_string_of_ascii "year")
    (ps.job.Job.time.Unix.tm_year + 1900)
    None;
  ParseState.new_counter ps
    (UString.uc_string_of_ascii "month")
    (ps.job.Job.time.Unix.tm_mon  + 1)
    None;
  ParseState.new_counter ps
    (UString.uc_string_of_ascii "day")
    ps.job.Job.time.Unix.tm_mday
    None;
  ParseState.new_counter ps
    (UString.uc_string_of_ascii "day-of-week")
    ps.job.Job.time.Unix.tm_wday
    None;


  (* boxes *)

  def_unexpandable_cmd "\\hskip"             hskip;
  def_unexpandable_cmd "\\vskip"             vskip;
  def_unexpandable_cmd "\\kern"              kern;

  def_unexpandable_cmd "\\hbox"              (hbox `Default);
  def_unexpandable_cmd "\\lrbox"             (hbox `LR);
  def_unexpandable_cmd "\\rlbox"             (hbox `RL);
  def_unexpandable_cmd "\\vbox"              vbox;
  def_unexpandable_cmd "\\phantom"           phantom;
  def_unexpandable_cmd "\\hphantom"          hphantom;
  def_unexpandable_cmd "\\vphantom"          vphantom;
  def_unexpandable_cmd "\\hleaders"          hleaders;
  def_unexpandable_cmd "\\vadjust"           vinsert;
  def_unexpandable_cmd "\\rule"              rule;
  def_unexpandable_cmd "\\image"             image;
  def_unexpandable_cmd "\\parbox"            parbox;
  def_unexpandable_cmd "\\accent"            accent;

  def_unexpandable_cmd "\\penalty"           penalty;
  def_unexpandable_cmd "\\discretionary"     discretionary;

  (* tables *)

  def_unexpandable_cmd "\\newtableentry"     new_table_entry;
  def_unexpandable_cmd "\\newtablerow"       new_table_row;
  def_unexpandable_cmd "\\begintable"        begin_table;
  def_unexpandable_cmd "\\endtable"          end_table;

  (* fonts *)

  def_macro "\\FontFamilyRoman"        "Computer Modern Roman";
  def_macro "\\FontFamilySans"         "Computer Modern Sans Serif";
  def_macro "\\FontFamilyTypewriter"   "Computer Modern Typewriter";
  def_macro "\\FontFamilyMath"         "Computer Modern Math Italic";
  def_macro "\\FontFamilySymbols"      "Computer Modern Math Symbols";
  def_macro "\\FontFamilyExtensions"   "Computer Modern Math Extensions";
  def_macro "\\FontSeriesMedium"       "medium";
  def_macro "\\FontSeriesBold"         "bold extended";
  def_macro "\\FontShapeUpright"       "normal";
  def_macro "\\FontShapeItalic"        "italic";
  def_macro "\\FontShapeSlanted"       "slanted";
  def_macro "\\FontShapeSmallCaps"     "small caps";
  def_macro "\\FontSizeTiny"            "5";
  def_macro "\\FontSizeScript"          "7";
  def_macro "\\FontSizeFootnote"        "8";
  def_macro "\\FontSizeSmall"           "9";
  def_macro "\\FontSizeNormal"         "10";
  def_macro "\\FontSizeLargeI"         "12";
  def_macro "\\FontSizeLargeII"        "14.4";
  def_macro "\\FontSizeLargeIII"       "17.28";
  def_macro "\\FontSizeHugeI"          "20.74";
  def_macro "\\FontSizeHugeII"         "24.88";

  def_macro "\\rmfamily"      "\\setparameter{font}{ family = \\FontFamilyRoman }";
  def_macro "\\sffamily"      "\\setparameter{font}{ family = \\FontFamilySans }";
  def_macro "\\ttfamily"      "\\setparameter{font}{ family = \\FontFamilyTypewriter }";
  def_macro "\\mdseries"      "\\setparameter{font}{ series = \\FontSeriesMedium }";
  def_macro "\\bfseries"      "\\setparameter{font}{ series = \\FontSeriesBold }";
  def_macro "\\upshape"       "\\setparameter{font}{ shape = \\FontShapeUpright }";
  def_macro "\\itshape"       "\\setparameter{font}{ shape = \\FontShapeItalic }";
  def_macro "\\slshape"       "\\setparameter{font}{ shape = \\FontShapeSlanted }";
  def_macro "\\scshape"       "\\setparameter{font}{ shape = \\FontShapeSmallCaps }";
  def_macro "\\tiny"          "\\setparameter{font}{ size = \\FontSizeTiny }";
  def_macro "\\scriptsize"    "\\setparameter{font}{ size = \\FontSizeScript }";
  def_macro "\\footnotesize"  "\\setparameter{font}{ size = \\FontSizeFootnote }";
  def_macro "\\small"         "\\setparameter{font}{ size = \\FontSizeSmall }";
  def_macro "\\normalsize"    "\\setparameter{font}{ size = \\FontSizeNormal }";
  def_macro "\\large"         "\\setparameter{font}{ size = \\FontSizeLargeI }";
  def_macro "\\Large"         "\\setparameter{font}{ size = \\FontSizeLargeII }";
  def_macro "\\LARGE"         "\\setparameter{font}{ size = \\FontSizeLargeIII }";
  def_macro "\\huge"          "\\setparameter{font}{ size = \\FontSizeHugeI }";
  def_macro "\\Huge"          "\\setparameter{font}{ size = \\FontSizeHugeII }";

  def_macro "\\normalfont"    "\\setparameter{font}{family = \\FontFamilyRoman; series = \\FontSeriesMedium; shape = \\FontShapeUpright}";

  (* colours *)

  def_unexpandable_cmd "\\setgreycolour" set_grey_colour;
  def_unexpandable_cmd "\\setrgbcolour"  set_rgb_colour;
  def_unexpandable_cmd "\\setcymkcolour" set_cmyk_colour;

  (* accents *)

  def_macro_arg "\\`"         "\\accent{18}{#1}";
  def_macro_arg "\\'"         "\\accent{19}{#1}";
  def_macro_arg "\\v"         "\\accent{20}{#1}";
  def_macro_arg "\\u"         "\\accent{21}{#1}";
  def_macro_arg "\\="         "\\accent{22}{#1}";
  def_macro_arg "\\^"         "\\accent{94}{#1}";
  def_macro_arg "\\."         "\\accent{95}{#1}";
  def_macro_arg "\\H"         "\\accent{125}{#1}";
  def_macro_arg "\\~"         "\\accent{126}{#1}";
  def_macro_arg "\\\""        "\\accent{127}{#1}";

  (* math *)

  def_unexpandable_pat "$"               toggle_math;
  def_unexpandable_pat "_"               sub_script;
  def_unexpandable_pat "^"               super_script;
  def_unexpandable_cmd "\\frac"          fraction;
  def_unexpandable_cmd "\\genfrac"       general_fraction;
  def_unexpandable_cmd "\\overline"      overline;
  def_unexpandable_cmd "\\underline"     underline;
  def_unexpandable_cmd "\\left"          left_delim;
  def_unexpandable_cmd "\\middle"        mid_delim;
  def_unexpandable_cmd "\\right"         right_delim;
  def_unexpandable_cmd "\\indexposition" index_position;

  def_macro "\\limits"   "\\indexposition{vert}";
  def_macro "\\nolimits" "\\indexposition{right}";

  def_unexpandable_cmd "\\displaystyle"      (set_math_style MathLayout.Display);
  def_unexpandable_cmd "\\textstyle"         (set_math_style MathLayout.Text);
  def_unexpandable_cmd "\\scriptstyle"       (set_math_style MathLayout.Script);
  def_unexpandable_cmd "\\scriptscriptstyle" (set_math_style MathLayout.Script2);

  def_unexpandable_cmd "\\beginmath"     Mode.begin_math;
  def_unexpandable_cmd "\\endmath"       Mode.end_math;
  def_unexpandable_cmd "\\begintext"     begin_text;
  def_unexpandable_cmd "\\endtext"       end_text;

  def_unexpandable_cmd "\\mathord"   (fun ps -> set_math_code ps Box.Ordinary);
  def_unexpandable_cmd "\\mathop"    (fun ps -> set_math_code ps Box.Operator);
  def_unexpandable_cmd "\\mathbin"   (fun ps -> set_math_code ps Box.BinOp);
  def_unexpandable_cmd "\\mathrel"   (fun ps -> set_math_code ps Box.Relation);
  def_unexpandable_cmd "\\mathopen"  (fun ps -> set_math_code ps Box.Open);
  def_unexpandable_cmd "\\mathclose" (fun ps -> set_math_code ps Box.Close);
  def_unexpandable_cmd "\\mathpunct" (fun ps -> set_math_code ps Box.Punct);
  def_unexpandable_cmd "\\mathinner" (fun ps -> set_math_code ps Box.Inner);

  (* math symbols *)

  def_unexpandable_cmd "\\sqrt" (fun ps -> put_root ps 2 112 3 112);

  def_math_char "\\alpha"            Box.Ordinary 1  11;
  def_math_char "\\beta"             Box.Ordinary 1  12;
  def_math_char "\\gamma"            Box.Ordinary 1  13;
  def_math_char "\\delta"            Box.Ordinary 1  14;
  def_math_char "\\epsilon"          Box.Ordinary 1  15;
  def_math_char "\\zeta"             Box.Ordinary 1  16;
  def_math_char "\\eta"              Box.Ordinary 1  17;
  def_math_char "\\theta"            Box.Ordinary 1  18;
  def_math_char "\\iota"             Box.Ordinary 1  19;
  def_math_char "\\kappa"            Box.Ordinary 1  20;
  def_math_char "\\lambda"           Box.Ordinary 1  21;
  def_math_char "\\mu"               Box.Ordinary 1  22;
  def_math_char "\\nu"               Box.Ordinary 1  23;
  def_math_char "\\xi"               Box.Ordinary 1  24;
  def_math_char "\\pi"               Box.Ordinary 1  25;
  def_math_char "\\rho"              Box.Ordinary 1  26;
  def_math_char "\\sigma"            Box.Ordinary 1  27;
  def_math_char "\\tau"              Box.Ordinary 1  28;
  def_math_char "\\upsilon"          Box.Ordinary 1  29;
  def_math_char "\\phi"              Box.Ordinary 1  30;
  def_math_char "\\chi"              Box.Ordinary 1  31;
  def_math_char "\\psi"              Box.Ordinary 1  32;
  def_math_char "\\omega"            Box.Ordinary 1  33;
  def_math_char "\\varepsilon"       Box.Ordinary 1  34;
  def_math_char "\\vartheta"         Box.Ordinary 1  35;
  def_math_char "\\varpi"            Box.Ordinary 1  36;
  def_math_char "\\varrho"           Box.Ordinary 1  37;
  def_math_char "\\varsigma"         Box.Ordinary 1  38;
  def_math_char "\\varphi"           Box.Ordinary 1  39;
  def_math_char "\\Gamma"            Box.Ordinary 0   0;
  def_math_char "\\Delta"            Box.Ordinary 0   1;
  def_math_char "\\Theta"            Box.Ordinary 0   2;
  def_math_char "\\Lambda"           Box.Ordinary 0   3;
  def_math_char "\\Xi"               Box.Ordinary 0   4;
  def_math_char "\\Pi"               Box.Ordinary 0   5;
  def_math_char "\\Sigma"            Box.Ordinary 0   6;
  def_math_char "\\Upsilon"          Box.Ordinary 0   7;
  def_math_char "\\Phi"              Box.Ordinary 0   8;
  def_math_char "\\Psi"              Box.Ordinary 0   9;
  def_math_char "\\Omega"            Box.Ordinary 0  10;
  def_math_char "\\aleph"            Box.Ordinary 2  64;
  def_math_char "\\imath"            Box.Ordinary 1 123;
  def_math_char "\\jmath"            Box.Ordinary 1 124;
  def_math_char "\\ell"              Box.Ordinary 1  96;
  def_math_char "\\wp"               Box.Ordinary 1 125;
  def_math_char "\\Re"               Box.Ordinary 2  60;
  def_math_char "\\Im"               Box.Ordinary 2  61;
  def_math_char "\\partial"          Box.Ordinary 1  64;
  def_math_char "\\infty"            Box.Ordinary 2  49;
  def_math_char "\\prime"            Box.Ordinary 2  48;
  def_math_char "\\emptyset"         Box.Ordinary 2  59;
  def_math_char "\\nabla"            Box.Ordinary 2 114;
  def_math_char "\\surd"             Box.Operator 2 112;
  def_math_char "\\top"              Box.Ordinary 2  62;
  def_math_char "\\bot"              Box.Ordinary 2  63;
  def_math_char "\\triangle"         Box.Ordinary 2  52;
  def_math_char "\\forall"           Box.Ordinary 2  56;
  def_math_char "\\exists"           Box.Ordinary 2  57;
  def_math_char "\\neg"              Box.Ordinary 2  58;
  def_math_char "\\flat"             Box.Ordinary 1  91;
  def_math_char "\\natural"          Box.Ordinary 1  92;
  def_math_char "\\sharp"            Box.Ordinary 1  93;
  def_math_char "\\clubsuit"         Box.Ordinary 2 124;
  def_math_char "\\diamondsuit"      Box.Ordinary 2 125;
  def_math_char "\\heartsuit"        Box.Ordinary 2 126;
  def_math_char "\\spadesuit"        Box.Ordinary 2 127;
  def_math_char "\\coprod"           Box.Operator 3  96;
  def_math_char "\\bigvee"           Box.Operator 3  87;
  def_math_char "\\bigwedge"         Box.Operator 3  86;
  def_math_char "\\biguplus"         Box.Operator 3  85;
  def_math_char "\\bigcap"           Box.Operator 3  84;
  def_math_char "\\bigcup"           Box.Operator 3  83;
  def_math_char "\\intop"            Box.Operator 3  82;
  def_math_char "\\prod"             Box.Operator 3  81;
  def_math_char "\\sum"              Box.Operator 3  80;
  def_math_char "\\bigotimes"        Box.Operator 3  78;
  def_math_char "\\bigoplus"         Box.Operator 3  76;
  def_math_char "\\bigodot"          Box.Operator 3  74;
  def_math_char "\\ointop"           Box.Operator 3  72;
  def_math_char "\\bigsqcup"         Box.Operator 3  70;
  def_math_char "\\smallint"         Box.Operator 2 115;
  def_math_char "\\triangleleft"     Box.BinOp    1  47;
  def_math_char "\\triangleright"    Box.BinOp    1  46;
  def_math_char "\\bigtriangleup"    Box.BinOp    2  52;
  def_math_char "\\bigtriangledown"  Box.BinOp    2  53;
  def_math_char "\\wedge"            Box.BinOp    2  94;
  def_math_char "\\vee"              Box.BinOp    2  95;
  def_math_char "\\cap"              Box.BinOp    2  92;
  def_math_char "\\cup"              Box.BinOp    2  91;
  def_math_char "\\ddagger"          Box.BinOp    2 122;
  def_math_char "\\dagger"           Box.BinOp    2 121;
  def_math_char "\\sqcap"            Box.BinOp    2 117;
  def_math_char "\\sqcup"            Box.BinOp    2 116;
  def_math_char "\\uplus"            Box.BinOp    2  93;
  def_math_char "\\amalg"            Box.BinOp    2 113;
  def_math_char "\\diamond"          Box.BinOp    2   5;
  def_math_char "\\bullet"           Box.BinOp    2  15;
  def_math_char "\\wr"               Box.BinOp    2 111;
  def_math_char "\\div"              Box.BinOp    2   4;
  def_math_char "\\odot"             Box.BinOp    2  12;
  def_math_char "\\oslash"           Box.BinOp    2  11;
  def_math_char "\\otimes"           Box.BinOp    2  10;
  def_math_char "\\ominus"           Box.BinOp    2   9;
  def_math_char "\\oplus"            Box.BinOp    2   8;
  def_math_char "\\mp"               Box.BinOp    2   7;
  def_math_char "\\pm"               Box.BinOp    2   6;
  def_math_char "\\circ"             Box.BinOp    2  14;
  def_math_char "\\bigcirc"          Box.BinOp    2  13;
  def_math_char "\\setminus"         Box.BinOp    2 110;
  def_math_char "\\cdot"             Box.BinOp    2   1;
  def_math_char "\\ast"              Box.BinOp    2   3;
  def_math_char "\\times"            Box.BinOp    2   2;
  def_math_char "\\star"             Box.BinOp    1  63;
  def_math_char "\\propto"           Box.Relation 2  47;
  def_math_char "\\sqsubseteq"       Box.Relation 2 118;
  def_math_char "\\sqsupseteq"       Box.Relation 2 119;
  def_math_char "\\parallel"         Box.Relation 2 107;
  def_math_char "\\mid"              Box.Relation 2 106;
  def_math_char "\\mvert"            Box.BinOp    2 106;
  def_math_char "\\dashv"            Box.Relation 2  97;
  def_math_char "\\vdash"            Box.Relation 2  96;
  def_math_char "\\nearrow"          Box.Relation 2  37;
  def_math_char "\\searrow"          Box.Relation 2  38;
  def_math_char "\\nwarrow"          Box.Relation 2  45;
  def_math_char "\\swarrow"          Box.Relation 2  46;
  def_math_char "\\Leftrightarrow"   Box.Relation 2  44;
  def_math_char "\\Leftarrow"        Box.Relation 2  40;
  def_math_char "\\Rightarrow"       Box.Relation 2  41;
  def_math_char "\\leq"              Box.Relation 2  20;
  def_math_char "\\geq"              Box.Relation 2  21;
  def_math_char "\\succ"             Box.Relation 2  31;
  def_math_char "\\prec"             Box.Relation 2  30;
  def_math_char "\\approx"           Box.Relation 2  25;
  def_math_char "\\succeq"           Box.Relation 2  23;
  def_math_char "\\preceq"           Box.Relation 2  22;
  def_math_char "\\supset"           Box.Relation 2  27;
  def_math_char "\\subset"           Box.Relation 2  26;
  def_math_char "\\supseteq"         Box.Relation 2  19;
  def_math_char "\\subseteq"         Box.Relation 2  18;
  def_math_char "\\in"               Box.Relation 2  50;
  def_math_char "\\ni"               Box.Relation 2  51;
  def_math_char "\\gg"               Box.Relation 2  29;
  def_math_char "\\ll"               Box.Relation 2  28;
  def_math_char "\\not"              Box.Relation 2  54;
  def_math_char "\\leftrightarrow"   Box.Relation 2  36;
  def_math_char "\\leftarrow"        Box.Relation 2  32;
  def_math_char "\\rightarrow"       Box.Relation 2  33;
  def_math_char "\\mapstochar"       Box.Relation 2  55;
  def_math_char "\\sim"              Box.Relation 2  24;
  def_math_char "\\simeq"            Box.Relation 2  39;
  def_math_char "\\perp"             Box.Relation 2  63;
  def_math_char "\\equiv"            Box.Relation 2  17;
  def_math_char "\\asymp"            Box.Relation 2  16;
  def_math_char "\\smile"            Box.Relation 1  94;
  def_math_char "\\frown"            Box.Relation 1  95;
  def_math_char "\\leftharpoonup"    Box.Relation 1  40;
  def_math_char "\\leftharpoondown"  Box.Relation 1  41;
  def_math_char "\\rightharpoonup"   Box.Relation 1  42;
  def_math_char "\\rightharpoondown" Box.Relation 1  43;
  def_math_char "\\lhook"            Box.Relation 1  44;
  def_math_char "\\rhook"            Box.Relation 1  45;
  def_math_char "\\ldotp"            Box.Punct    1  58;
  def_math_char "\\cdotp"            Box.Punct    2   1;
  def_math_char "\\colon"            Box.Punct    0  58;

  def_math_delim "\\lmoustache"      Box.Open     3 122 3  64;
  def_math_delim "\\rmoustache"      Box.Close    3 123 3  65;
  def_math_delim "\\arrowvert"       Box.Ordinary 2 106 3  60;
  def_math_delim "\\Arrowvert"       Box.Ordinary 2 197 3  61;
  def_math_delim "\\vert"            Box.Ordinary 2 106 3  12;
  def_math_delim "\\rvert"           Box.Close    2 106 3  12;
  def_math_delim "\\lvert"           Box.Open     2 106 3  12;
  def_math_delim "\\mvert"           Box.Relation 2 106 3  12;
  def_math_delim "\\Vert"            Box.Ordinary 2 107 3  13;
  def_math_delim "\\uparrow"         Box.Relation 2  34 3 120;
  def_math_delim "\\downarrow"       Box.Relation 2  35 3 121;
  def_math_delim "\\updownarrow"     Box.Relation 2 108 3  63;
  def_math_delim "\\Uparrow"         Box.Relation 2  42 3 126;
  def_math_delim "\\Downarrow"       Box.Relation 2  43 3 127;
  def_math_delim "\\Updownarrow"     Box.Relation 2 109 3 119;
  def_math_delim "\\backslash"       Box.Ordinary 2 110 3  15;
  def_math_delim "\\rangle"          Box.Close    2 105 3  11;
  def_math_delim "\\langle"          Box.Open     2 104 3  10;
  def_math_delim "\\rbrace"          Box.Close    2 103 3   9;
  def_math_delim "\\lbrace"          Box.Open     2 102 3   8;
  def_math_delim "\\rceil"           Box.Close    2 101 3   7;
  def_math_delim "\\lceil"           Box.Open     2 100 3   6;
  def_math_delim "\\rfloor"          Box.Close    2  99 3   5;
  def_math_delim "\\lfloor"          Box.Open     2  98 3   4;

  def_math_acc "\\acute"             0  19;
  def_math_acc "\\grave"             0  18;
  def_math_acc "\\ddot"              0 127;
  def_math_acc "\\tilde"             0 126;
  def_math_acc "\\bar"               0  22;
  def_math_acc "\\breve"             0  21;
  def_math_acc "\\check"             0  20;
  def_math_acc "\\hat"               0  94;
  def_math_acc "\\vec"               1 126;
  def_math_acc "\\dot"               0  95;
  def_math_acc "\\widetilde"         3 101;
  def_math_acc "\\widehat"           3  98;

  set_math_code_table ps default_math_code_table
};


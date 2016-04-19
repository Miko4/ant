
open XNum;
open Runtime;
open Unicode.Types;
open Logging;
open Dim;
open Box;
open Page;
open PageLayout;

value tracing_page_breaks = ref False;

type area_params =
{
  galley      : uc_string;
  top_skip    : num;
  bottom_skip : num;
  min_size    : num;
  grid_size   : num            (* if non-zero then all y coordinates are rounded  *)
};

type break_state =                            (* state while breaking galleys           *)
{
  bs_height        : mutable dim;              (* current height of the area             *)
  bs_depth         : mutable dim;              (* current depth of the area              *)
  bs_height_goal   : mutable num;              (* desired height of the area             *)
  bs_best_badness  : mutable num;              (* best break found so far                *)
  bs_chosen_lines  : ListBuilder.builder box;  (* lines to put on the page               *)
  bs_pending_lines : ListBuilder.builder box;  (* glue and command-boxes following break *)

  (* marks and graphic commands found while scanning the lines *)

  bs_chosen_marks  : ListBuilder.builder
                     [= `PageCmd of page_cmd | `GfxCmd of Box.gfx_cmd ];
  bs_pending_marks : ListBuilder.builder
                     [= `PageCmd of page_cmd | `GfxCmd of Box.gfx_cmd ]
};

(*
  |process_marks <marks> <page-state>| processes the marks found in the last area and updates
  <page-state> accordingly.
*)

value rec process_marks marks page_state = match marks with
[ []      -> page_state
| [m::ms] -> match m with
  [ `PageCmd cmd -> match cmd with
    [ SetNextLayout layout ->
        try
          process_marks ms
            {
              (page_state)

              with

              ps_next_layout = DynUCTrie.find_string layout page_state.ps_layouts
            }
        with
        [ Not_found -> do
          {
            log_string "\nWarning: unknown page layout `";
            log_uc_string layout;
            log_string "'";

            process_marks ms page_state
          }
        ]
    | SetMark mark str ->
        process_marks ms
          {
            (page_state)

            with

            ps_new_marks = [(mark, str) :: page_state.ps_new_marks]
          }
    | Float f ->
        process_marks ms
          {
            (page_state)

            with

            ps_new_floats = [ f :: page_state.ps_new_floats ]
          }
    | CallPageFunction _ -> process_marks ms page_state
    ]
  | `GfxCmd _ -> process_marks ms page_state
  ]
];


(* Layout of a single area *)


(*
  |calc_top_skip <line> <max-top> <line-params>| calculates how much glue must be inserted above <line>
  such that it extends exactly <max-top> above the baseline. Similarly,
  |calc_bot_skip <line> <max-bot> <line-params>| calculates the amount needed such that <line>
  extends <max-bot> below the baseline.
*)

value calc_top_skip line max_top line_params = do
{
  let top_params = {
                     (line_params)

                     with

                     Galley.line_skip_limit = num_zero;
                     Galley.line_skip       = dim_zero
                   };
  let top_skip   = dim_sub line_params.Galley.baseline_skip max_top;

  top_params.Galley.leading
    (new_rule_box line.b_width dim_zero top_skip)
    line
    top_params
};

value calc_bot_skip line max_bot line_params = do
{
  let bot_params = {
                     (line_params)

                     with

                     Galley.line_skip_limit = num_zero;
                     Galley.line_skip       = dim_zero
                   };
  let bot_skip   = dim_sub line_params.Galley.baseline_skip max_bot;

  bot_params.Galley.leading
    line
    (new_rule_box line.b_width bot_skip dim_zero)
    bot_params
};

(*
  |calc_top_shift <line> <max-top> <line-params>| calculates how much the <line> must be shifted down
  such that it doesn't extend more than <max-top> above the baseline. Similarly,
  |calc_bot_shift <line> <max-bot> <line-params>| calculates the amount needed such that <line> doesn't
  extend more than <max-bot> below the baseline.
*)

value calc_top_shift line max_top line_params = do
{
  let skip = calc_top_skip line max_top line_params;

  dim_sub (dim_add skip line.b_height) max_top
};

value calc_bot_shift line max_bot line_params = do
{
  let skip = calc_bot_skip line max_bot line_params;

  dim_sub (dim_add skip line.b_depth) max_bot
};

(*
  |assemble_interval <lines> <interval> <line-params>| creates a vbox containing <lines> whose size is
  determined by <interval>.
*)

value assemble_interval lines (top, height, bottom) line_params = do
{
  let rec get_first_and_last_line lines = match lines with
  [ []      -> None
  | [b::bs] -> do
    {
      if is_real_box b then
        Some (b, get_last_line b bs)
      else
        get_first_and_last_line bs
    }
  ]
  and get_last_line last lines = match lines with
  [ []      -> last
  | [b::bs] -> do
    {
      if is_real_box b then
        get_last_line b bs
      else
        get_last_line last bs
    }
  ];

  match get_first_and_last_line lines with
  [ None -> new_compound_box dim_zero dim_zero (fixed_dim height) []
  | Some (first_line, last_line) -> do
    {
      let top_skip = calc_top_skip first_line (fixed_dim top)    line_params;
      let bot_skip = calc_bot_skip last_line  (fixed_dim bottom) line_params;

      shift_compound_vert
        (VBox.make_to (height +/ top +/ bottom)
                      ( [new_glue_box dim_zero top_skip False True]
                      @ lines
                      @ [new_glue_box dim_zero bot_skip False True;
                         new_glue_box dim_zero dim_zero False True]))
        (minus_num (height +/ bottom))
    }
  ]
};

(*
  |break_page (<top>, <height>, <bottom>) <lines> <line-params>|
  returns a prefix of <lines> that fits into the given interval.
*)

value break_page (top, height, bottom) lines line_params = do
{
  let calc_badness current_height height_goal =
    dim_scale_badness (adjustment_ratio current_height height_goal);

  let rec add_lines break_state boxes = match boxes with
  [ []      -> (ListBuilder.get break_state.bs_chosen_lines,
                ListBuilder.get break_state.bs_chosen_marks,
                [],
                break_state.bs_best_badness)
  | [b::bs] -> do
    {
      let skip       = calc_bot_shift b (fixed_dim bottom) line_params;
      let bh         = dim_add b.b_height skip;
      let bd         = dim_sub b.b_depth  skip;
      let new_height = dim_add (dim_add break_state.bs_height break_state.bs_depth) bh;
      let bad        = calc_badness break_state.bs_height break_state.bs_height_goal;

      if break_state.bs_height.d_base >/ break_state.bs_height_goal && bad >= infinite then do
      {
        if break_state.bs_best_badness </ infinite then
          (ListBuilder.get break_state.bs_chosen_lines,
           ListBuilder.get break_state.bs_chosen_marks,
           ListBuilder.get break_state.bs_pending_lines
             @ boxes,
           break_state.bs_best_badness)
        else do
        {
          ListBuilder.append break_state.bs_chosen_lines break_state.bs_pending_lines;
          ListBuilder.append break_state.bs_chosen_marks break_state.bs_pending_marks;

          (ListBuilder.get break_state.bs_chosen_lines,
           ListBuilder.get break_state.bs_chosen_marks,
           boxes,
           break_state.bs_best_badness)
        }
      }
      else match b.b_contents with
      [ BreakBox p _ _ _ _ -> do
        {
          if !tracing_page_breaks then do
          {
            log_string "\n#G t = ";
            log_dim break_state.bs_height;
            log_string "; g = ";
            log_num break_state.bs_height_goal;
            log_string "; b = ";
            log_num bad;
            log_string "; p = ";
            log_num p;
            log_string "; c = ";
            log_num (bad +/ p);

            if bad +/ p </ break_state.bs_best_badness then
              log_string "#"
            else ()
          }
          else ();

          if p <= minus_num infinite then do
          {
            ListBuilder.append break_state.bs_chosen_lines break_state.bs_pending_lines;
            ListBuilder.append break_state.bs_chosen_marks break_state.bs_pending_marks;

            (ListBuilder.get break_state.bs_chosen_lines,
             ListBuilder.get break_state.bs_chosen_marks,
             bs,
             minus_num infinite)
          }
          else if bad +/ p </ break_state.bs_best_badness then do
          {
            break_state.bs_height       := new_height;
            break_state.bs_depth        := bd;
            break_state.bs_best_badness := bad +/ p;

            ListBuilder.append break_state.bs_chosen_lines break_state.bs_pending_lines;
            ListBuilder.append break_state.bs_chosen_marks break_state.bs_pending_marks;

            add_lines break_state bs
          }
          else do
          {
            break_state.bs_height  := new_height;
            break_state.bs_depth   := bd;

            ListBuilder.add break_state.bs_pending_lines b;

            add_lines break_state bs
          }
        }
      | CommandBox cmd -> match cmd with
        [ (`PageCmd _) as cmd -> do
          {
            break_state.bs_height := new_height;
            break_state.bs_depth  := bd;
            ListBuilder.add break_state.bs_pending_marks cmd;
            ListBuilder.add break_state.bs_pending_lines b;

            add_lines break_state bs
          }
        | (`GfxCmd c) as cmd -> match c with
          [ Graphic.SetColour   _
          | Graphic.SetBgColour _
          | Graphic.SetAlpha    _ -> do
            {
              break_state.bs_height := new_height;
              break_state.bs_depth  := bd;

              ListBuilder.add break_state.bs_pending_marks cmd;
              ListBuilder.add break_state.bs_pending_lines b;

              add_lines break_state bs
            }
          | _ -> do
            {
              break_state.bs_height := new_height;
              break_state.bs_depth  := bd;

              ListBuilder.add break_state.bs_pending_lines b;

              add_lines break_state bs
            }
          ]
        | _ -> do
          {
            break_state.bs_height := new_height;
            break_state.bs_depth  := bd;

            ListBuilder.add break_state.bs_pending_lines b;

            add_lines break_state bs
          }
        ]
      | _ -> do
        {
          break_state.bs_height := new_height;
          break_state.bs_depth  := bd;

          ListBuilder.add break_state.bs_pending_lines b;

          add_lines break_state bs
        }
      ]
    }
  ];

  let rec filter_cmds cmds = match cmds with
  [ []        -> []
  | [{ b_contents = CommandBox c } :: cs] ->
      match c with
      [ (`PageCmd _) as cmd -> [cmd :: filter_cmds cs]
      | (`GfxCmd  _) as cmd -> [cmd :: filter_cmds cs] 
      | _                   -> filter_cmds cs
      ]
  | [_ :: cs] -> filter_cmds cs
  ];

  match discard_glue lines with
  [ (cmds, [])               -> ([], filter_cmds cmds, [], num_zero)
  | (cmds, [first_line::ls]) -> do
    {
      let first_skip = calc_top_skip first_line (fixed_dim top) line_params;

      add_lines
        {
          bs_height        = fixed_dim (minus_num top);
          bs_depth         = first_skip;
          bs_height_goal   = height;
          bs_best_badness  = infinite;
          bs_chosen_lines  = ListBuilder.make ();
          bs_pending_lines = ListBuilder.make ();
          bs_chosen_marks  = ListBuilder.make ();
          bs_pending_marks = ListBuilder.make ()
        }
        (cmds @ [first_line :: ls])
    }
  ]
};

(*
  We need to keep track of graphic state changes, so we can set the right colours at the beginning
  of an area. |update_gfx_cmds <gfx> <cmd>| updates the graphics state <gfx> according to <cmd>.
  |make_gfx_cmd_boxes <gfx>| returns a list of command boxes that set the graphics state to <gfx>.
*)

value update_gfx_cmds ((fg, bg, alpha) as gfx) cmd = match cmd with
[ `GfxCmd c -> match c with
  [ Graphic.SetColour   _ -> (Some c, bg, alpha)
  | Graphic.SetBgColour _ -> (fg, Some c, alpha)
  | Graphic.SetAlpha    _ -> (fg, bg, Some c)
  | _                     -> gfx
  ]
| _ -> gfx
];

value make_gfx_cmd_boxes (fg, bg, alpha) = do
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

value layout_interval page page_state x y interval lines line_params = do
{
  let (boxes, marks, remaining, badness) =
    break_page interval lines line_params;

  let gfx_cmds = make_gfx_cmd_boxes
                   (List.fold_left update_gfx_cmds (None, None, None) marks);

  let ps  = process_marks marks page_state;
  let ps2 =
    {
      (ps)

      with

      ps_badness  = ps.ps_badness +/ badness;
      ps_finished = ps.ps_finished && (boxes == [])
    };

  if boxes <> [] then
    (put_box_on_page
       page
       x y
       (assemble_interval boxes interval line_params),
     ps2,
     gfx_cmds @ remaining)
  else
    (page, ps2, gfx_cmds @ remaining)
};

(* |contents_from_galley <params> <page> <area> <floats> <page-state>| fills <area> from a galley. *)

value contents_from_galley params page area _floats page_state = do
{
  match DynUCTrie.lookup_string params.galley page_state.ps_galleys with
  [ None -> do
    {
      log_error ("", 0, 0) "unknown galley `";
      log_uc_string params.galley;
      log_string "'!";
      None
    }
  | Some (lines, g) -> do
    {
      let line_params = Galley.line_params g;

      iter (page, page_state, lines) (area_free_vert page area)

      where rec iter (page, page_state, lines) intervals = match intervals with
      [ [] -> do
        {
          simple_page_update
            page
            {
              (page_state)

              with

              ps_galleys = DynUCTrie.add_string params.galley (lines, g) page_state.ps_galleys
            }
        }
      | [(a,b) :: is] -> do
        {
          let b2 = b -/ params.top_skip;
          let a2 = a +/ params.bottom_skip;

          let max_y = if b2 >=/ area.as_pos_y then
                        area.as_pos_y
                      else if params.grid_size >/ num_zero then
                        area.as_pos_y
                        +/ params.grid_size
                           */ floor_num ((b2 -/ area.as_pos_y) // params.grid_size)
                      else
                        b2;
          let min_y = if a2 <=/ area.as_pos_y -/ area.as_height then
                        area.as_pos_y -/ area.as_height
                      else if params.grid_size >/ num_zero then
                        area.as_pos_y
                        +/ params.grid_size
                           */ ceiling_num ((a2 -/ area.as_pos_y) // params.grid_size)
                      else
                        a2;
          let max_top = min_num area.as_top    (b2 -/ max_y);
          let max_bot = min_num area.as_bottom (min_y -/ a2);

          if max_y -/ min_y <=/ params.min_size then
            iter (page, page_state, lines) is
          else
            iter
              (layout_interval
                page page_state
                area.as_pos_x
                max_y
                (max_top, max_y -/ min_y, max_bot)
                lines
                line_params)
              is
        }
      ]
    }
  ]
};


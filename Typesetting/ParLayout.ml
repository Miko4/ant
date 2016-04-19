
open XNum;
open Unicode.Types;
open Runtime;
open Logging;
open Dim;
open Substitute;
open FontMetric;
open Box;

value tracing_line_breaks = ref False;

value num_100 = num_of_int 100;
value num_13  = num_of_int 13;
value num_m13 = num_of_int (-13);
value num_m1  = num_of_int (-1);
value num_1_2 = num_of_ints 1 2;

type line_break_params =
{
  pre_tolerance          : num;
  tolerance              : num;
  looseness              : int;
  line_penalty           : num;
  adj_demerits           : num;
  double_hyphen_demerits : num;
  final_hyphen_demerits  : num;
  emergency_stretch      : num;
  river_demerits         : num;
  river_threshold        : num;
  simple_breaking        : bool
};

type par_params =
{
  measure           : num;
  par_indent        : dim;
  par_fill_skip     : dim;
  par_skip          : dim;
  left_skip         : dim;
  right_skip        : dim;
  par_shape         : int -> (num * num);
  pre_break         : (*int ->*) list extended_glyph_item;
  post_break        : (*int ->*) list extended_glyph_item;
  post_process_line : list box -> list box
};

(*
  |calc_adjust_ratio <line-width> <goal-width> <background-width>| calculates the adjustment
  ratio of the line.
*)

value calc_adjust_ratio line_width goal_width background_width = do
{
  let width = xdim_add line_width background_width;
  let delta = goal_width -/ width.xd_base;
  let s     = if delta </ num_zero then
                xdim_max_shrink  width
              else
                xdim_max_stretch width;

  if s >=/ infinite then
    num_zero
  else if s >/ num_zero then
    delta // s
  else if delta >=/ num_zero then
    infinite
  else
    minus_infinite
};

(*
  |calc_demerits <line_break_params> <badness> <penalty> <hyphen-demerits> <cur-hyphen> <prev-hyphen> <cur-fit> <prev-fit>|
  calculates the demerits of the line from its <badness>, the <penalty> of the break-point, the penalty
  <hyphen-demerits> for two consecutive hyphenations, two flags <cur-hyphen> and <prev-hyphen> indicating
  whether the current and the previous break-point is due to a hyphenation, and the fitness of the current
  and the previous line.
*)

value calc_demerits line_break_params badness penalty hyphen_demerits cur_hyphen prev_hyphen cur_fit prev_fit = do
{
  let alpha = if cur_hyphen && prev_hyphen then
                hyphen_demerits
              else
                num_zero;
  let beta  = line_break_params.line_penalty +/ badness;
  let gamma = if abs (cur_fit - prev_fit) > 1 then
                line_break_params.adj_demerits
              else
                num_zero;

  if penalty >=/ num_zero then
    beta */ beta +/ penalty */ penalty +/ alpha +/ gamma
  else if penalty >/ minus_infinite then
    beta */ beta -/ penalty */ penalty +/ alpha +/ gamma
  else
    beta */ beta +/ alpha +/ gamma
};

value very_loose_fit = 0; (* lines wider than their stretchability                                  *)
value loose_fit      = 1; (* lines stretched by 1/2 to 1 times their stretchability                 *)
value decent_fit     = 2; (* lines stretched or shrinkt at most 1/2 of their stretch-/shrinkability *)
value tight_fit      = 3; (* lines shrinked by 1/2 to 1 times their shrinkability                   *)

(*
  |calc_fitness <badness>| calculates the fitness of the line. <badness> is negativ if the line has
  to be shrinked.
*)

value calc_fitness badness = do
{
  if badness >=/ num_100 then
    very_loose_fit
  else if badness >=/ num_13 then
    loose_fit
  else if badness <=/ num_m13 then
    tight_fit
  else
    decent_fit
};

module Fast =
struct

(* list of break_points *)

type break_point =
{
  bp_pos        : int;   (* index into the list of boxes                  *)
  bp_prev       : int;   (* number of the previous break point            *)
  bp_line       : int;   (* the number of the line before the break point *)
  bp_fit        : int;   (* fitness of this break                         *)
  bp_hyph       : bool;  (* is this break point due to hyphenation?       *)
  bp_forced     : bool;  (* is this break forced?                         *)
  bp_demerits   : num    (* demerits of the break                         *)
};

type break_delta =
[ Break of break_point
| Delta of xdim
];

(* |log_break_point <break_point> <number>| prints a description of <break_point>. *)

value log_break_point bp n = do
{
  log_string "@@";
  log_int n;
  log_string ": line ";
  log_int bp.bp_line;
  log_string ".";
  log_int bp.bp_fit;

  if bp.bp_hyph then
    log_string "-"
  else
    ();

  log_string " t=";
  log_num bp.bp_demerits;
  log_string " -> @@";
  log_int bp.bp_prev;
  log_string "\n"
};

(*
  |update_breaks <pos> <prev> <line> <fit> <hyph> <demerits> <new_breaks>| inserts a new break-point into
  the list <new_breaks> if there isn't already a better break-point present.
*)

value update_breaks pos prev line fit hyph forced demerits new_breaks line_break_params = do
{
  iter new_breaks

  where rec iter breaks = match breaks with
  [ []        -> [Break {
                    bp_pos        = pos;
                    bp_prev       = prev;
                    bp_line       = line;
                    bp_fit        = fit;
                    bp_hyph       = hyph;
                    bp_forced     = forced;
                    bp_demerits   = demerits
                  }]
  | [b :: bs] -> match b with
    [ Delta _  -> assert False  (* <new_breaks> contains only break points *)
    | Break bp -> do
      {
        if bp.bp_line = line then do
        {
          if bp.bp_fit = fit then do
          {
            if bp.bp_demerits >/ demerits then
              [Break {
                 bp_pos        = pos;
                 bp_prev       = prev;
                 bp_line       = line;
                 bp_fit        = fit;
                 bp_hyph       = hyph;
                 bp_forced     = forced;
                 bp_demerits   = demerits
               }
               :: bs]
            else
              breaks
          }
          else if demerits >/ bp.bp_demerits +/ line_break_params.adj_demerits then
            breaks
          else if demerits +/ line_break_params.adj_demerits </ bp.bp_demerits then
            iter bs
          else
            [b :: iter bs]
        }
        else
          [b :: iter bs]
      }
    ]
  ]
};

type break_state =
{
  bs_number     : int;
  bs_position   : int;
  bs_dist       : xdim;
  bs_discarding : option xdim;
  bs_active     : list break_delta;
  bs_passive    : list break_delta
};

value break_lines loc boxes par_params line_break_params = do
{
  (* FIX: check that left- and right-skip don't contain infinite shrinkability *)

  let left_right_skip = xdim_add_dim (dim_to_xdim par_params.left_skip) par_params.right_skip;

  (* |insert_delta <dist> <active>| inserts a delta node into the active list. *)

  let insert_delta dist active = match active with
  [ []              -> [Delta dist]
  | [Delta d :: ds] -> [Delta (xdim_add d dist) :: ds]
  | [Break b :: _]  -> iter b.bp_pos active
      where rec iter pos active = match active with
      [ []              -> [Delta dist]
      | [Delta d :: ds] -> [Delta (xdim_add d dist) :: ds]
      | [Break b :: bs] -> if pos = b.bp_pos then
                             [Break b :: iter pos bs]
                           else
                             [Delta dist :: active]
      ]
  ];

  (* |inc_pos <box> <state>| moves on to the next box. *)

  let inc_pos box state =
  {
    bs_number     = state.bs_number;
    bs_position   = state.bs_position + 1;
    bs_dist       = xdim_add_dim state.bs_dist box.b_width;
    bs_discarding = None;
    bs_active     = match state.bs_discarding with
                    [ Some dist -> insert_delta dist state.bs_active
                    | None      -> state.bs_active
                    ];
    bs_passive    = state.bs_passive
  };

  (* |skip_glue <box> <state>| moves on to the next box where the current box is a glue-box. *)

  let skip_glue box state = match state.bs_discarding with
  [ Some dist -> if is_discardable_glue box then
                   {
                     bs_number     = state.bs_number;
                     bs_position   = state.bs_position + 1;
                     bs_dist       = state.bs_dist;
                     bs_discarding = Some (xdim_add_dim dist box.b_width);
                     bs_active     = state.bs_active;
                     bs_passive    = state.bs_passive
                   }
                 else
                   {
                     bs_number     = state.bs_number;
                     bs_position   = state.bs_position + 1;
                     bs_dist       = xdim_add_dim state.bs_dist box.b_width;
                     bs_discarding = None;
                     bs_active     = insert_delta dist state.bs_active;
                     bs_passive    = state.bs_passive
                   }
  | None      -> {
                   bs_number     = state.bs_number;
                   bs_position   = state.bs_position + 1;
                   bs_dist       = xdim_add_dim state.bs_dist box.b_width;
                   bs_discarding = None;
                   bs_active     = state.bs_active;
                   bs_passive    = state.bs_passive
                 }
  ];

  (*
    |try_break <threshold> <hyphen-demerits> <force-active> <background-width> <break-box> <state>|
    decides whether the break-node <break-box> at position <pos> is feasible and updates the lists of
    active and passive break-points accordingly. <threshold> contains the amount of badness which is
    allowed, <hyphen-demerits> is the penalty for two consecutive hyphenations, <background-width>
    contains the width of an empty line, and if <force-active> is |True| the functions ensures that the
    list of active nodes never becomes empty.
  *)

  let try_break threshold hyphen_demerits background_width force_active break_box state = do
  {
    let (penalty, hyph, pre_break, post_break) =
      match break_box.b_contents with
      [ BreakBox p h pre post _ -> (p,h,pre,post)
      | _                       -> assert False
      ];
    let no_break_width   = break_box.b_width;
    let pre_break_width  = HBox.calc_width pre_break;
    let post_break_width = HBox.calc_width post_break;
    let forced_break     = (penalty <=/ minus_infinite);

    let iter_breaks cur_dist n act new_breaks = do
    {
      let new_act = ListBuilder.make ();

      iter cur_dist n act new_breaks

      where rec iter cur_dist n act new_breaks = do
      {
        match act with
        [ []                -> (new_breaks, ListBuilder.get new_act, state.bs_passive)
        | [cur_act :: acts] -> match cur_act with
          [ Delta dist -> do
            {
              ListBuilder.add new_act cur_act;
              iter (xdim_add cur_dist dist) n acts new_breaks
            }
          | Break bp   -> do
            {
              let (left_indent, right_indent) = par_params.par_shape bp.bp_line;
              let width            = par_params.measure -/ left_indent -/ right_indent;
              let adjustment_ratio = calc_adjust_ratio cur_dist width background_width;
              let badness          = badness adjustment_ratio;

              if adjustment_ratio </ num_m1 then do
              {
                (* stop scanning for break-points *)

                if force_active && ListBuilder.is_empty new_act && new_breaks = [] then do
                {
                  (* prevent active list from becoming empty *)

                  if !tracing_line_breaks then do
                  {
                    if new_breaks = [] then
                      log_string "\n"
                    else ();

                    log_string "@emergency break via @@";
                    log_int n;
                    log_string "\n"
                  }
                  else ();

                  (update_breaks
                     state.bs_position
                     n
                     (bp.bp_line + 1)
                     tight_fit
                     False
                     forced_break
                     bp.bp_demerits
                     []
                     line_break_params,
                   [],
                   act @ state.bs_passive)
                }
                else
                  (new_breaks, ListBuilder.get new_act, act @ state.bs_passive)
              }
              else if badness >/ threshold then do
              {
                (* no break possible *)

                if not bp.bp_forced then do
                {
                  ListBuilder.add new_act cur_act;
                  iter cur_dist (n - 1) acts new_breaks
                }
                else do
                {
                  (* don't scan past a forced break *)

                  ListBuilder.add new_act cur_act;

                  (new_breaks, ListBuilder.get new_act, acts @ state.bs_passive)
                }
              }
              else do
              {
                (* break possible *)

                let fit            = calc_fitness
                                       (if adjustment_ratio </ num_zero then
                                          minus_num badness
                                        else
                                          badness);
                let demerits       = (*if forced_break then
                                       num_zero
                                     else *)
                                       calc_demerits
                                         line_break_params
                                         badness
                                         penalty
                                         hyphen_demerits
                                         hyph bp.bp_hyph
                                         fit  bp.bp_fit;
                let total_demerits = bp.bp_demerits +/ demerits;

                if !tracing_line_breaks then do
                {
                  if new_breaks = [] then
                    log_string "\n"
                  else ();

                  log_string "@break via @@";
                  log_int n;
                  log_string " b=";
                  log_num badness;
                  log_string " p=";
                  log_num penalty;
                  log_string " d=";
                  log_num demerits;
                  log_string "\n"
                }
                else ();

                ListBuilder.add new_act cur_act;

                iter cur_dist (n - 1) acts
                     (update_breaks
                       state.bs_position
                       n
                       (bp.bp_line + 1)
                       fit
                       hyph
                       forced_break
                       total_demerits
                       new_breaks
                       line_break_params)
              }
            }
          ]
        ]
      }
    };

    let (new_act, act, pas) =
        iter_breaks (xdim_add_dim state.bs_dist pre_break_width)
                    state.bs_number state.bs_active [];
    do
    {
      if !tracing_line_breaks then do
      {
        ignore
          (List.fold_right
            (fun bp n -> do
              {
                match bp with
                [ Break b -> log_break_point b n
                | _       -> ()
                ];
                n + 1
              })
            new_act
            (state.bs_number + 1)
          )
      }
      else ();

      if new_act = [] then
        {
          bs_number     = state.bs_number;
          bs_position   = state.bs_position + 1;
          bs_dist       = xdim_add_dim state.bs_dist no_break_width;
          bs_discarding = state.bs_discarding;
          bs_active     = act;
          bs_passive    = pas
        }
      else do
      {
        let inter_dist = xdim_sub_dim (xdim_add_dim state.bs_dist no_break_width) post_break_width;

        if post_break = [] then
          {
            bs_number     = state.bs_number + List.length new_act;
            bs_position   = state.bs_position + 1;
            bs_dist       = xdim_zero;
            bs_discarding = Some xdim_zero;
            bs_active     = new_act @ match state.bs_discarding with
                                      [ Some dist -> [Delta (xdim_add inter_dist dist)]
                                      | None      -> [Delta inter_dist]
                                      ]
                                    @ act;
            bs_passive    = pas
          }
        else
          {
            bs_number     = state.bs_number + List.length new_act;
            bs_position   = state.bs_position + 1;
            bs_dist       = dim_to_xdim post_break_width;
            bs_discarding = None;
            bs_active     = new_act @ match state.bs_discarding with
                                      [ Some dist -> [Delta (xdim_add inter_dist dist)]
                                      | None      -> [Delta inter_dist]
                                      ]
                                    @ act;
            bs_passive    = pas
          }
      }
    }
  };

  (* |split_lines <cur-break> <breaks> <boxes>| splits the list of boxes into the individual lines. *)

  let split_lines cur_break breaks boxes = do
  {
    let get_subrange arr from_pos to_pos = do
    {
      iter [] to_pos

      where rec iter result i = do
      {
        if i < from_pos then
          result
        else
          iter [arr.(i) :: result] (i - 1)
      }
    };

    let boxes = Array.of_list boxes;

    iter cur_break []

    where rec iter cur_break lines = do
    {
      if cur_break.bp_pos = 0 then
        lines
      else do
      {
        let prev = breaks.(cur_break.bp_prev);

        let (post_break, first_pos) =
          match boxes.(prev.bp_pos).b_contents with
          [ BreakBox _ _ _ p _ -> (p, prev.bp_pos + 1)
          | _                  -> ([], prev.bp_pos)
          ];
        let pre_break = match boxes.(cur_break.bp_pos).b_contents with
          [ BreakBox _ _ p _ _ -> p
          | _                  -> []
          ];

        let line             = get_subrange boxes first_pos (cur_break.bp_pos - 1);
        let (cmds, new_line) = discard_glue (post_break @ (remove_breaks line) @ pre_break);

        iter prev [cmds @ new_line :: lines]
      }
    }
  };

  (*
    |check_result <boxes> <state>| tests whether there is a feasible break-point at the end of the
     paragraph.
  *)

  let check_result boxes state = do
  {
    let end_pos = List.length boxes - 1;

    let rec find_best break_points = do
    {
      iter (Array.length break_points - 2) (Array.length break_points - 1)

      where rec iter i best = do
      {
        if i < 0 then
          break_points.(best)
        else if break_points.(i).bp_pos < end_pos then
          break_points.(best)
        else if break_points.(i).bp_demerits </ break_points.(best).bp_demerits then
          iter (i - 1) i
        else
          iter (i - 1) best
      }
    };

    let rec find_line best looseness break_points = do
    {
      iter (Array.length break_points - 1) best

      where rec iter i best = do
      {
        if i < 0 then
          best
        else do
        {
          let delta = break_points.(i).bp_line - best.bp_line;

          if break_points.(i).bp_pos < end_pos then
            best
          else if looseness = 0 then
            best
          else if looseness > 0 then do
          {
            if delta > 0 && delta <= looseness then
              iter (looseness - delta) break_points.(i)
            else
              iter looseness best
          }
          else do
          {
            if delta < 0 && delta >= looseness then
              iter (looseness - delta) break_points.(i)
            else
              iter looseness best
          }
        }
      }
    };

    let breaks = Array.of_list
                   (List.fold_left
                     (fun l bp -> match bp with
                      [ Break b -> [b :: l]
                      | _       -> l
                      ])
                     []
                     (state.bs_active @ state.bs_passive));
    let num_breaks = Array.length breaks;

    if num_breaks = 0 then
      None
    else do
    {
      let best_break =
        if line_break_params.looseness = 0 then
          find_best breaks
        else
          find_line (find_best breaks) line_break_params.looseness breaks;

      if breaks.(num_breaks - 1).bp_pos <> end_pos then
        None
      else
        Some (split_lines best_break breaks boxes)
    }
  };

  (*
    |pass <tolerance> <left-right-skip> <allow-hyphen> <force-active> <state> <boxes>|
    is the main loop of the algorithm. It scanns the paragraph and checks every possible
    break-point.
  *)

  let pass tolerance left_right_skip allow_hyphen force_active state par_boxes = do
  {
    iter par_boxes state

    where rec iter boxes state = match boxes with
    [ []      -> check_result par_boxes state
    | [b::bs] -> do
      {
        if state.bs_active = [] && not force_active then
          None
        else do
        {
          if !tracing_line_breaks then
            log_box b
          else ();

          match b.b_contents with
          [ BreakBox _ h _ _ _ -> do
            {
              if not h || allow_hyphen then
                iter bs (try_break
                          tolerance
                          (if bs = [] then
                             line_break_params.final_hyphen_demerits
                           else
                             line_break_params.double_hyphen_demerits)
                          left_right_skip
                          force_active
                          b
                          state)
              else
                if is_real_box b then
                  iter bs (inc_pos b state)
                else
                  iter bs { (state) with bs_position = state.bs_position + 1 }
            }
          | GlueBox _ _        -> iter bs (skip_glue b state)
          | _                  -> do
            {
              if is_real_box b then
                iter bs (inc_pos b state)
              else
                iter bs { (state) with bs_position = state.bs_position + 1 }
            }
          ]
        }
      }
    ]
  };

  let initial_state =
  {
    bs_number     = 0;
    bs_position   = 0;
    bs_dist       = xdim_zero;
    bs_discarding = None;
    bs_active     = [Break {
                       bp_pos        = 0;
                       bp_prev       = 0;
                       bp_line       = 0;
                       bp_fit        = decent_fit;
                       bp_hyph       = False;
                       bp_forced     = True;
                       bp_demerits   = num_zero
                     }];
    bs_passive    = []
  };

  if !tracing_line_breaks then
    log_info loc "@firstpass\n"
  else ();

  match pass line_break_params.pre_tolerance left_right_skip False False initial_state boxes with
  [ Some x -> x
  | None   -> do
    {
      if !tracing_line_breaks then
        log_info loc "@secondpass\n"
      else ();

      match pass line_break_params.tolerance left_right_skip True False initial_state boxes with
      [ Some x -> x
      | None   -> do
        {
          if !tracing_line_breaks then
            log_info loc "@emergencypass\n"
          else ();

          match
            pass line_break_params.tolerance
                 (xdim_add_dim
                    left_right_skip
                    { d_base           = num_zero;
                      d_stretch_factor = line_break_params.emergency_stretch;
                      d_stretch_order  = 1;
                      d_shrink_factor  = num_zero;
                      d_shrink_order   = 0
                    })
                 True True
                 initial_state boxes
          with
          [ Some x -> x
          | None   -> assert False
          ]
        }
      ]
    }
  ]
};

end;

module Good =
struct

(* line breaking *)

type break_graph =
{
  bg_items             : array extended_glyph_item;
  bg_breaks            : array int;
  bg_left_right_skip   : xdim;
  bg_threshold         : num;
  bg_allow_breaks      : bool;
  bg_par_params        : par_params;
  bg_line_break_params : line_break_params;
  bg_hyphen_params     : JustHyph.hyphen_params
};

value make_break_graph items threshold left_right_skip allow_breaks par_params line_break_params hyphen_params = do
{
  let num_breaks =
    Array.fold_left
      (fun n i -> match i with
       [ `Break (_, h, _, _, _) ->
           if not h || allow_breaks then
             n+1
           else
             n
       | _ -> n
       ])
      0
      items;
  let breaks = Array.make num_breaks 0;

  Array.fold_left
    (fun (n,k) i -> match i with
     [ `Break (_, h, _, _, _) -> do
       {
         if not h || allow_breaks then do
         {
           breaks.(n) := k;
           (n+1, k+1)
         }
         else
           (n, k+1)
       }
     | _ -> (n, k+1)
     ])
    (0, 0)
    items;

  (* FIX: check that left- and right-skip don't contain infinite shrinkability *)
  {
    bg_items             = items;
    bg_breaks            = breaks;
    bg_left_right_skip   = left_right_skip;
    bg_threshold         = threshold;
    bg_allow_breaks      = allow_breaks;
    bg_par_params        = par_params;
    bg_line_break_params = line_break_params;
    bg_hyphen_params     = hyphen_params
  }
};

type break_point =
{
  bp_previous : break_point;    (* the previous break point                      *)
  bp_line_no  : int;            (* the number of the line before the break point *)
  bp_line     : list box;       (* the line before the break point               *)
  bp_fit      : int;            (* fitness of this break                         *)
  bp_rivers   : list (num * num * num);  (* <from-x> <to-x> <demerits>           *)
  bp_hyph     : bool;           (* is this break point due to hyphenation?       *)
  bp_forced   : bool;           (* is this break forced?                         *)
  bp_demerits : num             (* demerits of the break                         *)
};

value is_forced_break_point graph bp = match graph.bg_items.(graph.bg_breaks.(bp)) with
[ `Break (p, _, _, _, _) -> p <=/ minus_infinite
| _                      -> assert False
];

value insert_break_point graph break_points new_bp = do
{
  iter break_points

  where rec iter break_points = match break_points with
  [ []      -> [new_bp]
  | [b::bs] -> do
    {
      (* We keep <break_points> sorted (lexicographically w.r.t. |bp_line_no| and |bp_fit|). *)

      if b.bp_line_no < new_bp.bp_line_no then
        [b :: iter bs]
      else if b.bp_line_no > new_bp.bp_line_no then
        [new_bp :: break_points]
      else do
      {
        if b.bp_fit = new_bp.bp_fit then do
        {
          (* Take the better one. *)

          if b.bp_demerits <=/ new_bp.bp_demerits then
            break_points
          else
            [new_bp :: bs]
        }
        (* Optimisation: Taking a predecessor with a different fit value increases the demerits
           at most by |adj_demerits|. If the difference is greater than this value then we can
           discard one of the break points.
           FIX: The next two tests need to be adapted if we add river detection.
        *)
        else if new_bp.bp_demerits >/ b.bp_demerits +/ graph.bg_line_break_params.adj_demerits then
          break_points
        else if new_bp.bp_demerits +/ graph.bg_line_break_params.adj_demerits </ b.bp_demerits then
          iter bs
        else if b.bp_fit < new_bp.bp_fit then
          [b :: iter bs]
        else (* b.bp_fit > new_bp.bp_fit *)
          [new_bp :: break_points]
      }
    }
  ]
};

value calc_rivers params adj_ratio width last_rivers current_rivers = do
{
  if params.river_demerits <=/ num_zero then
    (num_zero, [])                             (* Abort early. *)
  else do
  {
    let scale d = do
    {
      (dim_scale
         (xdim_select_order d width.d_stretch_order width.d_shrink_order)
         adj_ratio).d_base
    };

    let rec scale_rivers x rivers = match rivers with
    [ []  -> assert False              (* <rivers> is of odd length. *)
    | [_] -> []
    | [a; b :: rs] -> do
      {
        let delta = scale a;         (* distance to start of white space *)
        let width = scale b;         (* width of white space             *)
        let right = delta +/ width;  (* position of right border         *)

        (* Ignore tiny white space. *)

        if width >/ params.river_threshold then
          [(x +/ delta, right) :: scale_rivers right rs]
        else
          scale_rivers right rs
      }
    ];
    (* FIX: add left-skip and left-indent *)
    let scaled_rivers = scale_rivers num_zero current_rivers;

    let result = ListBuilder.make ();

    merge_rivers num_zero last_rivers scaled_rivers

    where rec merge_rivers demerits last current = match current with
    [ []            -> (demerits, ListBuilder.get result)
    | [(a,b) :: cs] -> do
      {
        iter last

        where rec iter last = match last with
        [ [] -> do
          {
            List.iter
              (fun (a,b) ->
                ListBuilder.add result (a,b,num_zero))
              current;

            (demerits, ListBuilder.get result)
          }
        | [(x,y,d) :: zs] -> do
          {
            if y <=/ a then
              iter zs
            else if x >=/ b then do
            {
              ListBuilder.add result (a,b,num_zero);
              merge_rivers demerits last cs
            }
            else do
            {
              let u = max_num a x;
              let v = min_num b y;

              let new_dem = d +/ (v -/ u) */ params.river_demerits;

              ListBuilder.add result (a,b,new_dem);
              merge_rivers (demerits +/ new_dem) last cs
            }
          }
        ]
      }
    ]
  }
};

value calc_line_demerits graph prev_bp is_final hyph fit badness penalty = do
{
  let hyphen_demerits = if is_final then
                          graph.bg_line_break_params.final_hyphen_demerits
                        else
                          graph.bg_line_break_params.double_hyphen_demerits;
  let demerits        = calc_demerits
                          graph.bg_line_break_params
                          badness
                          penalty
                          hyphen_demerits
                          hyph prev_bp.bp_hyph
                          fit  prev_bp.bp_fit;
  prev_bp.bp_demerits +/ demerits
};

value make_break_point graph prev_bp line rivers width is_final forced_break hyph adj_ratio badness penalty = do
{
  let fit =
    calc_fitness
      (if fst adj_ratio </ num_zero then
         minus_num badness
       else
         badness);
  let (river_demerits, new_rivers) =
    calc_rivers graph.bg_line_break_params adj_ratio width prev_bp.bp_rivers rivers;
  let demerits =
    river_demerits +/ calc_line_demerits graph prev_bp is_final hyph fit badness penalty;

  if !tracing_line_breaks then do
  {
    List.iter log_box line;
(*    List.iter long_dump_box line;*)
    log_string "\n@break";
(*    log_int n;*)
    log_string ": line ";
    log_int (prev_bp.bp_line_no + 1);
    log_string ".";
    log_int fit;

    if hyph then log_string "-" else ();

    log_string " b=";
    log_num badness;
    log_string " p=";
    log_num penalty;
    log_string " t=";
    log_num demerits;
    log_string "\n"
  }
  else ();

  {
    bp_previous = prev_bp;
    bp_line_no  = prev_bp.bp_line_no + 1;
    bp_line     = line;
    bp_fit      = fit;
    bp_rivers   = new_rivers;
    bp_hyph     = hyph;
    bp_forced   = forced_break;
    bp_demerits = demerits
  }
};

type partial_line =
{
  pl_prefix       : list box;                  (* The prefix of the line that is known to not *)
                                               (* change anymore (in reversed order).         *)
  pl_rivers       : list xdim;                 (* The rivers found in the prefix.             *)
  pl_position     : int;                       (* The position in |graph.bg_items| where the  *)
                                               (* prefix ends.                                *)
  pl_right_margin : list extended_glyph_item;  (* The margin glyph corresponding to the last  *)
                                               (* item in |pl_prefix|.                        *)
  pl_width        : xdim                       (* The width of the prefix.                    *)
};

value compute_line graph partial_line previous current = do
{
  let return_result partial_line new_pos right_margin boxes1 boxes2 = do
  {
    if graph.bg_line_break_params.river_demerits <=/ num_zero then do
    {
      (* We do not care about rivers. *)

      let width1 = HBox.calc_xwidth boxes1;
      let width2 = HBox.calc_xwidth boxes2;

      let new_partial_line =
        {
          pl_prefix       = boxes1 @ partial_line.pl_prefix;
          pl_rivers       = [];
          pl_position     = new_pos;
          pl_right_margin = right_margin;
          pl_width        = xdim_add partial_line.pl_width width1
        };

      (List.rev_append new_partial_line.pl_prefix boxes2,
       [],
       xdim_add new_partial_line.pl_width width2,
       new_partial_line)
    }
    else do
    {
      (* compute rivers *)

      let rec rev_append_rivers r1 r2 = match r1 with
      [ []      -> assert False  (* Rivers have odd length. *)
      | [x::xs] -> match r2 with
          [ []      -> assert False
          | [y::ys] -> List.rev_append xs [xdim_add x y :: ys]
          ]
      ];

      let (width1, rivers1) = HBox.calc_width_and_glue boxes1;
      let (width2, rivers2) = HBox.calc_width_and_glue boxes2;

      let rivers = rev_append_rivers
                     partial_line.pl_rivers
                     (rev_append_rivers rivers1 rivers2);

      let new_partial_line =
        {
          pl_prefix       = boxes1 @ partial_line.pl_prefix;
          pl_rivers       = rev_append_rivers rivers1 partial_line.pl_rivers;
          pl_position     = new_pos;
          pl_right_margin = right_margin;
          pl_width        = xdim_add partial_line.pl_width width1
        };

      (List.rev_append new_partial_line.pl_prefix boxes2,
       rivers,
       xdim_add new_partial_line.pl_width width2,
       new_partial_line)
    }
  };

  let make_margin_glyph border_item = match border_item with
  [ `Glyph (_,f) -> [`Glyph (Border Margin, f)]
  | _            -> []
  ];
  let prefix_with_list list array = match list with
  [ []      -> array
  | [x::xs] -> do
    {
      let l = List.length list;
      let a = Array.make (l + Array.length array) x;

      List.fold_left
        (fun i x -> do { a.(i) := x; i + 1 })
        1
        xs;

      for i = 0 to Array.length array - 1 do
      {
        a.(l + i) := array.(i);
      };

      a
    }
  ];
  let postfix_with_list list array = match list with
  [ []      -> array
  | [x::xs] -> do
    {
      let l = List.length list;
      let a = Array.make (l + Array.length array) x;

      List.fold_left
        (fun i x -> do { a.(i + Array.length array) := x; i + 1 })
        1
        xs;

      for i = 0 to Array.length array - 1 do
      {
        a.(i) := array.(i);
      };

      a
    }
  ];

  if partial_line.pl_position < 0 then do
  {
    (* no precomputed information available *)

    let post_break = match graph.bg_items.(previous) with
    [ `Break (_, _, _, p, _) -> prefix_with_list graph.bg_par_params.post_break p
                                (* FIX: post_break should depend on the line number *)
    | _                      -> assert False
    ];
    let pre_break = match graph.bg_items.(current) with
    [ `Break (_, _, p, _, _) -> postfix_with_list graph.bg_par_params.pre_break p
                                (* FIX: pre_break should depend on the line number *)
    | _                      -> assert False
    ];

    let (cmds1, pos1) =
      Compose.discard_glue_array
        (previous + 1)
        (current - 1)
        graph.bg_items;

    (* compute margin glyphs for margin kerning *)

    let left_margin = match Substitute.first_real_item post_break 0 (Array.length post_break - 1) with
    [ Some i -> make_margin_glyph i
    | None   -> match Substitute.first_real_item graph.bg_items (previous + 1) (current - 1) with
        [ Some i -> make_margin_glyph i
        | None   -> match Substitute.first_real_item pre_break 0 (Array.length pre_break - 1) with
            [ Some i -> make_margin_glyph i
            | None   -> []
            ]
        ]
    ];

    if pos1 >= current then do
    {
      (* All of the regular items were discarded. To keep things
         simple we do not precompute a partial line in this case. *)

      let right_margin = match Substitute.last_real_item pre_break 0 (Array.length pre_break - 1) with
      [ Some i -> make_margin_glyph i
      | None   -> match Substitute.last_real_item graph.bg_items (previous + 1) (current - 1) with
          [ Some i -> make_margin_glyph i
          | None   -> match Substitute.last_real_item post_break 0 (Array.length post_break - 1) with
              [ Some i -> make_margin_glyph i
              | None   -> []
              ]
          ]
      ];

      let (cmds2, pos2) =
        Compose.discard_glue_array
          0
          (Array.length pre_break - 1)
          pre_break;
      let boxes = XList.append_sub_array pre_break pos2 (Array.length pre_break - 1) right_margin;

      let line =
        XList.append_sub_array
          post_break
          0
          (Array.length post_break - 1)
          (cmds1 @ cmds2 @ boxes);
      let boxes = List.map extended_item_to_box (left_margin @ line);

      if graph.bg_line_break_params.river_demerits <=/ num_zero then do
      {
        let width = HBox.calc_xwidth boxes;

        (boxes, [], width, partial_line)
      }
      else do
      {
        let (width, rivers) = HBox.calc_width_and_glue boxes;

        (boxes, rivers, width, partial_line)
      }
    }
    else do
    {
      let items1 =
        XList.from_sub_array graph.bg_items pos1 (current - 1);
      let line_prefix =
          left_margin
        @ XList.append_sub_array
            post_break
            0
            (Array.length post_break - 1)
            (cmds1 @ items1);
      let (prefix, len, rest) =
        JustHyph.add_lig_kern_iterative_list False [] line_prefix;
      let prefix_pos = pos1 + len - Array.length post_break - List.length cmds1 - List.length left_margin;

      let prefix_right_margin = match Substitute.last_real_item graph.bg_items pos1 (prefix_pos - 1) with
          [ Some i -> make_margin_glyph i
          | None   -> match Substitute.last_real_item post_break 0 (Array.length post_break - 1) with
              [ Some i -> make_margin_glyph i
              | None   -> []
              ]
          ];
      let right_margin = match Substitute.last_real_item pre_break 0 (Array.length pre_break - 1) with
      [ Some i -> make_margin_glyph i
      | None   -> match Substitute.last_real_item graph.bg_items prefix_pos (current - 1) with
          [ Some i -> make_margin_glyph i
          | None   -> prefix_right_margin
          ]
      ];

      let suffix =
        JustHyph.add_lig_kern False
          (rest @ XList.append_sub_array
                    pre_break
                    0
                    (Array.length pre_break-1)
                    right_margin);
      let boxes1 = List.map simple_item_to_box prefix;
      let boxes2 = List.map simple_item_to_box suffix;

      return_result partial_line prefix_pos prefix_right_margin boxes1 boxes2
    }
  }
  else do
  {
    (* we have already precomputed a prefix of the line *)

    let pre_break = match graph.bg_items.(current) with
    [ `Break (_, _, p, _, _) -> postfix_with_list graph.bg_par_params.pre_break p
                                (* FIX: pre_break should depend on the line number *)
    | _                      -> assert False
    ];

    let (new_prefix, prefix_pos) =
      JustHyph.add_lig_kern_iterative_array
        False
        []
        partial_line.pl_position
        (current - 1)
        graph.bg_items;

    let prefix_right_margin = match Substitute.last_real_item graph.bg_items partial_line.pl_position (prefix_pos - 1) with
        [ Some i -> make_margin_glyph i
        | None   -> partial_line.pl_right_margin
        ];
    let right_margin = match Substitute.last_real_item pre_break 0 (Array.length pre_break - 1) with
    [ Some i -> make_margin_glyph i
    | None   -> match Substitute.last_real_item graph.bg_items prefix_pos (current - 1) with
        [ Some i -> make_margin_glyph i
        | None   -> prefix_right_margin
        ]
    ];

    let line_suffix =
      JustHyph.add_lig_kern
        False
        (XList.append_sub_array
          graph.bg_items
          prefix_pos
          (current - 1)
          (XList.append_sub_array
            pre_break
            0
            (Array.length pre_break-1)
            right_margin));
    let boxes1 = List.map simple_item_to_box new_prefix;
    let boxes2 = List.map simple_item_to_box line_suffix;

    return_result partial_line prefix_pos prefix_right_margin boxes1 boxes2
  }
};

value update_breaks graph previous_breaks partial_line previous current breaks = do
{
  let new_line = ref None;

  iter (-1) breaks previous_breaks

  where rec iter last_line_no new_breaks breaks = match breaks with
  [ [] -> match !new_line with
          [ None               -> (new_breaks, Some partial_line)
          | Some (_, _, _, pl) -> (new_breaks, Some pl)
          ]
  | [b::bs] -> do
    {
      if b.bp_line_no = last_line_no then
        (* We have already considered this line. Since the result depends
           only on the line number (via par-shape) we do not need to compute
           the demerits again. *)
        iter last_line_no new_breaks bs
      else do
      {
        let (left_indent, right_indent) =
          graph.bg_par_params.par_shape b.bp_line_no;
        let goal_width = graph.bg_par_params.measure -/ right_indent -/ left_indent;

        if goal_width </ xdim_min_value (xdim_add partial_line.pl_width graph.bg_left_right_skip) &&
           previous + 1 < current then
          (* Abort early since the line is too long. *)
          iter b.bp_line_no new_breaks bs
        else do
        {
          let previous_pos = graph.bg_breaks.(previous);
          let current_pos  = graph.bg_breaks.(current);

          let (penalty, hyph) = match graph.bg_items.(current_pos) with
          [ `Break (p, h, _, _, _) -> (p, h)
          | _                      -> assert False
          ];
          let forced_break = penalty <=/ minus_infinite;

          let (line, rivers, line_width, _) = match !new_line with
          [ Some l -> l
          | None   -> do
            {
              let l = compute_line graph partial_line previous_pos current_pos;

              !new_line := Some l;
              l
            }
          ];

          let total_width =
            xdim_to_dim
              (xdim_add line_width graph.bg_left_right_skip);
          let adj_ratio = adjustment_ratio total_width goal_width;
          let badness   = dim_scale_badness adj_ratio;
          let badness2  = if badness >/ graph.bg_threshold
                          && total_width.d_base >/ goal_width
                          && previous + 1 = current then
                            (* If already the first break point is too far away there is no proper line-breaking
                               solution. To get some reasonable output we simply allow the break. *)
                            num_zero
                          else
                            badness;

          if badness2 >/ graph.bg_threshold then
            (* break not possible *)
            iter b.bp_line_no new_breaks bs
          else do
          {
            (* add a new break point to the list *)
            let new_bp =
              make_break_point
                graph b line rivers total_width
                (current = Array.length graph.bg_breaks - 1)
                forced_break hyph adj_ratio badness2 penalty;

            iter b.bp_line_no (insert_break_point graph new_breaks new_bp) bs
          }
        }
      }
    }
  ]
};

value compute_best_break graph breaks = do
{
  let num_breaks = Array.length breaks;

  match breaks.(num_breaks - 1) with
  [ []      -> None
  | [b::bs] -> do
    {
      let rec find_best best breaks = match breaks with
      [ []      -> best
      | [b::bs] -> if b.bp_demerits </ best.bp_demerits then
                     find_best b bs
                   else
                     find_best best bs
      ];
      let rec get_break best looseness breaks = match breaks with
      [ []      -> best
      | [b::bs] -> do
        {
          let delta = b.bp_line_no - best.bp_line_no;

          if delta = 0 then do
          {
            if b.bp_demerits </ best.bp_demerits then
              get_break b looseness bs
            else
              get_break best looseness bs
          }
          else do
          {
            if (delta > 0 && delta <= looseness) ||
               (delta < 0 && delta >= looseness) then
              get_break b (looseness - delta) bs
            else
              get_break best looseness bs
          }
        }
      ];

      Some
        (get_break
          (find_best b bs)
          graph.bg_line_break_params.looseness
          breaks.(num_breaks - 1))
    }
  ]
};

value get_lines bp = do
{
  iter bp []

  where rec iter bp lines = do
  {
    if bp.bp_line_no > 0 then
      iter bp.bp_previous [bp.bp_line :: lines]
    else
      lines
  }
};

value break_lines loc items par_params line_break_params hyphen_params = do
{
  let rec initial_bp =
    {
      bp_previous = initial_bp;
      bp_line_no  = 0;
      bp_line     = [];
      bp_fit      = decent_fit;
      bp_rivers   = [];
      bp_hyph     = False;
      bp_forced   = True;
      bp_demerits = num_zero
    };
  let pass tolerance left_right_skip allow_breaks = do
  {
    let graph = make_break_graph
                  (Array.of_list [`Break (num_zero, False, [||], [||], [||]) :: items])
                  tolerance
                  left_right_skip
                  allow_breaks
                  par_params
                  line_break_params
                  hyphen_params;

    ShortestPath.find_shortest_path
      update_breaks
      is_forced_break_point
      compute_best_break
      initial_bp
      {
        pl_prefix       = [];
        pl_rivers       = [xdim_zero];
        pl_position     = -1;
        pl_right_margin = [];
        pl_width        = xdim_zero
      }
      graph
      (Array.length graph.bg_breaks)
  };

  let left_right_skip = xdim_add_dim
                          (dim_to_xdim par_params.left_skip)
                          par_params.right_skip;

  if !tracing_line_breaks then
    log_string "\n@firstpass\n"
  else ();

  match pass line_break_params.pre_tolerance left_right_skip False with
  [ Some bp -> get_lines bp
  | None    -> do
    {
      if !tracing_line_breaks then
        log_string "\n\n@secondpass\n"
      else ();

      match pass line_break_params.tolerance left_right_skip True with
      [ Some bp -> get_lines bp
      | None    -> do
        {
          log_warn loc "Inserting emergency stretch!";

          iter 1 (max_num num_1_2 line_break_params.emergency_stretch)

          where rec iter n emergency_stretch = do
          {
            if n >= 999 then do
            {
              (* Ok, we have to resort to desperate measures. *)

              if !tracing_line_breaks then
                log_string "\n\n@final emergencypass\n"
              else ();

              match
                pass line_break_params.tolerance
                     (dim_to_xdim
                       {
                         d_base           = left_right_skip.xd_base;
                         d_stretch_factor = num_one;
                         d_stretch_order  = 1;
                         d_shrink_factor  = num_one;
                         d_shrink_order   = 1
                       })
                     True
              with
              [ Some bp -> get_lines bp
              | None    -> assert False
              ]
            }
            else do
            {
              if !tracing_line_breaks then do
              {
                log_string "\n\n@emergencypass ";
                log_int n;
                log_string "\n"
              }
              else ();

              match
                pass line_break_params.tolerance
                     (xdim_add_dim
                       left_right_skip
                       { d_base           = num_zero;
                         d_stretch_factor = emergency_stretch;
                         d_stretch_order  = 0;
                         d_shrink_factor  = num_zero;
                         d_shrink_order   = 0
                       })
                     True
              with
              [ Some bp -> get_lines bp
              | None    -> iter (n+1) (num_two */ emergency_stretch)
              ]
            }
          }
        }
      ]
    }
  ]
};

end;

(* check that <items> don't contain infinitely shrinkable glue *)

value check_shrinkage loc items = do
{
  let check_box box = do
  {
    let width = box.b_width;

    if width.d_shrink_factor = num_zero || width.d_shrink_order = 0 then
      box
    else do
    {
      log_warn loc "Paragraph contains infinitely shrinkable glue!\n";

      (* set minimal width of the box to 0 *)

      {
        (box)

        with

        b_width = { (width)  with  d_shrink_factor = width.d_base;  d_shrink_order = 0 }
      }
    }
  };

  List.map check items

  where check item = match item with
  [ `Glyph _     -> item
  | `Kern _      -> item
  | `Break _     -> item
  | `Command box -> `Command (check_box box)
  | `Box box     -> `Box (check_box box)
  ]
};

(* add par-indent and par-fill-skip *)

value add_par_fill_skip items par_params = do
{
  let (cmds, par) = Compose.discard_glue (List.rev items);

  [`Command (new_glue_box par_params.par_indent dim_zero True False)
    :: List.rev [`Break (minus_infinite, False, [||], [||], [||]);
                 `Command (new_glue_box par_params.par_fill_skip dim_zero True True)
                   :: cmds @ par]
  ]
};

(* break a paragraph into lines *)

value break_paragraph loc items par_params line_break_params hyphen_params = do
{
  let par = add_par_fill_skip (check_shrinkage loc items) par_params;

  if line_break_params.simple_breaking then
    Fast.break_lines
      loc
      (List.map simple_item_to_box (JustHyph.add_lig_kern True par))
      par_params
      line_break_params
  else
    Good.break_lines
      loc
      par
      par_params
      line_break_params
      hyphen_params
};

(* layout-line *)

value layout_line width line_no line par_params = do
{
  let rec process_commands boxes = match boxes with
  [ []      -> []
  | [b::bs] -> match b.b_contents with
    [ CommandBox (`ParCmd (CallParFunction f)) -> do
      {
        f line_no;
        process_commands bs
      }
    | _ -> [b :: process_commands bs]
    ]
  ];

  let (left_indent, right_indent) = par_params.par_shape line_no;

  let boxes =
    process_commands
      (par_params.post_process_line
        ([new_glue_box
            (dim_add par_params.left_skip (fixed_dim left_indent))
            dim_zero
            True False
         ]
       @ line
       @ [new_glue_box
            (dim_add par_params.right_skip (fixed_dim right_indent))
            dim_zero
            True False
         ]));

  HBox.make_to HBox.LR width boxes
};


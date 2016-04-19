
open XNum;
open Runtime;
open Unicode.Types;
open Logging;
open Dim;
open Substitute;
open FontMetric;
open Box;
open Page;

type area =                            (* an area of the page *)
{
  ar_name     : uc_string;
  ar_shape    : area_shape;
  ar_contents : area_contents_function (* function to fill the area with its contents *)
}

and page_update = (page * area_finaliser * page_state)

and area_contents_function =
  page -> area_shape -> list (list extended_glyph_item) -> page_state -> option page_update

and page_layout =                      (* description of a page *)
{
  pl_width  : num;                     (* paper dimensions *)
  pl_height : num;
  pl_areas  : array area               (* the areas *)
}

and area_finaliser = page -> page

and page_state =                                    (* state while filling areas *)
{
  ps_page_no        : int;
  ps_old_marks      : DynUCTrie.t uc_string;        (* marks in previous pages   *)
  ps_new_marks      : list (uc_string * uc_string); (* marks in the current page *)
  ps_galleys        : DynUCTrie.t (list box * Galley.galley);
  ps_layouts        : DynUCTrie.t page_layout;
  ps_next_layout    : page_layout;                  (* layout of the next page                            *)
  ps_new_floats     : list floating;                (* floats found in the current page, in reverse order *)
  ps_badness        : num;                          (* the accumulated badness of the current layout      *)
  ps_finished       : bool                          (* the galley filling some area is empty              *)
};

(* |page_run_state| contains the information needed about previous pages *)

type page_run_state =
{
  rs_page_no     : mutable int;
  rs_marks       : mutable DynUCTrie.t uc_string;                (* marks in previous pages *)
  rs_galleys     : mutable DynUCTrie.t (list box * Galley.galley);
  rs_layouts     : DynUCTrie.t page_layout;
  rs_next_layout : mutable page_layout;                            (* layout of the next page *)
  rs_finished    : mutable bool;

  rs_float_misplacement_demerits : num;   (* penalty for floats appearing on a different page *)

  (* unprocessed floats from the previous pages and how many  pages ago they appeared *)

  rs_floats      : mutable list (int * floating)
};

value tracing_page_layout = ref False;

(* |simple_page_update <page> <page-state>| is a page-update without finalisation function. *)

value simple_page_update page page_state = Some (page, fun p -> p, page_state);

(* the page_info structure *)

value get_page_info page ps =
{
  pi_width     = page.p_width;
  pi_height    = page.p_height;
  pi_page_no   = ps.ps_page_no;
  pi_old_marks = DynUCTrie.fold (fun k v ls -> [(k,v) :: ls]) ps.ps_old_marks [];
  pi_new_marks = ps.ps_new_marks
};

(* the page_layout structure *)

value lookup_area page_layout area_name = do
{
  iter_areas 0

  where rec iter_areas i = do
  {
    if i >= Array.length page_layout.pl_areas then do
    {
      log_warn ("", 0, 0) "unknown area: ";
      log_uc_string area_name;
      0
    }
    else do
    {
      if page_layout.pl_areas.(i).ar_name = area_name then
        i
      else
        iter_areas (i+1)
    }
  }
};

(* |layout_one_sided <layout> <page-no>| always returns <layout>. *)

value layout_one_sided layout _ = layout;

(* |layout_two_sided <even> <odd> <page-no>| alternates between <even> and <odd>. *)

value layout_two_sided even odd page_no = do
{
  if page_no land 1 = 0 then
    even
  else
    odd
};

(* |assemble_page <page-state>| assembles the filled areas from <page-state> *)

value assemble_page page = do
{
  new_compound_box
    (fixed_dim page.p_width)
    dim_zero
    (fixed_dim page.p_height)
    (List.map
      (fun (x,y,b) ->
        Graphic.PutBox (fixed_dim x) (fixed_dim (y -/ page.p_height)) b)
      page.p_boxes)
};

(* the page_run_state structure *)

value page_no rs = rs.rs_page_no;

(* |new_page_run_state <page-no> <float-demerits> <galleys> <layout>| creates a new |page_run_state|. *)

value new_page_run_state page_no float_demerits galleys layouts = do
{
  let last_break = [new_glue_box dim_zero dim_fil True True;
                    new_break_box (minus_num infinite) False [] [] []];

  {
    rs_page_no     = page_no;
    rs_marks       = DynUCTrie.empty;
    rs_galleys     = DynUCTrie.map (fun g -> (Galley.lines g @ last_break, g)) galleys;
    rs_layouts     = layouts;
    rs_next_layout = {
                       pl_width  = num_zero;
                       pl_height = num_zero;
                       pl_areas  = [| |]
                     };
    rs_finished    = True;
    rs_floats      = [];
    rs_float_misplacement_demerits = float_demerits
  }
};

value get_galley_table page_run_state = do
{
  let store_lines (lines, galley) = match lines with
  [ [] | [_] | [_; _] -> Galley.keep_lines galley lines
  | [x; y :: z]       -> Galley.keep_lines galley (remove_last x y z)
  ]
  where rec remove_last x y z = match z with
  [ []      -> []
  | [u::us] -> [x :: remove_last y u us]
  ];

  DynUCTrie.map store_lines page_run_state.rs_galleys
};

(* page breaking *)

(*
  |layout_page_with_floats <page-layout> <floats> <state>| layouts a page with a given set of floats.
  <floats> is in reverse order.
*)

value layout_page_with_floats page_layout floats state = do
{
  if !tracing_page_layout then do
  {
    log_string "\n#P: Trying to layout page ";
    log_int state.ps_page_no;
    log_string " with ";
    log_int (List.length floats);
    log_string " floats.";
  }
  else ();

  let page = new_page page_layout.pl_width page_layout.pl_height;

  let float_array = Array.make
                      (Array.length page_layout.pl_areas)
                      [];
  let finalisers  = Array.make
                      (Array.length page_layout.pl_areas)
                      (fun p -> p);

  (* collect the floats for each area *)

  List.iter
    (fun (i, f) ->
      float_array.(i) := [f :: float_array.(i)]
    )
    floats;

  (* fill areas *)

  iter 0 page state

  where rec iter i page state = do
  {
    if i >= Array.length page_layout.pl_areas then do
    {
      (* finalise all areas *)

      let p = Array.fold_left (fun p f -> f p) page finalisers;

      if !tracing_page_layout then do
      {
        log_string "\n#P: successful layout: badness ";
        log_num state.ps_badness
      }
      else ();

      Some (p, state)
    }
    else do
    {
      if !tracing_page_layout then do
      {
        log_string "\n#P: Layouting area \"";
        log_uc_string page_layout.pl_areas.(i).ar_name;
        log_string "\" ..."
      }
      else ();

      match
        page_layout.pl_areas.(i).ar_contents
          page
          page_layout.pl_areas.(i).ar_shape
          float_array.(i)
          state
      with
      [ None -> do
        {
          if !tracing_page_layout then
            log_string "\n#P: layout failed."
          else ();

          None
        }
      | Some (page, finaliser, state) -> do
        {
          finalisers.(i) := finaliser;

          iter (i + 1) page state
        }
      ]
    }
  }
};

(*
  |execute_command_boxes <page-state> <position> <box>| searchs for command boxes in <box>.
  It handles |CallFunction| and |Floats| commands.
*)

(* FIX: merge with draw_box *)

value rec execute_command_boxes page_info x y box = match box.b_contents with
[ CommandBox (`PageCmd cmd) -> match cmd with
  [ CallPageFunction f-> do
    {
      f page_info (x, y);

      box
    }
  | _ -> box
  ]
| ProcBox f -> do
  {
    new_compound_box
      box.b_width
      box.b_height
      box.b_depth
      (f page_info (x, y) box)
  }
| CompBox cmds -> do
  {
    new_compound_box box.b_width box.b_height box.b_depth (iter x y cmds)

    where rec iter x y cmds = match cmds with
    [ []                             -> []
    | [Graphic.PutBox dx dy b :: bs] -> do
      {
        [Graphic.PutBox dx dy
           (execute_command_boxes page_info (x +/ dx.d_base) (y +/ dy.d_base) b)
        :: iter x y bs]
      }
    | [c :: cs] -> [c :: iter x y cs]
    ]
  }
| MathBox c b -> new_math_box c (execute_command_boxes page_info x y b)
| _           -> box
];

(*
  |collect_floats <run-state> <page-state>| returns the combined list of floats from <run-state> and
  <page-state>.
*)

value collect_floats run_state page_state = do
{
    run_state.rs_floats
  @ (List.map (fun f -> (0,f)) page_state.ps_new_floats)
};

(*
  |build_page <run-state> <layout-result>| creates a page and updates <run-state> accordingly.
*)

value build_page run_state (page, state, num_floats) = do
{
  let remaining_floats = XList.drop num_floats (collect_floats run_state state);

  if !tracing_page_layout then do
  {
    let n = List.length remaining_floats;

    if n = 0 then
      log_string "\n#P: Choosing layout with all floats."
    else do
    {
      log_string "\n#P: Choosing layout with ";
      log_int num_floats;
      log_string " floats, defering ";
      log_int (List.length remaining_floats);
      log_string " floats to the next pages."
    }
  }
  else ();

  let contents =
    List.map
      (fun (x,y,b) ->
         Graphic.PutBox
           x (y -/ page.p_height)
           (draw_box (get_page_info page state) x y b))
      page.p_boxes;

  run_state.rs_page_no     := run_state.rs_page_no + 1;
  run_state.rs_floats      := List.map (fun (i, f) -> (i+1, f)) remaining_floats;
  run_state.rs_next_layout := state.ps_next_layout;
  run_state.rs_galleys     := state.ps_galleys;
  run_state.rs_finished    := state.ps_finished;
  run_state.rs_marks       :=
    List.fold_left
      (fun marks (k,v) -> DynUCTrie.add_string k v marks)
      state.ps_old_marks
      (List.rev state.ps_new_marks);

  Some {
         FontMetric.p_contents = FontMetric.Group contents;
         FontMetric.p_number   = run_state.rs_page_no - 1;
         FontMetric.p_width    = page.p_width;
         FontMetric.p_height   = page.p_height
       }
};


(* |choose_best_layout <num_old_floats> <results>| choose the best page layout among the given list. *)

value choose_best_layout misplacement_demerits num_old_floats results = do
{
  let demerits (_, page_state, num_floats) = do
  {
    misplacement_demerits
      */ num_of_int (List.length page_state.ps_new_floats + num_old_floats - num_floats)
    +/ page_state.ps_badness
  };

  match results with
  [ []      -> assert False
  | [r::rs] -> find_best (demerits r) r rs
  ]
  where rec find_best best_demerits best results = match results with
  [ []      -> do
    {
      if !tracing_page_layout then do
      {
        log_string "\n#P: Best layout with ";
        log_num best_demerits;
        log_string " demerits."
      }
      else ();

      best
    }
  | [r::rs] -> do
    {
      let d = demerits r;

      if d </ best_demerits then
        find_best d r rs
      else
        find_best best_demerits best rs
    }
  ]
};

value break_page layout run_state = do
{
  let page_state =
  {
    ps_page_no     = run_state.rs_page_no;
    ps_old_marks   = run_state.rs_marks;
    ps_new_marks   = [];
    ps_galleys     = run_state.rs_galleys;
    ps_layouts     = run_state.rs_layouts;
    ps_next_layout = layout (run_state.rs_page_no + 1);
    ps_new_floats  = [];
    ps_badness     = num_zero;
    ps_finished    = run_state.rs_finished
  };

  let current_layout = run_state.rs_next_layout;

  (* layout without any floats to collect a list of new floats*)

  match layout_page_with_floats current_layout [] page_state with
  [ None        -> None
  | Some (p, s) -> do
    {
      let num_old_floats = List.length run_state.rs_floats;
      let floats         = collect_floats run_state s;

      (* FIX: collect also floats referenced in <floats> *)

      (* FIX: layout a float page *)

      build_page run_state
        (choose_best_layout run_state.rs_float_misplacement_demerits num_old_floats
          (iter 1 [(p,s,0)] [] floats))

      where rec iter num_floats results floats_on_page remaining_floats = match remaining_floats with
      [ []      -> results
      | [f::fs] -> do
        {
          (* layout the page with the floats of <floats_on_page> *)

          let new_floats = [f :: floats_on_page];

          match
            layout_page_with_floats
              current_layout
              (List.map
                (fun (_, (a,f)) -> (lookup_area current_layout a, f))
                new_floats)
              page_state
          with
          [ None       -> results
          | Some (p,s) -> do
            {
              let num_new_floats = List.length s.ps_new_floats;

              if num_floats > num_old_floats + num_new_floats then
                results           (* we placed a float before its reference *)
              else
                iter
                  (num_floats + 1)
                  [(p, s, num_floats) :: results]
                  new_floats
                  fs
            }
          ]
        }
      ]
    }
  ]
};

(*
  |layout_run_of_pages <layout> <abort> <page-run-state>| sets several pages with the layout returned by
  <layout> until <abort> returns |True|.
*)

value layout_run_of_pages layout abort page_run_state = do
{
  let old_page_run_state =
  {
    rs_page_no     = page_run_state.rs_page_no;
    rs_marks       = page_run_state.rs_marks;
    rs_galleys     = page_run_state.rs_galleys;
    rs_layouts     = page_run_state.rs_layouts;
    rs_next_layout = page_run_state.rs_next_layout;
    rs_finished    = page_run_state.rs_finished;
    rs_float_misplacement_demerits = page_run_state.rs_float_misplacement_demerits;
    rs_floats      = page_run_state.rs_floats
  };

  let pages = ListBuilder.make ();

  page_run_state.rs_next_layout := layout page_run_state.rs_page_no;

  iter ()

  where rec iter () = do
  {
    (* FIX: test for abortion /before/ layouting the next page *)

    old_page_run_state.rs_page_no     := page_run_state.rs_page_no;
    old_page_run_state.rs_marks       := page_run_state.rs_marks;
    old_page_run_state.rs_galleys     := page_run_state.rs_galleys;
    old_page_run_state.rs_next_layout := page_run_state.rs_next_layout;
    old_page_run_state.rs_finished    := page_run_state.rs_finished;
    old_page_run_state.rs_floats      := page_run_state.rs_floats;

    page_run_state.rs_finished := True;

    match break_page layout page_run_state with
    [ None   -> (ListBuilder.get pages, page_run_state)
    | Some p -> if abort page_run_state then
                  (ListBuilder.get pages, old_page_run_state)
                else do
                {
                  ListBuilder.add pages p;
                  iter ()
                }
    ]
  }
};

(* abort functions *)

(* |abort_on_empty_area <no> <page-state>| returns |True| if the box number <no> is empty. *)

(* FIX:
value abort_on_empty_area no page_state = do
{
  page_state.ps_boxes.(no) = empty_box
};
*)

(* |abort_when_done <page-state>| returns |True| if all galleys are empty. *)

value abort_when_done page_state = do
{
  page_state.rs_finished
};

(* |abort_on_page <no> <page-state>| returns |True| if the page number is greater than <no>. *)

value abort_on_page no page_state = do
{
  page_state.rs_page_no > no
};


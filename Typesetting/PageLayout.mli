
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

and page_layout =                               (* description of a page     *)
{
  pl_width  : num;                              (* paper dimensions          *)
  pl_height : num;
  pl_areas  : array area                        (* the areas                 *)
}

and area_finaliser = page -> page

and page_state =                                    (* state while filling areas *)
{
  ps_page_no        : int;
  ps_old_marks      : DynUCTrie.t uc_string;      (* marks in previous pages                      *)
  ps_new_marks      : list (uc_string * uc_string); (* marks in the current page                    *)
  ps_galleys        : DynUCTrie.t (list box * Galley.galley);
  ps_layouts        : DynUCTrie.t page_layout;
  ps_next_layout    : page_layout;                  (* layout of the next page                            *)
  ps_new_floats     : list floating;                (* floats found in the current page, in reverse order *)
  ps_badness        : num;                          (* the accumulated badness of the current layout      *)
  ps_finished       : bool                          (* the galley filling some area is empty              *)
};

(* |page_run_state| contains the information needed about previous pages *)

type page_run_state;

value tracing_page_layout : ref bool;

value simple_page_update : page -> page_state -> option page_update;

value get_page_info : page -> page_state -> page_info;

value new_page_run_state : int -> num -> DynUCTrie.t Galley.galley ->
                           DynUCTrie.t page_layout -> page_run_state;
value page_no            : page_run_state -> int;
value get_galley_table   : page_run_state -> DynUCTrie.t Galley.galley;

value layout_one_sided    : page_layout -> int -> page_layout;
value layout_two_sided    : page_layout -> page_layout -> int -> page_layout;

value layout_run_of_pages : (int -> page_layout) -> (page_run_state -> bool) -> page_run_state ->
                            (list FontMetric.page * page_run_state);

(*value abort_on_empty_area  : int -> page_run_state -> bool; *)
value abort_when_done      : page_run_state -> bool;
value abort_on_page        : int -> page_run_state -> bool;


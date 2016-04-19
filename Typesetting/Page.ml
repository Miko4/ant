
open XNum;
open Runtime;
open Unicode.Types;
open Logging;
open Dim;
open Box;

type page =
{
  p_width  : num;                       (* dimensions *)
  p_height : num;
  p_used   : PlanePartition.map unit;   (* area covered by boxes *)
  p_boxes  : list (num * num * box)     (* list of boxes and their position *)
};

type area_shape =                       (* an area of the page                          *)
{
  as_pos_x  : num;                      (* position of the base line                    *)
  as_pos_y  : num;
  as_width  : num;
  as_height : num;                      (* distance between first and last baseline     *)
  as_top    : num;                      (* maximal amount the area might extend above   *)
  as_bottom : num                       (* the first and below the last baseline        *)
};

value new_page width height =
{
  p_width  = width;
  p_height = height;
  p_used   = PlanePartition.empty num_zero num_zero width height;
  p_boxes  = []
};

(* |allocate_rect <page> <x> <y> <width> <height>| marks the given rectangle as used. *)

value allocate_rect page x y width height =
{
  (page)

  with

  p_used = PlanePartition.set_rect
             page.p_used
             x y (x +/ width) (y -/ height)
             ()
};

(* |put_box <page> <x> <y> <box>| adds <box> to <page>. *)

value put_box page x y box  =
{
  (page)  with  p_boxes = [(x, y, box) :: page.p_boxes]
};

(*
  |pub_box_on_page <page> <x> <y> <box>| is a convenient wrapper for |allocate_rect| and |put_box|
  that adds <box> to <page> at position (<x>, <y>) and marks the covered area as used.
*)

value put_box_on_page page x y box =
{
  (page)

  with

  p_boxes = [(x, y, box) :: page.p_boxes];
  p_used  = PlanePartition.set_rect
              page.p_used
              x
              (y -/ box.b_depth.d_base)
              (x +/ box.b_width.d_base)
              (y +/ box.b_height.d_base)
              ()
};

value find_place_in_area_top page area top height bottom = do
{
  let total_height = xdim_to_dim
                       (xdim_add_dim
                         (xdim_add_dim height top)
                         bottom);
  let min_height   = dim_min_value total_height;
  let min_top      = if total_height.d_shrink_order = 0 then
                       (dim_scale_upto top (num_minus_one, 0)).d_base
                     else
                       (dim_scale_upto top (top.d_base, total_height.d_shrink_order)).d_base;
  let min_bottom   = if total_height.d_shrink_order = 0 then
                       (dim_scale_upto bottom (num_minus_one, 0)).d_base
                     else
                       (dim_scale_upto bottom (bottom.d_base, total_height.d_shrink_order)).d_base;

  let min_top_shift    = max_num num_zero (min_top    -/ area.as_top);
  let min_bottom_shift = max_num num_zero (min_bottom -/ area.as_bottom);

  match
    PlanePartition.find_free_top
      page.p_used
      area.as_pos_x
      (area.as_pos_y -/ area.as_height +/ min_bottom_shift)
      (area.as_pos_x +/ area.as_width)
      (area.as_pos_y -/ min_top_shift)
      min_height
  with
  [ None       -> None
  | Some (a1,a2) -> do
    {
      let b2 = min_num a2 (area.as_pos_y +/ area.as_top);

      (* First try the maximal height. *)

      let h = if a1 +/ total_height.d_base <=/ a2 then
                total_height.d_base
              else
                a2 -/ a1;
      let r = adjustment_ratio total_height h;
      let t = (dim_scale_upto top    r).d_base;
      let b = (dim_scale_upto bottom r).d_base;

      (* Are the top and bottom baselines ok? *)

      let top_baseline    = b2 -/ t;
      let bottom_baseline = b2 -/ h +/ b;

      if top_baseline    <=/ area.as_pos_y
      && bottom_baseline >=/ area.as_pos_y -/ area.as_height
      && bottom_baseline >=/ a1 +/ b then
        Some (top_baseline, r)
      else do
      {
        (* Shrink box to fit. *)

        let top_shift = max_num num_zero (top_baseline -/ area.as_pos_y);
        let bot_shift = max_num num_zero
                          (max_num (area.as_pos_y -/ area.as_height)
                                   (a1 +/ b)
                           -/ bottom_baseline);

        let r = adjustment_ratio total_height (h -/ top_shift -/ bot_shift);

        Some (top_baseline -/ top_shift, r)
      }
    }
  ]
};

value find_place_in_area_bottom page area top height bottom = do
{
  let total_height = xdim_to_dim
                       (xdim_add_dim
                         (xdim_add_dim height top)
                         bottom);
  let min_height   = dim_min_value total_height;
  let min_top      = if total_height.d_shrink_order = 0 then
                       (dim_scale_upto top (num_minus_one, 0)).d_base
                     else
                       (dim_scale_upto top (top.d_base, total_height.d_shrink_order)).d_base;
  let min_bottom   = if total_height.d_shrink_order = 0 then
                       (dim_scale_upto bottom (num_minus_one, 0)).d_base
                     else
                       (dim_scale_upto bottom (bottom.d_base, total_height.d_shrink_order)).d_base;

  let min_top_shift    = max_num num_zero (min_top    -/ area.as_top);
  let min_bottom_shift = max_num num_zero (min_bottom -/ area.as_bottom);

  match
    PlanePartition.find_free_bottom
      page.p_used
      area.as_pos_x
      (area.as_pos_y -/ area.as_height +/ min_bottom_shift)
      (area.as_pos_x +/ area.as_width)
      (area.as_pos_y -/ min_top_shift)
      min_height
  with
  [ None       -> None
  | Some (a1,a2) -> do
    {
      let b1 = max_num a1 (area.as_pos_y -/ area.as_height -/ area.as_bottom);

      (* First try the maximal height. *)

      let h = if a1 +/ total_height.d_base <=/ a2 then
                total_height.d_base
              else
                a2 -/ a1;
      let r = adjustment_ratio total_height h;
      let t = (dim_scale_upto top    r).d_base;
      let b = (dim_scale_upto bottom r).d_base;

      (* Are the top and bottom baselines ok? *)

      let top_baseline    = b1 +/ h -/ t;
      let bottom_baseline = b1 +/ b;

      if top_baseline    <=/ area.as_pos_y
      && top_baseline    <=/ a2 -/ t
      && bottom_baseline >=/ area.as_pos_y -/ area.as_height then
        Some (top_baseline, r)
      else do
      {
        (* Shrink box to fit. *)

        let top_shift = max_num num_zero
                          (top_baseline -/
                            (max_num area.as_pos_y (a2 -/ t)));
        let bot_shift = max_num
                          num_zero
                          (area.as_pos_y -/ area.as_height -/ bottom_baseline);

        let r = adjustment_ratio total_height (h -/ top_shift -/ bot_shift);

        Some (top_baseline -/ top_shift, r)
      }
    }
  ]
};

value find_place_in_area_horiz find page area width = do
{
  let min_width = dim_min_value width;

  match
    find
      page.p_used
      area.as_pos_x area.as_pos_y
      (area.as_pos_x +/ area.as_width)
      (area.as_pos_y -/ area.as_height)
      min_width
  with
  [ None       -> None
  | Some (a,b) -> do
    {
      if a +/ width.d_base <=/ b then
        Some (a, (num_zero, 0))
      else
        Some (a, adjustment_ratio width (b -/ a))
    }
  ]
};

value find_place_in_area_left page area width =
  find_place_in_area_horiz PlanePartition.find_free_left page area width;

value find_place_in_area_right page area width =
  find_place_in_area_horiz PlanePartition.find_free_right page area width;

value area_free_vert page area = do
{
  iter
    num_zero
    (PlanePartition.vert_strip
      page.p_used
      area.as_pos_x
      (area.as_pos_x +/ area.as_width))
    []

  (* calculate reversed complement of interval list *)

  where rec iter y strip result = match strip with
  [ []            -> [(y, page.p_height) :: result]
  | [(a,b) :: cs] -> iter b cs [(y, a) :: result]
  ]
};

value area_free_horiz page area = do
{
  iter
    num_zero
    (PlanePartition.horiz_strip
      page.p_used
      (area.as_pos_y +/ area.as_top)
      (area.as_pos_y -/ area.as_height -/ area.as_bottom))

  (* calculate complement of interval list *)

  where rec iter x strip = match strip with
  [ []            -> [(x, page.p_width)]
  | [(a,b) :: cs] -> [(x, a) :: iter b cs]
  ]
};


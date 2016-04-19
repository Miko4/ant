
open XNum;
open Runtime;
open Logging;
open Dim;
open Graphic;
open Box;

type vert_alignment = [ Top | Bottom ];

type float_params =
{
  alignment   : vert_alignment;
  top_skip    : num;    (* minimal whitespace above floats   *)
  bottom_skip : num;    (* minimal whitespace below floats   *)
  float_sep   : dim     (* minimal whitespace between floats *)
};

value layout params page area floats page_state = do
{
  let width = area.Page.as_width;

  let float_sep_glue = new_glue_box dim_zero params.float_sep False False;

  let make_box items = do
  {
    let boxes = List.map extended_item_to_box items;
    let box   = resize_box_horiz_upto (HBox.make HBox.LR boxes) width;
    let shift = fixed_dim ((width -/ box.b_width.d_base) // num_two);

    new_compound_box
      (fixed_dim width)
      box.b_height
      box.b_depth
      [PutBox shift dim_zero box]
  };

  let rec make_boxes floats = match floats with
  [ []      -> []
  | [f]     -> [make_box f]
  | [f::fs] -> [make_box f; float_sep_glue :: make_boxes fs]
  ];

  match make_boxes floats with
  [ []    -> PageLayout.simple_page_update page page_state
  | boxes -> do
    {
      let (top, height, bottom) = VBox.calc_vert_dimensions boxes;

      (* FIX: process page-commands in <boxes> *)
      (* FIX: consider top-sep and bottom-sep  *)

      let placement_function = match params.alignment with
      [ Top    -> Page.find_place_in_area_top
      | Bottom -> Page.find_place_in_area_bottom
      ];

      match placement_function page area top height bottom with
      [ None        -> None
      | Some (y, r) -> do
        {
          let x        = area.Page.as_pos_x;
          let box      = VBox.to_top (VBox.layout_scaled r boxes);
          let new_page = Page.put_box_on_page page x y box;

          if !PageLayout.tracing_page_layout then do
          {
            log_string "\n#P: Placing float at ";
            log_num (page.Page.p_height -/ y);

            if box.b_height.d_base <>/ height.xd_base then do
            {
              log_string " scaled to ";
              log_num box.b_height.d_base
            }
            else ()
          }
          else ();

          PageLayout.simple_page_update new_page page_state
        }
      ]
    }
  ]
};
(*
  let width = area.PageLayout.ar_width in

  (* add a box to the list of boxes *)

  let add_box y box boxes = do
  {
    let x = (width -/ box.b_width.d_base) // num_two in

    if x </ num_zero then
      raise (Invalid_argument "box does not fit")
    else
      [PutBox (fixed_dim x) (xdim_to_dim y) box :: boxes]
  }
  in

  (* resize the box to its final height *)

  let resize_box box height = do
  {
    if height =/ box.b_height.d_base +/ box.b_depth.d_base then
      box
    else
      resize_box_vert box box.b_height.d_base (height -/ box.b_depth.d_base)
  }
  in

  (* put the final box on the page *)

  let put_floats_on_page box page = do
  {
    let x      = area.PageLayout.ar_pos_x in
    let y      = area.PageLayout.ar_pos_y in
    let free   = PlanePartition.vert_strip page.PageLayout.p_used x width in
    let height = dim_add box.b_depth (fixed_dim box.b_height.d_base) in

    let reserve y h = PageLayout.allocate_rect page x y width h in

    (* find a free spot on the page *)

    find y free

    where rec find y free = match free with
    [ [] -> do
      {
        if y +/ dim_min_value height
            <=/ area.PageLayout.ar_pos_y +/ area.PageLayout.ar_height then do
        {
          let h = min_num height.d_base (area.PageLayout.ar_pos_y +/ area.PageLayout.ar_height -/ y) in

          Some (reserve y h, finalise x y (resize_box box h))
        }
        else
          None
      }
    | [(a,b) :: cs] -> do
      {
        if y +/ dim_min_value height <=/ a then do
        {
          let h = min_num height.d_base (a -/ y) in

          Some (reserve y h, finalise x y (resize_box box h))
        }
        else
          find b cs
      }
    ]
  }
  in
*)

(*
  let glue = new_glue_box dim_fil dim_zero False False in

  let rec make_boxes floats = match floats with
  [ []      -> []
  | [f::fs] -> [HBox.make_to width [glue :: f @ [glue]] (* FIX *)
                :: make_boxes floats]
  ]
  in

  let boxes                 = make_boxes floats               in
  let (top, height, bottom) = VBox.calc_vert_dimensions boxes in
  let total_height          = xdim_to_dim (xdim_add_dim (xdim_add_dim height top) bottom) in
  let min_height            = dim_min_value total_height in

  let min_y      = area.PageLayout.ar_pos_y in
  let max_y      = area.PageLayout.ar_pos_y +/ area.PageLayout.ar_height in
  (* FIX take top/bottom into account *)

  match
    PlanePartition.find_free_top page.PageLayout.p_used
      area.PageLayout.ar_pos_x
      min_y
      (area.PageLayout.ar_pos_x +/ area.PageLayout.ar_width)
      max_y
      min_height
  with
  [ None        -> None
  | Some (y, c) -> do
    {
      if total_height.d_base >=/ c -/ y then do
      {
        let box = VBox.make boxes in

      }
      else do
      {
        let ratio = adjustment_ratio total_height (c -/ y)      in
        let box   = scale_box_vert_upto (VBox.make boxes) ratio in

      }
    }
  ]
*)

(*
  try
    match List.flatten floats with
    [ []      -> Some (page, fun p -> p)
    | [b::bs] -> do
      {
        let top = min_num b.b_height.d_base area.PageLayout.ar_max_top in
        let y   = xdim_add_dim
                    xdim_zero
                    (fixed_dim (b.b_height.d_base -/ top))
        in

        iter y b.b_depth (add_box y b []) bs

        where rec iter y d result boxes = match boxes with
        [ []      -> put_floats_on_page
                      (new_compound_box
                        (fixed_dim width)
                        (fixed_dim top)
                        (xdim_to_dim (xdim_add_dim y d))
                        result)
                      page
        | [b::bs] -> do
          {
            let new_y = xdim_add_dim
                          (xdim_add_dim
                            (xdim_add_dim y d)
                            b.b_height)
                          float_sep
            in

            iter new_y b.b_depth (add_box new_y b result) bs
          }
        ]
      }
    ]
  with
  [ _ -> None ]
*)


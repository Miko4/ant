
open XNum;
open Runtime;
open Logging;
open Dim;
open Box;

(* The maximal badness tolerated by make. *)

value max_v_badness = ref (num_of_int 1000);

(*
  |calc_vert_dimension <boxes>| returns the triple (<top>, <height>, <bottom>)
  where <top> is the height of the first box, <bottom> is the depth of the last box,
  and <height> is the sum of the remaining heights and depths.
*)

value calc_vert_dimensions boxes = match boxes with
[ []      -> (dim_zero, xdim_zero, dim_zero)
| [b::bs] -> do
  {
    let top = b.b_height;

    iter xdim_zero b.b_depth bs

    where rec iter height depth boxes = match boxes with
    [ []      -> (top, height, depth)
    | [b::bs] -> iter (xdim_add_dim (xdim_add_dim height depth) b.b_height) b.b_depth bs
    ]
  }
];

value calc_height boxes = do
{
  let (top, height, bottom) = calc_vert_dimensions boxes;

  (xdim_to_dim (xdim_add_dim height top), bottom)
};

value make boxes = do
{
  let (h, d) = calc_height boxes;

  iter dim_zero xdim_zero [] (List.rev boxes)

  where rec iter width height result boxes = match boxes with
  [ []      -> new_compound_box width (xdim_to_dim height) d result
  | [b::bs] -> match b.b_contents with
    [ CommandBox (`GfxCmd c) -> iter width height [c :: result] bs
    | _                      -> do
      {
        let v_pos = xdim_add_dim height b.b_depth;
        iter (dim_max width b.b_width)
             (xdim_add_dim (xdim_add_dim height b.b_height) b.b_depth)
             [Graphic.PutBox
               dim_zero
               (xdim_select_order v_pos h.d_stretch_order h.d_shrink_order)
               b
              :: result]
             bs
      }
    ]
  ]
};

value layout_scaled (factor, order) boxes = do
{
  if boxes = [] then
    new_compound_box dim_zero dim_zero dim_zero []
  else do
  {
    let ratio = if factor </ num_of_int (-1) && order = 0 then
                  (num_of_int (-1), 0)
                else
                  (factor, order)
    and bad   = dim_scale_badness (factor, order);
    let boxes = List.rev boxes;
    let depth = (scale_box_vert (List.hd boxes) ratio).b_depth;

    if bad >/ !max_v_badness then do
    {
      if factor </ num_zero then
        log_string "\nWarning: Overfull vbox (badness "
      else
        log_string "\nWarning: Underfull vbox (badness ";

      if bad </ infinite then
        log_num bad
      else
        log_string "infinite";

      log_string ")!\n"
    }
    else
      ();

    iter dim_zero xdim_zero [] boxes

    where rec iter width height result boxes = match boxes with
    [ []      -> new_compound_box width (fixed_dim height.xd_base) depth result
    | [b::bs] -> match b.b_contents with
      [ CommandBox (`GfxCmd c) -> iter width height [c :: result] bs
      | _                      -> do
        {
          let box   = scale_box_vert b ratio;
          let v_pos = xdim_add_dim height b.b_depth;
          iter (dim_max width box.b_width)
               (xdim_add_dim (xdim_add_dim height box.b_height) box.b_depth)
               [Graphic.PutBox dim_zero (fixed_dim v_pos.xd_base) box :: result]
               bs
        }
      ]
    ]
  }
};

value to_top box = do
{
  let rec get_baseline cmds = match cmds with
  [ []                          -> num_zero
  | [Graphic.PutBox _ y _ :: _] -> minus_num y.d_base
  | [_ :: cs]                   -> get_baseline cs
  ];

  match box.b_contents with
  [ CompBox cmds -> shift_compound_vert box (get_baseline cmds)
  | _            -> box
  ]
};

value make_to height boxes = do
{
  let (h, _) = calc_height boxes;
  let ratio  = adjustment_ratio h height;

  layout_scaled ratio boxes
};

value make_scaled factor boxes = do
{
  let (h, _) = calc_height boxes;
  let height = factor */ h.d_base;
  let ratio  = adjustment_ratio h height;

  layout_scaled ratio boxes
};

value make_spread amount boxes = do
{
  let (h, _) = calc_height boxes;
  let height = amount +/ h.d_base;
  let ratio  = adjustment_ratio h height;

  layout_scaled ratio boxes
};

value make_top boxes = to_top (make boxes);

value make_top_to height boxes = do
{
  let (h, _) = calc_height boxes;
  let ratio  = adjustment_ratio h height;

  to_top (layout_scaled ratio boxes)
};

value make_top_scaled factor boxes = do
{
  let (h, d) = calc_height boxes;
  let height = factor */ (h.d_base +/ d.d_base);
  let ratio  = adjustment_ratio (dim_add h d) height;

  to_top (layout_scaled ratio boxes)
};

value make_top_spread amount boxes = do
{
  let (h, _) = calc_height boxes;
  let height = amount +/ h.d_base;
  let ratio  = adjustment_ratio h height;

  to_top (layout_scaled ratio boxes)
};


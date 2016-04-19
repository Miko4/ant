open XNum;
open Logging;

(* One inch in points. *)

value inch = num_of_ints 7227 100;

(* Badness is a measure of how bad a scaled box looks. *)

value infinite       = num_of_int 10000;
value minus_infinite = num_of_int (-10000);

(* |badness <ratio>| calculates the badness of a box from its adjustment ratio. *)

value badness ratio = do
{
  if ratio </ num_of_int (-1) then
    infinite
  else
    round_num (abs_num (num_of_int 100 */ ratio */ ratio */ ratio))
};

(*
   A dimension consists of a base value and the amount it can grow or shrink.
   All values are in units of point.
*)

type dim =
{
  d_base           : num;
  d_stretch_factor : num;
  d_stretch_order  : int;
  d_shrink_factor  : num;
  d_shrink_order   : int
};

type xdim =
{
  xd_base    : num;
  xd_stretch : list (num * int);
  xd_shrink  : list (num * int)
};

value fixed_dim x =
{
  d_base           = x;
  d_stretch_factor = num_zero;
  d_stretch_order  = 0;
  d_shrink_factor  = num_zero;
  d_shrink_order   = 0
};

value dim_zero   = fixed_dim num_zero;
value dim_1pt    = fixed_dim num_one;
value dim_12pt   = fixed_dim (num_of_int 12);
value dim_fil    = { d_base = num_zero; d_stretch_factor = num_one;         d_stretch_order = 1;
                                        d_shrink_factor  = num_zero;        d_shrink_order  = 0 };
value dim_fill   = { d_base = num_zero; d_stretch_factor = num_one;         d_stretch_order = 2;
                                        d_shrink_factor  = num_zero;        d_shrink_order  = 0 };
value dim_ss     = { d_base = num_zero; d_stretch_factor = num_one;         d_stretch_order = 1;
                                        d_shrink_factor  = num_one;         d_shrink_order  = 1 };
value dim_filneg = { d_base = num_zero; d_stretch_factor = num_of_int (-1); d_stretch_order = 1;
                                        d_shrink_factor  = num_zero;        d_shrink_order  = 0 };

value xdim_zero  = { xd_base = num_zero; xd_stretch = []; xd_shrink = [] };

value dim_stretch d = (d.d_stretch_factor, d.d_stretch_order);
value dim_shrink  d = (d.d_shrink_factor,  d.d_shrink_order);

value xdim_stretch d = do
{
  (* return the first non-zero entry in |xd_stretch| *)

  iter d.xd_stretch

  where rec iter stretch = match stretch with
  [ []      -> (num_zero, 0)
  | [s::ss] -> if fst s <>/ num_zero then
                 s
               else
                 iter ss
  ]
};

value xdim_shrink d = do
{
  (* return the first non-zero entry in |xd_shrink| *)

  iter d.xd_shrink

  where rec iter shrink = match shrink with
  [ []      -> (num_zero, 0)
  | [s::ss] -> if fst s <>/ num_zero then
                 s
               else
                 iter ss
  ]
};

(* convert xdim to dim *)

value xdim_to_dim d = do
{
  let (st_f, st_o) = xdim_stretch d;
  let (sh_f, sh_o) = xdim_shrink  d;
  {
    d_base           = d.xd_base;
    d_stretch_factor = st_f;
    d_stretch_order  = st_o;
    d_shrink_factor  = sh_f;
    d_shrink_order   = sh_o
  }
};

(* convert dim to xdim *)

value dim_to_xdim d =
{
  xd_base    = d.d_base;
  xd_stretch = [(d.d_stretch_factor, d.d_stretch_order)];
  xd_shrink  = [(d.d_shrink_factor,  d.d_shrink_order)]
};

value dim_equal d0 d1 = do
{
     d0.d_base           = d1.d_base
  && d0.d_stretch_factor = d1.d_stretch_factor
  && d0.d_stretch_order  = d1.d_stretch_order
  && d0.d_shrink_factor  = d1.d_shrink_factor
  && d0.d_shrink_order   = d1.d_shrink_order
};

value dim_is_zero d = do
{
     d.d_base           = num_zero
  && d.d_stretch_factor = num_zero
  && d.d_stretch_order  = 0
  && d.d_shrink_factor  = num_zero
  && d.d_shrink_order   = 0
};

value log_dim d = do
{
  log_num d.d_base;
  log_string "pt";

  if d.d_stretch_factor <>/ num_zero then do
  {
    log_string " plus ";
    log_num d.d_stretch_factor;

    if d.d_stretch_order = 0 then
      log_string "pt"
    else do
    {
      log_string "fi";
      for i = 1 to d.d_stretch_order do
      {
        log_string "l"
      }
    }
  }
  else ();

  if d.d_shrink_factor <>/ num_zero then do
  {
    log_string " minus ";
    log_num d.d_shrink_factor;

    if d.d_shrink_order = 0 then
      log_string "pt"
    else do
    {
      log_string "fi";
      for i = 1 to d.d_shrink_order do
      {
        log_string "l"
      }
    }
  }
  else ()
};

(* Routines to add and subtract dimensions. *)

value dim_add d1 d2 = do
{
  let add_order s1 o1 s2 o2 = do
  {
    if o1 < o2 then
      (s2, o2)
    else if o1 > o2 then
      (s1, o1)
    else
      (s1 +/ s2, o1)
  };

  let (st_f, st_o) = add_order d1.d_stretch_factor d1.d_stretch_order
                               d2.d_stretch_factor d2.d_stretch_order;
  let (sh_f, sh_o) = add_order d1.d_shrink_factor  d1.d_shrink_order
                               d2.d_shrink_factor  d2.d_shrink_order;
  {
    d_base = d1.d_base +/ d2.d_base;
    d_stretch_factor = st_f;
    d_stretch_order  = st_o;
    d_shrink_factor  = sh_f;
    d_shrink_order   = sh_o
  }
};

value xdim_add d1 d2 = do
{
  let rec insert_order l (s, o) = match l with
  [ []          -> [(s,o)]
  | [(x,y)::zs] ->
    if o > y then
      [(s, o) :: l]
    else if o = y then
      [(s +/ x, o) :: zs]
    else
      [(x, y) :: insert_order zs (s, o)]
  ];
  let rec add_lists l1 l2 = match l2 with
  [ []          -> l1
  | [(s,o)::xs] -> insert_order (add_lists l1 xs) (s, o)
  ];

  {
    xd_base    = d1.xd_base +/ d2.xd_base;
    xd_stretch = add_lists d1.xd_stretch d2.xd_stretch;
    xd_shrink  = add_lists d1.xd_shrink  d2.xd_shrink
  }
};

value xdim_add_dim d1 d2 = do
{
  let rec insert_order l s o = match l with
  [ []          -> [(s,o)]
  | [(x,y)::zs] ->
    if o > y then
      [(s, o) :: l]
    else if o = y then
      [(s +/ x, o) :: zs]
    else
      [(x, y) :: insert_order zs s o]
  ];

  {
    xd_base    = d1.xd_base +/ d2.d_base;
    xd_stretch = insert_order d1.xd_stretch d2.d_stretch_factor d2.d_stretch_order;
    xd_shrink  = insert_order d1.xd_shrink  d2.d_shrink_factor  d2.d_shrink_order
  }
};

value dim_neg d =
{
  d_base           = minus_num d.d_base;
  d_stretch_factor = minus_num d.d_stretch_factor;
  d_stretch_order  = d.d_stretch_order;
  d_shrink_factor  = minus_num d.d_shrink_factor;
  d_shrink_order   = d.d_shrink_order
};

value xdim_neg d = do
{
  let neg_list l = List.map (fun [(x,y) -> (minus_num x, y)]) l;
  {
    xd_base    = minus_num d.xd_base;
    xd_stretch = neg_list d.xd_stretch;
    xd_shrink  = neg_list d.xd_shrink
  }
};

value dim_sub  d1 d2 = dim_add  d1 (dim_neg  d2);
value xdim_sub d1 d2 = xdim_add d1 (xdim_neg d2);
value xdim_sub_dim d1 d2 = xdim_add_dim d1 (dim_neg d2);

value dim_mult factor d =
{
  d_base           = factor */ d.d_base;
  d_stretch_factor = factor */ d.d_stretch_factor;
  d_stretch_order  = d.d_stretch_order;
  d_shrink_factor  = factor */ d.d_shrink_factor;
  d_shrink_order   = d.d_shrink_order
};

value xdim_mult factor d = do
{
  let mult_list l = List.map (fun [(x,y) -> (factor */ x, y)]) l;
  {
    xd_base    = factor */ d.xd_base;
    xd_stretch = mult_list d.xd_stretch;
    xd_shrink  = mult_list d.xd_shrink
  }
};

(* maximal and minimal values *)

(*
  |max_extension (<f>, <o>)| returns the maximal increase allowed by <f> and <o>. If it is unlimited
  |infinite| or |-infinite| is returned.
*)

value max_extension (f, o) = do
{
  if o = 0 then
    f
  else if f >=/ num_zero then
    infinite
  else
    minus_num infinite
};

(* These functions return the maximal amount the given dimension can be stretched/shrinked. *)

value dim_max_stretch  dim = max_extension (dim_stretch  dim);
value dim_max_shrink   dim = max_extension (dim_shrink   dim);
value xdim_max_stretch dim = max_extension (xdim_stretch dim);
value xdim_max_shrink  dim = max_extension (xdim_shrink  dim);

(* The functions below return the maximal and minimal extensions of a given dimension. *)

value max_value base extension = do
{
  if extension >=/ infinite then
    infinite
  else if extension <=/ minus_num infinite then
    minus_num infinite
  else
    base +/ extension
};

value dim_max_value  dim = max_value dim.d_base  (dim_max_stretch  dim);
value dim_min_value  dim = max_value dim.d_base  (dim_max_shrink   dim);
value xdim_max_value dim = max_value dim.xd_base (xdim_max_stretch dim);
value xdim_min_value dim = max_value dim.xd_base (xdim_max_shrink  dim);

(*
  |shift_order (<f>, <o>) <delta>| adds <delta> to <f> if <o> is zero. If <f> changes its sign then
  the result is (0, 0).
*)

value shift_order (f, o) delta = do
{
  if o <> 0 then
    (f, o)
  else do
  {
    let x = f +/ delta;

    if (f >=/ num_zero && x >=/ num_zero) ||
       (f </  num_zero && x </  num_zero) then
      (x, o)
    else
      (num_zero, 0)
  }
};

(*
  |dim_shift_base <dim> <delta>| adds <delta> to the base of <dim> without changing the maximal and
  minimal extension of <dim>. The variant |dim_shift_base_upto <dim> <delta>| respects the maximal
  and minimal extensions of <dim>.
*)

value dim_shift_base dim delta = do
{
  let (st_f, st_o) = shift_order (dim_stretch dim) (minus_num delta);
  let (sh_f, sh_o) = shift_order (dim_shrink  dim) delta;
  {
    d_base           = dim.d_base +/ delta;
    d_stretch_factor = st_f;
    d_stretch_order  = st_o;
    d_shrink_factor  = sh_f;
    d_shrink_order   = sh_o
  }
};

value dim_shift_base_upto dim delta = do
{
  if delta >=/ num_zero then do
  {
    if dim.d_stretch_order = 0 && dim.d_stretch_factor </ delta then
      dim_shift_base dim dim.d_stretch_factor
    else
      dim_shift_base dim delta
  }
  else do
  {
    if dim.d_shrink_order = 0 && dim.d_shrink_factor </ minus_num delta then
      dim_shift_base dim (minus_num dim.d_shrink_factor)
    else
      dim_shift_base dim delta
  }
};

(*
  |dim_inc_upto <dim> <delta>| increases <dim> by <delta>. If this value is larger than the maximal
  value of <dim> then this maximum is returned. It is assumed that <delta> is non-negative.
*)

value dim_inc_upto dim delta = do
{
  let max_delta = dim_max_stretch dim;

  if max_delta </ delta then
    dim_shift_base dim max_delta
  else
    dim_shift_base dim delta
};

(*
  |dim_dec_upto <dim> <delta>| decreases <dim> by <delta>. If this value is smaller than the minimal
  value of <dim> then this minimum is returned. It is assumed that <delta> is non-negative.
*)

value dim_dec_upto dim delta = do
{
  let max_delta = dim_max_shrink dim;

  if max_delta </ delta then
    dim_shift_base dim (minus_num max_delta)
  else
    dim_shift_base dim (minus_num delta)
};

(* |dim_resize_upto <dim> <delta>| increases or decreases <dim> by <delta>. *)

value dim_resize_upto dim delta = do
{
  if delta >=/ num_zero then
    dim_inc_upto dim delta
  else
    dim_dec_upto dim delta
};

(* maximum and minimun of dimensions *)

(*
  |min_factor <f1> <f2>| returns the argument of minimal absolute value. If <f1> and <f2> have
  different signs then 0 is returned. Analogously, |max_factor <f1> <f2>| returns the argument
  of maximal absolute value.
*)

value min_factor f1 f2 = do
{
  if f1 <=/ f2 then do
  {
    if f1 >=/ num_zero then
      f1
    else if f2 <=/ num_zero then
      f2
    else
      num_zero
  }
  else do
  {
    if f2 >=/ num_zero then
      f2
    else if f1 <=/ num_zero then
      f1
    else
      num_zero
  }
};

value max_factor f1 f2 = do
{
  if f1 <=/ f2 then do
  {
    if f1 >=/ num_zero then
      f2
    else if f2 <=/ num_zero then
      f1
    else
      num_zero
  }
  else do
  {
    if f2 >=/ num_zero then
      f1
    else if f1 <=/ num_zero then
      f2
    else
      num_zero
  }
};

value min_order (f1, o1) (f2, o2) = do
{
  if o1 < o2 then
    (f1, o1)
  else if o2 < o1 then
    (f2, o2)
  else
    (min_factor f1 f2, o1)
};

value max_order (f1, o1) (f2, o2) = do
{
  if o1 > o2 then
    (f1, o1)
  else if o2 > o1 then
    (f2, o2)
  else
    (max_factor f1 f2, o1)
};

value dim_max d1 d2 = do
{
  if d1.d_base >/ d2.d_base then do
  {
    let delta = d1.d_base -/ d2.d_base;

    let (st_f, st_o) = min_order (dim_stretch d1) (shift_order (dim_stretch d2) (minus_num delta));
    let (sh_f, sh_o) = min_order (dim_shrink  d1) (shift_order (dim_shrink  d2) delta);
    {
      d_base           = d1.d_base;
      d_stretch_factor = st_f;
      d_stretch_order  = st_o;
      d_shrink_factor  = sh_f;
      d_shrink_order   = sh_o
    }
  }
  else do
  {
    let delta = d2.d_base -/ d1.d_base;

    let (st_f, st_o) = min_order (shift_order (dim_stretch d1) (minus_num delta)) (dim_stretch d2);
    let (sh_f, sh_o) = min_order (shift_order (dim_shrink  d1) delta)             (dim_shrink  d2);
    {
      d_base           = d2.d_base;
      d_stretch_factor = st_f;
      d_stretch_order  = st_o;
      d_shrink_factor  = sh_f;
      d_shrink_order   = sh_o
    }
  }
};

value dim_min d1 d2 = do
{
  if d1.d_base </ d2.d_base then do
  {
    let delta = d1.d_base -/ d2.d_base;

    let (st_f, st_o) = min_order (dim_stretch d1) (shift_order (dim_stretch d2) (minus_num delta));
    let (sh_f, sh_o) = min_order (dim_shrink  d1) (shift_order (dim_shrink  d2) delta);
    {
      d_base           = d1.d_base;
      d_stretch_factor = st_f;
      d_stretch_order  = st_o;
      d_shrink_factor  = sh_f;
      d_shrink_order   = sh_o
    }
  }
  else do
  {
    let delta = d2.d_base -/ d1.d_base;

    let (st_f, st_o) = min_order (shift_order (dim_stretch d1) (minus_num delta)) (dim_stretch d2);
    let (sh_f, sh_o) = min_order (shift_order (dim_shrink  d1) delta)             (dim_shrink  d2);
    {
      d_base           = d2.d_base;
      d_stretch_factor = st_f;
      d_stretch_order  = st_o;
      d_shrink_factor  = sh_f;
      d_shrink_order   = sh_o
    }
  }
};

value adjustment_ratio dim size = do
{
  let scale (factor, order) delta = do
  {
    if delta = num_zero then
      (num_zero, 0)
    else if factor = num_zero then do
    {
      if delta >=/ num_zero then
        (infinite, 0)
      else
        (minus_num infinite, 0)
    }
    else
      (delta // factor, order)
  }
  and b = dim.d_base;

  if size </ b then
    scale (dim_shrink dim)  (size -/ b)
  else
    scale (dim_stretch dim) (size -/ b)
};

value dim_scale_badness (factor, order) = do
{
  if order > 0 then num_zero else badness factor
};

(*
  |dim_scale <dim> (<factor>, <order>)| scales <dim> by <factor> times its strechability/shrinkability,
  provided its stretch/shrink order equals <order>. The variant |dim_scale <dim> (<factor>, <order>)|
  respects the maximal and minimal extension of <dim>.
*)

value dim_scale dim (factor, order) = do
{
  if factor </ num_zero then do
  {
    if order = dim.d_shrink_order then
      dim_shift_base dim (factor */ dim.d_shrink_factor)
    else
      dim
  }
  else do
  {
    if order = dim.d_stretch_order then
      dim_shift_base dim (factor */ dim.d_stretch_factor)
    else
      dim
  }
};

value dim_scale_upto dim (factor, order) = do
{
  if factor </ num_zero then do
  {
    if order = dim.d_shrink_order then
      dim_shift_base_upto dim (factor */ dim.d_shrink_factor)
    else
      dim
  }
  else do
  {
    if order = dim.d_stretch_order then
      dim_shift_base_upto dim (factor */ dim.d_stretch_factor)
    else
      dim
  }
};

value xdim_select_order dim stretch_order shrink_order = do
{
  let rec find order list = match list with
  [ []           -> (num_zero, order)
  | [(f,o) :: l] ->
      if o = order then
        (f,order)
      else
        find order l
  ];
  let (st_f, st_o) = find stretch_order dim.xd_stretch;
  let (sh_f, sh_o) = find shrink_order  dim.xd_stretch;
  {
    d_base           = dim.xd_base;
    d_stretch_factor = st_f;
    d_stretch_order  = st_o;
    d_shrink_factor  = sh_f;
    d_shrink_order   = sh_o
  }
};


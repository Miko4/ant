
open XNum;
open Runtime;
open VM;
open ALCoding;

(* opaque type for dimensions *)

value apply_dim dim x = match !x with
[ Types.Symbol s -> do
  {
    if s = sym_Base then
      ref (Types.Number dim.Dim.d_base)
    else if s = sym_Stretch       then
      ref (Types.Tuple [|ref (Types.Number dim.Dim.d_stretch_factor);
                         ref (Types.Number (num_of_int dim.Dim.d_stretch_order))|])
    else if s = sym_StretchFactor then
      ref (Types.Number dim.Dim.d_stretch_factor)
    else if s = sym_StretchOrder  then
      ref (Types.Number (num_of_int dim.Dim.d_stretch_order))
    else if s = sym_Shrink        then
      ref (Types.Tuple [|ref (Types.Number dim.Dim.d_shrink_factor);
                         ref (Types.Number (num_of_int dim.Dim.d_shrink_order))|])
    else if s = sym_ShrinkFactor  then
      ref (Types.Number dim.Dim.d_shrink_factor)
    else if s = sym_ShrinkOrder   then
      ref (Types.Number (num_of_int dim.Dim.d_shrink_order))
    else
      Types.runtime_error "invalid argument"
  }
| _ -> Types.runtime_error "invalid argument"
];

value cmp_dim = Dim.dim_equal;

value (dim_wrapper, dim_unwrapper) = Opaque.declare_type "dimension" apply_dim cmp_dim cmp_dim;

value wrap_dim dim = Types.Opaque (dim_wrapper dim);

value unwrap_dim = decode_opaque "dimension" dim_unwrapper;

(* primitives *)

value prim_make_dim args = match args with
[ [base; st; st_ord; sh; sh_ord] -> do
  {
    let a = Machine.decode_num "make_dim" base;
    let b = Machine.decode_num "make_dim" st;
    let c = decode_int         "make_dim" st_ord;
    let d = Machine.decode_num "make_dim" sh;
    let e = decode_int         "make_dim" sh_ord;

    wrap_dim
      {
        Dim.d_base           = a;
        Dim.d_stretch_factor = b;
        Dim.d_stretch_order  = c;
        Dim.d_shrink_factor  = d;
        Dim.d_shrink_order   = e
      }
  }
| _ -> assert False
];

value prim_fixed_dim base = do
{
  let x = Machine.decode_num "fixed_dim" base;

  wrap_dim (Dim.fixed_dim x)
};

value prim_dim_zero   = wrap_dim Dim.dim_zero;
value prim_dim_1pt    = wrap_dim Dim.dim_1pt;
value prim_dim_12pt   = wrap_dim Dim.dim_12pt;
value prim_dim_fil    = wrap_dim Dim.dim_fil;
value prim_dim_fill   = wrap_dim Dim.dim_fill;
value prim_dim_ss     = wrap_dim Dim.dim_ss;
value prim_dim_filneg = wrap_dim Dim.dim_filneg;

value prim_dim_equal dim0 dim1 = do
{
  let d0 = unwrap_dim "dim_equal" dim0;
  let d1 = unwrap_dim "dim_equal" dim1;

  Types.Bool (Dim.dim_equal d0 d1)
};

value prim_dim_add dim0 dim1 = do
{
  let d0 = unwrap_dim "dim_add" dim0;
  let d1 = unwrap_dim "dim_add" dim1;

  wrap_dim (Dim.dim_add d0 d1)
};

value prim_dim_neg dim = do
{
  let d = unwrap_dim "dim_neg" dim;

  wrap_dim (Dim.dim_neg d)
};

value prim_dim_sub dim0 dim1 = do
{
  let d0 = unwrap_dim "dim_sub" dim0;
  let d1 = unwrap_dim "dim_sub" dim1;

  wrap_dim (Dim.dim_sub d0 d1)
};

value prim_dim_mult x dim = do
{
  let a = Machine.decode_num "dim_mult" x;
  let d = unwrap_dim         "dim_mult" dim;

  wrap_dim (Dim.dim_mult a d)
};

value prim_dim_max dim0 dim1 = do
{
  let d0 = unwrap_dim "dim_max" dim0;
  let d1 = unwrap_dim "dim_max" dim1;

  wrap_dim (Dim.dim_max d0 d1)
};

value prim_dim_min dim0 dim1 = do
{
  let d0 = unwrap_dim "dim_min" dim0;
  let d1 = unwrap_dim "dim_min" dim1;

  wrap_dim (Dim.dim_min d0 d1)
};

value prim_dim_max_stretch dim = do
{
  let d = unwrap_dim "dim_max_stretch" dim;

  Types.Number (Dim.dim_max_stretch d)
};

value prim_dim_max_shrink dim = do
{
  let d = unwrap_dim "dim_max_shrink" dim;

  Types.Number (Dim.dim_max_shrink d)
};

value prim_dim_max_value dim = do
{
  let d = unwrap_dim "dim_max_value" dim;

  Types.Number (Dim.dim_max_value d)
};

value prim_dim_min_value dim = do
{
  let d = unwrap_dim "dim_min_value" dim;

  Types.Number (Dim.dim_min_value d)
};

value prim_dim_shift_base dim x = do
{
  let d = unwrap_dim         "dim_shift_base" dim;
  let y = Machine.decode_num "dim_shift_base" x;

  wrap_dim (Dim.dim_shift_base d y)
};

value prim_dim_shift_base_upto dim x = do
{
  let d = unwrap_dim         "dim_shift_base_upto" dim;
  let y = Machine.decode_num "dim_shift_base_upto" x;

  wrap_dim (Dim.dim_shift_base_upto d y)
};

value prim_dim_inc_upto dim x = do
{
  let d = unwrap_dim         "dim_inc_upto" dim;
  let y = Machine.decode_num "dim_inc_upto" x;

  wrap_dim (Dim.dim_inc_upto d y)
};

value prim_dim_dec_upto dim x = do
{
  let d = unwrap_dim         "dim_dec_upto" dim;
  let y = Machine.decode_num "dim_dec_upto" x;

  wrap_dim (Dim.dim_dec_upto d y)
};

value prim_dim_resize_upto dim x = do
{
  let d = unwrap_dim         "dim_resize_upto" dim;
  let y = Machine.decode_num "dim_resize_upto" x;

  wrap_dim (Dim.dim_resize_upto d y)
};

value prim_adjustment_ratio dim x = do
{
  let d = unwrap_dim         "dim_adjustment_ratio" dim;
  let y = Machine.decode_num "dim_adjustment_ratio" x;

  let (a,b) = Dim.adjustment_ratio d y;

  Types.Tuple [|ref (Types.Number a); ref (Types.Number (num_of_int b))|]
};

value prim_dim_scale_badness ratio = match !ratio with
[ Types.Tuple [|x; y|] -> do
  {
    let a = Machine.decode_num "dim_scale_badness" x;
    let b = decode_int         "dim_scale_badness" y;

    Types.Number (Dim.dim_scale_badness (a,b))
  }
| _ -> Types.runtime_error "dim_scale_badness: invalid argument"
];

value prim_dim_scale dim ratio = do
{
  let d = unwrap_dim "dim_scale" dim;

  match !ratio with
  [ Types.Tuple [|y; z|] -> do
    {
      let a = Machine.decode_num "dim_scale" y;
      let b = decode_int         "dim_scale" z;

      wrap_dim (Dim.dim_scale d (a,b))
    }
  | _ -> Types.runtime_error "dim_scale: invalid argument"
  ]
};

value prim_dim_scale_upto dim ratio = do
{
  let d = unwrap_dim "dim_scale_upto" dim;

  match !ratio with
  [ Types.Tuple [|y; z|] -> do
    {
      let a = Machine.decode_num "dim_scale_upto" y;
      let b = decode_int         "dim_scale_upto" z;

      wrap_dim (Dim.dim_scale_upto d (a,b))
    }
  | _ -> Types.runtime_error "dim_scale_upto: invalid argument"
  ]
};


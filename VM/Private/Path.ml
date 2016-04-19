
open XNum;
open Types;
open Runtime;

module UString   = Unicode.UString;
module SymbolMap = Unicode.SymbolTable.SymbolMap;

(* Opaque type for path specifications *)

value apply_ps _ _ = runtime_error "application of non-function";

value cmp_ps p1 p2 = p1 == p2;

value (ps_wrapper, ps_unwrapper) = Opaque.declare_type "path-specification" apply_ps cmp_ps cmp_ps;

value wrap_ps ps = Opaque (ps_wrapper ps);

value unwrap_ps = Evaluate.evaluate_opaque "path-specification" ps_unwrapper;

value evaluate_vec name v = do
{
  match !v with
  [ Tuple [|x; y|] -> do
    {
      (Evaluate.evaluate_num name x,
       Evaluate.evaluate_num name y)
    }
  | _ -> runtime_error (name ^ ": pair expected but got " ^ type_name !v)
  ]
};

value rec make_path p = do
{
  let (x,y) = evaluate_vec "make_path" p;

  wrap_ps (Bezier.make_spec x y)
};

value close_path cycle spec = do
{
  let encode_pair x y =
    Tuple [|ref (Number x); ref (Number y)|];

  let ps = unwrap_ps "close_path" spec;

  match !cycle with
  [ Bool c -> do
    {
      Array.fold_right
        (fun (x0,y0,x1,y1,x2,y2,x3,y3) lst ->
          List
            (ref (Tuple
                   [|ref (encode_pair x0 y0);
                     ref (encode_pair x1 y1);
                     ref (encode_pair x2 y2);
                     ref (encode_pair x3 y3)|]))
            (ref lst))
        (Bezier.close_spec ps c)
        Nil
    }
  | _ -> runtime_error ("close_path: boolean expected but got " ^ type_name !cycle)
  ]
};

value add_point p spec = do
{
  let ps    = unwrap_ps "path_add_point" spec;
  let (x,y) = evaluate_vec "path_add_point" p;

  wrap_ps (Bezier.add_point ps x y)
};

value add_in_dir dir spec = do
{
  let ps       = unwrap_ps "path_add_in_dir" spec;
  let (dx, dy) = evaluate_vec "path_add_in_dir" dir;

  wrap_ps (Bezier.add_in_dir ps (Bezier.angle_of_vec dx dy))
};

value add_in_angle angle spec = do
{
  let ps = unwrap_ps "path_add_in_angle" spec;
  let a  = Evaluate.evaluate_num "path_add_in_angle" angle;

  wrap_ps (Bezier.add_in_dir ps (float_of_num a))
};

value add_in_curl curl spec = do
{
  let ps = unwrap_ps "path_add_in_curl" spec;
  let c  = Evaluate.evaluate_num "path_add_in_curl" curl;

  wrap_ps (Bezier.add_in_curl ps c)
};

value add_in_tension tension spec = do
{
  let ps = unwrap_ps "path_add_in_tension" spec;
  let t  = Evaluate.evaluate_num "path_add_in_tension" tension;

  wrap_ps (Bezier.add_in_tension ps t)
};

value add_out_dir dir spec = do
{
  let ps       = unwrap_ps "path_add_out_dir" spec;
  let (dx, dy) = evaluate_vec "path_add_out_dir" dir;

  wrap_ps (Bezier.add_out_dir ps (Bezier.angle_of_vec dx dy))
};

value add_out_angle angle spec = do
{
  let ps = unwrap_ps "path_add_out_angle" spec;
  let a  = Evaluate.evaluate_num "path_add_out_angle" angle;

  wrap_ps (Bezier.add_out_dir ps (float_of_num a))
};

value add_out_curl curl spec = do
{
  let ps = unwrap_ps "path_add_out_curl" spec;
  let c  = Evaluate.evaluate_num "path_add_out_curl" curl;

  wrap_ps (Bezier.add_out_curl ps c)
};

value add_out_tension tension spec = do
{
  let ps = unwrap_ps "path_add_out_tension" spec;
  let t  = Evaluate.evaluate_num "path_add_out_tension" tension;

  wrap_ps (Bezier.add_out_tension ps t)
};

value add_control_points args = match args with
[ [p1; p2; spec] -> do
  {
    let ps      = unwrap_ps "path_add_control_points" spec;
    let (x1,y1) = evaluate_vec "path_add_control_points" p1;
    let (x2,y2) = evaluate_vec "path_add_control_points" p2;

    wrap_ps (Bezier.add_control_points ps x1 y1 x2 y2)
  }
| _ -> assert False
];


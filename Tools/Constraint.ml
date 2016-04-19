
open XNum;

exception Inconsistent;

type var = int;              (* index into array *)

type bound    = (var * num); (* (v, n) represents  v + n *)

type equation = LinForm.lin_form var;

type coordinate =
{
  min          : num;
  max          : num;
  lower_bounds : list bound;
  upper_bounds : list bound;
  equations    : list equation
};

type system =
{
  x_coords : mutable array coordinate;
  y_coords : mutable array coordinate;
  num_vars : mutable int
};

value create () =
{
  x_coords = [| |];
  y_coords = [| |];
  num_vars = 0
};

value clone system =
{
  x_coords = Array.copy system.x_coords;
  y_coords = Array.copy system.y_coords;
  num_vars = system.num_vars
};

value compare_int (x : int) (y : int) = do
{
  if x < y then
    LinForm.Lt
  else if x > y then
    LinForm.Gt
  else
    LinForm.Eq
};

value add_variable system min_x min_y max_x max_y = do
{
  let x =
  {
    min          = min_x;
    max          = max_x;
    lower_bounds = [];
    upper_bounds = [];
    equations    = []
  };
  let y =
  {
    min          = min_y;
    max          = max_y;
    lower_bounds = [];
    upper_bounds = [];
    equations    = []
  };

  if system.num_vars < Array.length system.x_coords then do
  {
    system.x_coords.(system.num_vars) := x;
    system.y_coords.(system.num_vars) := y
  }
  else do
  {
    system.x_coords :=
      Array.init (2 * system.num_vars)
                 (fun i -> if i < system.num_vars then
                             system.x_coords.(i)
                           else
                             x);
    system.y_coords :=
      Array.init (2 * system.num_vars)
                 (fun i -> if i < system.num_vars then
                             system.y_coords.(i)
                           else
                             y)
  };

  system.num_vars := system.num_vars + 1
};

value update sys = do
{
  let eval_lin_form get_coord lf = do
  {
    List.fold_left
      (fun (min, max) (a, x) ->
        if a >=/ num_zero then
          (min +/ a */ (get_coord x).min,
           max +/ a */ (get_coord x).max)
        else
          (min +/ a */ (get_coord x).max,
           max +/ a */ (get_coord x).min)
      )
      (lf.LinForm.const, lf.LinForm.const)
      lf.LinForm.terms
  };
  let update_coord get_coord coord = do
  {
    let new_min =
      List.fold_left
        (fun min (var, const) ->
            max_num min ((get_coord var).min +/ const))
        coord.min
        coord.lower_bounds;
    let new_max =
      List.fold_left
        (fun max (var, const) ->
            min_num max ((get_coord var).max +/ const))
        coord.max
        coord.upper_bounds;
    let (new_min2, new_max2) =
      List.fold_left
        (fun (min, max) eq -> do
          {
            let (l, u) = eval_lin_form get_coord eq;
            (max_num min l, min_num max u)
          })
        (new_min, new_max)
        coord.equations;

    if new_min2 >/ new_max2 then
      raise Inconsistent
    else if new_min2 >/ coord.min || new_max2 </ coord.max then
      Some { (coord) with min = new_min2; max = new_max2 }
    else
      None
  };
  let get_x_coord v = sys.x_coords.(v);
  let get_y_coord v = sys.y_coords.(v);

  let changed = ref True;

  while !changed do
  {
    !changed := False;

    for i = 0 to sys.num_vars - 1 do
    {
      if sys.x_coords.(i).min </ sys.x_coords.(i).max then
        match update_coord get_x_coord sys.x_coords.(i) with
        [ None   -> ()
        | Some c -> do { sys.x_coords.(i) := c; !changed := True }
        ]
      else ();

      if sys.y_coords.(i).min </ sys.y_coords.(i).max then
        match update_coord get_y_coord sys.y_coords.(i) with
        [ None   -> ()
        | Some c -> do { sys.y_coords.(i) := c; !changed := True }
        ]
      else ()
    }
  }
};

value set_x_coord_without_update system var x = do
{
  let old_x = system.x_coords.(var);

  if x </ old_x.min || x >/ old_x.max then
    raise Inconsistent
  else ();

  system.x_coords.(var) :=
    { (system.x_coords.(var)) with min = x; max = x }
};

value set_y_coord_without_update system var y = do
{
  let old_y = system.y_coords.(var);

  if y </ old_y.min || y >/ old_y.max then
    raise Inconsistent
  else ();

  system.y_coords.(var) :=
    { (system.y_coords.(var)) with min = y; max = y }
};

value set_variable system var x y = do
{
  set_x_coord_without_update system var x;
  set_y_coord_without_update system var y;

  update system
};
value set_x_coord system var x = do
{
  set_x_coord_without_update system var x;

  update system
};
value set_y_coord system var y = do
{
  set_y_coord_without_update system var y;

  update system
};

value add_equation system add_eq eq = do
{
  match eq.LinForm.terms with
  [ [] ->
      if eq.LinForm.const =/ num_zero then
        ()
      else
        raise Inconsistent
  | _ -> do
    {
      iter eq (LinForm.of_num compare_int eq.LinForm.const)

      where rec iter eq1 eq2 = match eq1.LinForm.terms with
      [ []           -> update system
      | [(a,v) :: _] -> do
        {
          let new_eq1 = LinForm.remove_first_term eq1;
          let lf      = LinForm.scale (num_minus_one // a)
                                      (LinForm.add new_eq1 eq2);

          add_eq system v lf;

          iter new_eq1 (LinForm.add_unknown eq2 a v)
        }
      ]
    }
  ]
};

value add_x_equation system eq = do
{
  let add_x_eq system var eq = do
  {
    system.x_coords.(var) :=
      {
        (system.x_coords.(var))

        with equations = [eq :: system.x_coords.(var).equations]
      }
   };

   add_equation system add_x_eq eq
};

value add_y_equation system eq = do
{
  let add_y_eq system var eq = do
  {
    system.y_coords.(var) :=
      {
        (system.y_coords.(var))

        with equations = [eq :: system.y_coords.(var).equations]
      }
   };

   add_equation system add_y_eq eq
};

(* v1 - v2 >= off *)

value add_bound c1 c2 v1 v2 off = do
{
  ({ (c1) with lower_bounds = [(v2, off)           :: c1.lower_bounds] },
   { (c2) with upper_bounds = [(v1, minus_num off) :: c2.lower_bounds] })
};

value add_x_bound system v1 v2 off = do
{
  let (c1,c2) = add_bound system.x_coords.(v1) system.x_coords.(v2) v1 v2 off;

  system.x_coords.(v1) := c1;
  system.x_coords.(v2) := c2;

  update system
};

value add_y_bound system v1 v2 off = do
{
  let (c1,c2) = add_bound system.y_coords.(v1) system.y_coords.(v2) v1 v2 off;

  system.y_coords.(v1) := c1;
  system.y_coords.(v2) := c2;

  update system
};


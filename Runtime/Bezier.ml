
open XNum;

type spline = (num * num * num * num * num * num * num * num);

type side_spec =
[ Open
| Curl of num
| Angle of float
| Explicit of num and num
| Endpoint
| Cycle
];

type knot =
{
  left_spec     : side_spec;
  right_spec    : side_spec;
  left_tension  : num;
  right_tension : num;
  point_x       : num;
  point_y       : num
};

type path_spec = list knot;

value log_knot k = do
{
  Printf.eprintf "(%f,%f)" (float_of_num k.point_x) (float_of_num k.point_y);
  match k.left_spec with
  [ Open -> Printf.eprintf " open"
  | Curl c -> Printf.eprintf " curl %f" (float_of_num c)
  | Angle a -> Printf.eprintf " angle %f" a
  | Explicit x y -> Printf.eprintf " explicit %f %f" (float_of_num x) (float_of_num y)
  | Endpoint -> Printf.eprintf " endpoint"
  | Cycle -> Printf.eprintf " cycle"
  ];
  Printf.eprintf " %f" (float_of_num k.left_tension);
  match k.right_spec with
  [ Open -> Printf.eprintf " open"
  | Curl c -> Printf.eprintf " curl %f" (float_of_num c)
  | Angle a -> Printf.eprintf " angle %f" a
  | Explicit x y -> Printf.eprintf " explicit %f %f" (float_of_num x) (float_of_num y)
  | Endpoint -> Printf.eprintf " endpoint"
  | Cycle -> Printf.eprintf " cycle"
  ];
  Printf.eprintf " %f\n" (float_of_num k.right_tension);
};
value log_knots knots = for i = 0 to Array.length knots - 1 do
{
  Printf.eprintf "%d: " i; log_knot knots.(i)
};

(* side specifications *)

value is_open spec = match spec with
[ Open -> True
| _    -> False
];

value is_curl spec = match spec with
[ Curl _ -> True
| _      -> False
];

value is_angle spec = match spec with
[ Angle _ -> True
| _       -> False
];

value is_explicit spec = match spec with
[ Explicit _ _ -> True
| _            -> False
];

value is_endpoint spec = match spec with
[ Endpoint -> True
| _        -> False
];

value is_cycle spec = match spec with
[ Cycle -> True
| _     -> False
];

value curl spec = match spec with
[ Curl c -> c
| _      -> invalid_arg "Bezier.curl"
];

value convert_open_to_curl spec = match spec with
[ Open -> Curl num_one
| _    -> spec
];

(* geometric functions *)

value pi = 4.0 *. atan 1.0;

value angle_of_vec x y = 180.0 /. pi *. atan2 (float_of_num y) (float_of_num x);

value sind x = sin (pi *. x /. 180.0);
value cosd x = cos (pi *. x /. 180.0);

value fast_reduce_angle a = do
{
  if a > 180.0 then
    a -. 180.0
  else if a < -180.0 then
    a +. 180.0
  else
    a
};

value slow_reduce_angle a = do
{
  mod_float a 360.0
};

value num_4      = num_of_int 4;
value num_16     = num_of_int 16;
value num_sqrt_2 = num_of_float (sqrt 2.0);
value num_sqrt_5 = num_of_float (sqrt 5.0);

value const1 = num_of_ints 3 2 */ (num_sqrt_5 -/ num_one);
value const2 = num_of_ints 3 2 */ (num_three -/ num_sqrt_5);

value velocity sin_theta cos_theta sin_phi cos_phi tension = do
{
  let n = num_two +/
            num_sqrt_2 */ (sin_theta -/ sin_phi // num_16)
                       */ (sin_phi -/ sin_theta // num_16)
                       */ (cos_theta -/ cos_phi);
  let d = num_three +/ const1 */ cos_theta +/ const2 */ cos_phi;
  let x = n // (d */ tension);

  if x >=/ num_4 then
    num_4
  else
    x
};

value curl_ratio curl left_tension right_tension = do
{
  let alpha = num_one // right_tension;
  let beta  = num_one // left_tension;

  let r     = alpha // beta;
  let gamma = r */ r */ curl;

  let d = gamma */ alpha +/ num_three -/ beta;
  let n = gamma */ (num_three -/ alpha) +/ beta;
  let x = n // d;

  if x >=/ num_4 then
    num_4
  else
    x
};

(* Join knots with the same coordinates by a straight line. *)

value join_points spec = do
{
  let len = Array.length spec;

  for i = 0 to len - 1 do
  {
    let n  = if i + 1 < len then
               i + 1
             else
               0;
    let k1 = spec.(i);
    let k2 = spec.(n);

    if k1.point_x =/ k2.point_x &&
       k1.point_y =/ k2.point_y then do
    {
      (* add straight line (of length 0!) between <k> and <last_knot> *)

      match k1.right_spec with
      [ Open | Curl _ | Angle _ -> do
        {
          spec.(i) :=
            {
              (k1)
              with
              right_spec = Explicit k2.point_x k2.point_y;
              left_spec  = convert_open_to_curl k1.left_spec
            };
          spec.(n) :=
            {
              (k2)
              with
              left_spec  = Explicit k1.point_x k1.point_y;
              right_spec = convert_open_to_curl k2.right_spec
            }
        }
      | _ -> ()
      ]
    }
    else ()
  }
};

value calculate_angles spec = do
{
  let len     = Array.length spec;
  let delta_x = Array.make len num_zero;   (* vector from one point to the next one *)
  let delta_y = Array.make len num_zero;
  let dist    = Array.make len num_zero;   (* distance between the current point and the next one *)
  let psi     = Array.make len num_zero;   (* turning angle at the corresponding point *)

  for i = 0 to len - 1 do
  {
    let n  = if i + 1 < len then
               i + 1
             else
               0;
    let k1 = spec.(i);
    let k2 = spec.(n);

    delta_x.(i) := k2.point_x -/ k1.point_x;
    delta_y.(i) := k2.point_y -/ k1.point_y;
    dist.(i)    := num_of_float (sqrt (float_of_num (delta_x.(i) */ delta_x.(i) +/ delta_y.(i) */ delta_y.(i))));

    if i > 0 then do
    {
      let sine   = delta_y.(i-1) // dist.(i-1);
      let cosine = delta_x.(i-1) // dist.(i-1);

      psi.(i) := num_of_float (angle_of_vec (delta_x.(i) */ cosine +/ delta_y.(i) */ sine)
                                            (delta_y.(i) */ cosine -/ delta_x.(i) */ sine))
    }
    else ()
  };

  if is_cycle spec.(0).left_spec then do
  {
    let sine   = delta_y.(len-1) // dist.(len-1);
    let cosine = delta_x.(len-1) // dist.(len-1);

    psi.(0) := num_of_float (angle_of_vec (delta_x.(0) */ cosine +/ delta_y.(0) */ sine)
                                          (delta_y.(0) */ cosine -/ delta_x.(0) */ sine));
    psi.(len-1) := psi.(0)
  }
  else do
  {
    psi.(0)       := num_zero;
    psi.(len - 1) := num_zero
  };

  (delta_x, delta_y, dist, psi)
};

value remove_open_at_endpoints spec first_knot last_knot = do
{
  let k1 = spec.(first_knot);
  let k2 = spec.(last_knot);

  if is_open k1.right_spec then do
  {
    match k1.left_spec with
    [ Explicit x y -> do
      {
        let dx = k1.point_x -/ x;
        let dy = k1.point_y -/ y;

        if dx =/ num_zero && dy =/ num_zero then
          spec.(first_knot) :=
            { (k1) with right_spec = Curl num_one }
        else
          spec.(first_knot) :=
            { (k1) with right_spec = Angle (angle_of_vec dx dy) }
      }
    | _ -> ()
    ]
  }
  else ();

  if is_open k2.left_spec then do
  {
    match k2.right_spec with
    [ Explicit x y -> do
      {
        let dx = x -/ k2.point_x;
        let dy = y -/ k2.point_y;

        if dx =/ num_zero && dy =/ num_zero then
          spec.(last_knot) :=
            { (k2) with left_spec = Curl num_one }
        else
          spec.(last_knot) :=
            { (k2) with left_spec = Angle (angle_of_vec dx dy) }
      }
    | _ -> ()
    ]
  }
  else ()
};

value set_control_points knots left_knot right_knot delta_x delta_y sin_theta cos_theta sin_phi cos_phi = do
{
  let lk = knots.(left_knot);
  let rk = knots.(right_knot);

  let left_tension  = lk.right_tension;
  let right_tension = rk.left_tension;

  let left_vel  = velocity sin_theta cos_theta sin_phi cos_phi (abs_num left_tension);
  let right_vel = velocity sin_phi cos_phi sin_theta cos_theta (abs_num right_tension);

  let (left_vel2, right_vel2) =
    if (left_tension </ num_zero || right_tension </ num_zero)
    && ((sin_theta >=/ num_zero && sin_phi >=/ num_zero) ||
        (sin_theta <=/ num_zero && sin_phi <=/ num_zero)) then do
    {
      let st = abs_num sin_theta;
      let sp = abs_num sin_phi;
      let s = num_of_ints 4097 4096 */ (st */ cos_phi +/ sp */ cos_theta);

      if s >/ num_zero then do
      {
        let l = if left_tension </ num_zero then
                  min_num (sp // s) left_vel
                else
                  left_vel;
        let r = if right_tension </ num_zero then
                  min_num (st // s) right_vel
                else
                  right_vel;

        (l,r)
      }
      else
        (left_vel, right_vel)
    }
    else
     (left_vel, right_vel);

  let left_x  = lk.point_x +/ left_vel2  */ (delta_x */ cos_theta -/ delta_y */ sin_theta);
  let left_y  = lk.point_y +/ left_vel2  */ (delta_y */ cos_theta +/ delta_x */ sin_theta);
  let right_x = rk.point_x -/ right_vel2 */ (delta_x */ cos_phi   +/ delta_y */ sin_phi);
  let right_y = rk.point_y -/ right_vel2 */ (delta_y */ cos_phi   -/ delta_x */ sin_phi);

  knots.(left_knot)  := { (lk) with right_spec = Explicit left_x  left_y  };
  knots.(right_knot) := { (rk) with left_spec  = Explicit right_x right_y }
};

value calculate_splines knots (delta_x, delta_y, dist, psi) first_knot last_knot = do
{
  (* We solve a system of equations of the form  x_i + u_i x_{i+1} = v_i + w_i x_0 *)

  let len = Array.length knots;

  let idx i =
    if i < 0 then
      i + len
    else if i >= len then
      i - len
    else
      i;

  let u     = Array.make len num_zero;
  let v     = Array.make len num_zero;
  let w     = Array.make len num_zero;
  let theta = Array.make len num_zero;

  let next = idx (first_knot+1);

  match knots.(first_knot).right_spec with
  [ Angle a -> match knots.(next).left_spec with
    [ Angle a2 -> do
      {
        let angle = angle_of_vec delta_x.(first_knot) delta_y.(first_knot);

        set_control_points
          knots first_knot next
          delta_x.(first_knot) delta_y.(first_knot)
          (num_of_float (sind (a -. angle)))
          (num_of_float (cosd (a -. angle)))
          (num_of_float (sind (a2 -. angle)))
          (num_of_float (cosd (a2 -. angle)))
      }
    | _ -> do
      {
        (* theta_0 + 0 * theta_1 = a + 0 * theta_0 *)

        u.(first_knot) := num_zero;
        v.(first_knot) := num_of_float (fast_reduce_angle (a -. angle_of_vec delta_x.(first_knot) delta_y.(first_knot)));
        w.(first_knot) := num_zero
      }
    ]
  | Curl c -> match knots.(next).left_spec with
    [ Curl _ -> do
      {
        let kl = knots.(first_knot);
        let kr = knots.(next);

        let sl = num_three */ (abs_num kl.right_tension);
        let sr = num_three */ (abs_num kr.left_tension);

        knots.(first_knot) :=
          {
            (kl) with right_spec = Explicit (kl.point_x +/ delta_x.(first_knot) // sl)
                                            (kl.point_y +/ delta_y.(first_knot) // sl)
          };
        knots.(next) :=
          {
            (kr) with left_spec = Explicit (kr.point_x -/ delta_x.(first_knot) // sr)
                                           (kr.point_y -/ delta_y.(first_knot) // sr)
          }
      }
    | _ -> do
      {
        let lt = abs_num knots.(first_knot).right_tension;
        let rt = abs_num knots.(next).left_tension;

        if lt =/ num_one && rt =/ num_one then
          u.(first_knot) := (c +/ c +/ num_one) // (c +/ num_two)
        else
          u.(first_knot) := curl_ratio c lt rt;

        v.(first_knot) := minus_num (psi.(next) */ u.(first_knot));
        w.(first_knot) := num_zero
      }
    ]
  | Open -> do
    {
      u.(first_knot) := num_zero;
      v.(first_knot) := num_zero;
      w.(first_knot) := num_one
    }
  | _ -> assert False
  ];

  iter (idx (first_knot+1))

  where rec iter middle = do
  {
    let left  = idx (middle-1);
    let right = idx (middle+1);

    match knots.(middle).left_spec with
    [ Cycle | Open -> do
      {
        let lt  = abs_num knots.(left).right_tension;
        let rt  = abs_num knots.(right).left_tension;

        let a = num_one // (num_three */ lt -/ num_one);
        let b = num_one // (num_three */ rt -/ num_one);
        let c = num_one -/ a */ u.(left);
        let d = dist.(middle) */ (num_three -/ (num_one // lt));
        let e = dist.(left)   */ (num_three -/ (num_one // rt));

        let s = abs_num (knots.(middle).left_tension //
                         knots.(middle).right_tension);
        let f = e // (e +/ c */ d */ s */ s);

        u.(middle) := b */ f;

        let acc = minus_num (psi.(right) */ u.(middle));

        if is_curl knots.(left).right_spec then do
        {
          v.(middle) := acc -/ psi.(idx (first_knot + 1)) */ (num_one -/ f);
          w.(middle) := num_zero
        }
        else do
        {
          let f   = (num_one -/ f) // c;
          let acc = acc -/ psi.(middle) */ f;
          let f   = a */ f;

          v.(middle) := acc -/ f */ v.(left);
          w.(middle) := minus_num (f */ w.(left))
        };

        if is_cycle knots.(middle).left_spec then do
        {
          (* set theta.(first_knot) = theta.(last_knot) *)

          iter2 num_zero num_one left

          where rec iter2 a b i = do
          {
            let c = v.(i) -/ a */ u.(i);
            let d = w.(i) -/ b */ u.(i);

            if i <> last_knot then
              iter2 c d (idx (i-1))
            else do
            {
              let e = c // (num_one -/ d);

              theta.(last_knot) := e;
              v.(first_knot)    := e;

              iter3 (idx (first_knot + 1))

              where rec iter3 i = do
              {
                let n = idx (i+1);

                v.(i) := v.(i) +/ e */ w.(i);

                if n <> last_knot then
                  iter3 n
                else ()
              }
            }
          }
        }
        else ()
      }
    | Curl c -> do
      {
        let lt = abs_num knots.(middle).left_tension;
        let rt = abs_num knots.(left).right_tension;

        let s = if lt =/ num_one && rt =/ num_one then
                  (c +/ c +/ num_one) // (c +/ num_two)
                else
                  curl_ratio c rt lt;

        theta.(middle) := minus_num (s */ v.(left)) // (num_one -/ s */ u.(left));
      }
    | Angle a -> do
      {
        theta.(middle) := num_of_float (fast_reduce_angle (a -. angle_of_vec delta_x.(left) delta_y.(left)));
      }
    | _ -> () (* We have already computed the values. *)
    ];

    if middle <> last_knot then
      iter right
    else ()
  };

  iter (idx (last_knot - 1))

  where rec iter i = do
  {
    let n = idx (i+1);

    if i <> last_knot then                      (* If the path is cyclic then we have already        *)
      theta.(i) := v.(i) -/ u.(i) */ theta.(n)  (* computed theta.(i) for i = first_knot = last_knot *)
    else ();

    let phi       = minus_num psi.(n) -/ theta.(n);
    let sin_theta = sind (float_of_num theta.(i));
    let cos_theta = cosd (float_of_num theta.(i));
    let sin_phi   = sind (float_of_num phi);
    let cos_phi   = cosd (float_of_num phi);

    set_control_points
      knots i n
      delta_x.(i) delta_y.(i)
      (num_of_float sin_theta)
      (num_of_float cos_theta)
      (num_of_float sin_phi)
      (num_of_float cos_phi);

    if i <> first_knot then
      iter (idx (i-1))
    else ()
  }
};

value compute_path spec = do
{
  let knots = Array.of_list spec;
  let len   = Array.length knots;

  if len < 2 then do
  {
    if len = 0 then
      [||]
    else do
    {
      let k = knots.(0);
      [|(k.point_x,k.point_y,
         k.point_x,k.point_y,
         k.point_x,k.point_y,
         k.point_x,k.point_y)|]
    }
  }
  else do
  {
    join_points knots;

    let dist_psi = calculate_angles knots;

    let idx i =
      if i < 0 then
        i + len
      else if i >= len then
        i - len
      else
        i;

    let make_path knots = do
    {
      let n = if is_endpoint knots.(0).left_spec then
                Array.length knots - 1
              else
                Array.length knots;
      let make_spline i = do
      {
        let k1 = knots.(i);
        let k2 = knots.(idx (i+1));

        match (k1.right_spec,k2.left_spec) with
        [ (Explicit x1 y1, Explicit x2 y2) ->
            (k1.point_x, k1.point_y, x1, y1, x2, y2, k2.point_x, k2.point_y)
        | _ -> assert False
        ]
      };

      Array.init n make_spline
    };

    let rec find_next_break i = do
    {
      match (knots.(i).left_spec, knots.(i).right_spec) with
      [ (Open, Open) -> find_next_break (idx (i+1))
      | _            -> i
      ]
    };
    let first_break = match knots.(0).left_spec with
    [ Endpoint -> 0
    | Cycle    -> find_next_break 1
    | _        -> assert False
    ];

    if first_break > 0 then do
    {
      (* The path is cyclic and it contains a real break point.
         We replace the |Cycle| by |Open|. *)

      knots.(0) := { (knots.(0)) with left_spec = Open }
    }
    else ();

    iter first_break

    where rec iter cur_break = do
    {
      let continue next_break = do
      {
        if next_break <> first_break
        && not (is_endpoint knots.(next_break).right_spec) then
          iter next_break
        else
          make_path knots
      };

      if is_explicit knots.(cur_break).right_spec then
        (* nothing to do *)
        continue (idx (cur_break+1))
      else do
      {
        let next_break = find_next_break (idx (cur_break+1));

        remove_open_at_endpoints knots cur_break next_break;

        calculate_splines knots dist_psi cur_break next_break;

        continue next_break
      }
    }
  }
};

(* building path specifications *)

value make_spec x y =
[{
  left_spec     = Endpoint;
  right_spec    = Open;
  left_tension  = num_one;
  right_tension = num_one;
  point_x       = x;
  point_y       = y
}];

value make_specs_consistent knot = match (knot.left_spec, knot.right_spec) with
[ (Open, Curl c)   -> { (knot) with left_spec  = Curl c       }
| (Open, Angle a)  -> { (knot) with left_spec  = Angle a      }
| (Open, Endpoint) -> { (knot) with left_spec  = Curl num_one }
| (Curl c,   Open) -> { (knot) with right_spec = Curl c       }
| (Angle a,  Open) -> { (knot) with right_spec = Angle a      }
| (Endpoint, Open) -> { (knot) with right_spec = Curl num_one }
| _                -> knot
];

value rec cleanup_spec cycle result spec = match spec with
[ [k] -> do
  {
    if cycle then do
    {
      let x = make_specs_consistent { (k) with left_spec = Open };
      let y = { (x) with left_spec = Cycle };

      [y :: result]
    }
    else
      [make_specs_consistent k :: result]
  }
| [k::ks] -> cleanup_spec cycle [make_specs_consistent k :: result] ks
| []      -> assert False
];

value close_spec spec cycle = match spec with
[ []      -> assert False
| [k::ks] -> do
  {
    if is_cycle k.right_spec then
      invalid_arg "Bezier.close_spec: last point misssing!"
    else do
    {
      let s = if cycle then
                spec
              else
                [ { (k) with right_spec = Endpoint }
                  :: ks ];

      compute_path (cleanup_spec cycle [] s)
    }
  }
];

value add_point spec x y = match spec with
[ []      -> assert False
| [k::ks] -> do
  {
    if is_cycle k.right_spec then     (* |Cycle| indicates that the coordinates of <k> are invalid *)
      [ {
          (k)
          with
          right_spec = Open;
          point_x    = x;
          point_y    = y
        }
        :: ks ]
    else
      [ { left_spec     = Open;
          right_spec    = Open;
          left_tension  = num_one;
          right_tension = num_one;
          point_x       = x;
          point_y       = y }
        :: spec ]
  }
];

value add_in_dir spec angle = match spec with
[ []     -> assert False
| [k::_] -> do
  {
    if is_cycle k.right_spec then
      invalid_arg "Bezier.add_in_dir: left side already specified!"
    else
      [ { left_spec     = Angle (slow_reduce_angle angle);
          right_spec    = Cycle;
          left_tension  = num_one;
          right_tension = num_one;
          point_x       = num_zero;
          point_y       = num_zero }
        :: spec]
  }
];

value add_in_curl spec curl = match spec with
[ []      -> assert False
| [k::_] -> do
  {
    if is_cycle k.right_spec then
      invalid_arg "Bezier.add_in_curl: left side already specified!"
    else
      [ { left_spec     = Curl curl;
          right_spec    = Cycle;
          left_tension  = num_one;
          right_tension = num_one;
          point_x       = num_zero;
          point_y       = num_zero }
        :: spec]
  }
];

value add_in_tension spec tension = match spec with
[ []      -> assert False
| [k::ks] -> do
  {
    if is_cycle k.right_spec then
      [ { (k) with left_tension = tension }
        :: ks]
    else
      [ { left_spec     = Open;
          right_spec    = Cycle;
          left_tension  = tension;
          right_tension = num_one;
          point_x       = num_zero;
          point_y       = num_zero }
        :: spec]
  }
];

value add_out_dir spec angle = match spec with
[ []      -> assert False
| [k::ks] -> do
  {
    if is_open k.right_spec then
      [{ (k) with right_spec = Angle (slow_reduce_angle angle) } :: ks]
    else
      invalid_arg "Bezier.add_out_dir: right side already specified!"
  }
];

value add_out_curl spec curl = match spec with
[ []      -> assert False
| [k::ks] -> do
  {
    if is_open k.right_spec then
      [{ (k) with right_spec = Curl curl } :: ks]
    else
      invalid_arg "Bezier.add_out_curl: right side already specified!"
  }
];

value add_out_tension spec tension = match spec with
[ []      -> assert False
| [k::ks] -> [ { (k) with right_tension = tension }
               :: ks]
];

value add_control_points spec x1 y1 x2 y2 = match spec with
[ []      -> assert False
| [k::ks] -> do
  {
    if is_open k.right_spec then
      [ { left_spec     = Explicit x2 y2;
          right_spec    = Cycle;
          left_tension  = num_one;
          right_tension = num_one;
          point_x       = num_zero;
          point_y       = num_zero };
        { (k) with right_spec = Explicit x1 y1 }
        :: ks ]
    else if is_cycle k.right_spec then
      invalid_arg "Bezier.add_control_points: point missing!"
    else
      invalid_arg "Bezier.add_control_points: right side already specified!"
  }
];


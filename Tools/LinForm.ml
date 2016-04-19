
open XNum;

type compare_result = [ Eq | Lt | Gt ];

type compare 'a = 'a -> 'a -> compare_result;

type lin_form 'a =
{
  const   : num;
  terms   : list (num * 'a);
  compare : compare 'a
};

value of_num compare n =
{
  const   = n;
  terms   = [];
  compare = compare
};

value lin_zero compare = of_num compare num_zero;

value of_scaled_unknown compare a x = do
{
  if a =/ num_zero then
    lin_zero compare
  else
  {
    const   = num_zero;
    terms   = [(a, x)];
    compare = compare
  }
};

value of_unknown compare x = of_scaled_unknown compare num_one x;

value of_terms compare xs =
{
  const   = num_zero;
  terms   = List.filter (fun (a,_) -> a <>/ num_zero) xs;
  compare = compare
};

value is_constant lin = match lin.terms with
[ [] -> True
| _  -> False
];

value coefficient lin x = do
{
  iter lin.terms

  where rec iter terms = match terms with
  [ []             -> num_zero
  | [(a, y) :: ys] -> match lin.compare x y with
    [ Lt -> num_zero
    | Eq -> a
    | Gt -> iter ys
    ]
  ]
};

value remove_first_term lin = match lin.terms with
[ []        -> invalid_arg "LinForm.remove_first_term"
| [_ :: xs] -> { (lin) with terms = xs }
];

value add l0 l1 = do
{
  {
    const   = l0.const +/ l1.const;
    terms   = add_summands l0.terms l1.terms;
    compare = l0.compare
  }

  where rec add_summands xs ys = match (xs, ys) with
  [ ([], _) -> ys
  | (_, []) -> xs
  | ([(c, x) :: xx], [(d, y) :: yy]) -> match l0.compare x y with
      [ Lt -> [(c, x) :: add_summands xx ys]
      | Gt -> [(d, y) :: add_summands xs yy]
      | Eq -> do
        {
          let e = c +/ d;

          if e =/ num_zero then
            add_summands xx yy
          else
            [(e, x) :: add_summands xx yy]
        }
      ]
  ]
};

value add_const  lin c   = { (lin) with const = lin.const +/ c };
value add_unknown lin a x = add lin (of_scaled_unknown lin.compare a x);

value scale c lin = do
{
  if c =/ num_zero then
    lin_zero lin.compare
  else
    {
      const   = c */ lin.const;
      terms   = List.map (fun (b, x) -> (c */ b, x)) lin.terms;
      compare = lin.compare
    }
};

value lin_comb a l0 b l1 = do
{
  if a =/ num_zero then
    scale b l1
  else if b =/ num_zero then
    scale a l0
  else do
  {
    {
      const   = a */ l0.const +/ b */ l1.const;
      terms   = add_summands l0.terms l1.terms;
      compare = l0.compare
    }

    where rec add_summands xs ys = match (xs, ys) with
    [ ([], _) -> ys
    | (_, []) -> xs
    | ([(c, x) :: xx], [(d, y) :: yy]) -> match l0.compare x y with
        [ Lt -> [(a */ c, x) :: add_summands xx ys]
        | Gt -> [(b */ d, y) :: add_summands xs yy]
        | Eq -> do
          {
            let e = a */ c +/ b */ d;

            if e =/ num_zero then
              add_summands xx yy
            else
              [(e, x) :: add_summands xx yy]
          }
        ]
    ]
  }
};

value sub l0 l1 = lin_comb num_one l0 num_minus_one l1;

value map comp f lin =
{
  const   = lin.const;
  terms   = List.map (fun (b, x) -> (b, f x)) lin.terms;
  compare = comp
};


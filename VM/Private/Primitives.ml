
open XNum;
open Types;
open Runtime;
open Unicode.Types;
open Unicode.SymbolTable;

module UString = Unicode.UString;  (* we cannot open Unicode because of the name clash with Types *)
module UChar   = Unicode.UChar;
module Format  = Unicode.Format;

value rec uc_list_to_char_list str = match str with
[ []      -> Nil
| [c::cs] -> List (ref (Char c)) (ref (uc_list_to_char_list cs))
];

value uc_string_to_char_list str = do
{
  iter (Array.length str - 1) Nil

  where rec iter i list = do
  {
    if i < 0 then
      list
    else
      iter (i-1) (List (ref (Char str.(i))) (ref list))
  }
};

value ascii_to_char_list str = do
{
  uc_string_to_char_list (UString.uc_string_of_ascii str)
};

value rec evaluate_char_list name x = match !x with
[ Nil      -> []
| List a b -> match !a with
              [ Char c -> [c :: evaluate_char_list name b]
              | _      -> runtime_error (name ^ ": invalid argument")
              ]
| Unbound
| Constraint _ -> runtime_error (name ^ ": argument undefined")
| _            -> runtime_error (name ^ ": invalid argument")
];

(* control *)

value prim_error msg = do
{
  let str = evaluate_char_list "error" msg;

  raise (Runtime_error (Array.of_list str))
};


(* types *)

value prim_is_unbound x = match !x with
[ Unbound
| Constraint _
| LinForm _    -> Bool True
| _            -> Bool False
];

value prim_is_bool x = match !x with
[ Bool _ -> Bool True
| _      -> Bool False
];

value prim_is_number x = match !x with
[ Number _ -> Bool True
| _        -> Bool False
];

value prim_is_char x = match !x with
[ Char _ -> Bool True
| _      -> Bool False
];

value prim_is_symbol x = match !x with
[ Symbol _ -> Bool True
| _        -> Bool False
];

value prim_is_function x = match !x with
[ Primitive1 _
| Primitive2 _
| PrimitiveN _ _
| Function _ _ _
| Chain _
| Application _ _ _
| Dictionary _ -> Bool True
| _            -> Bool False
];

value prim_is_list x = match !x with
[ Nil
| List _ _ -> Bool True
| _        -> Bool False
];

value prim_is_tuple x = match !x with
[ Tuple _ -> Bool True
| _       -> Bool False
];

(* logic *)

value prim_or x y = match (!x, !y) with
[ (Bool a, Bool b) -> Bool (a || b)
| _                -> runtime_error "||: invalid argument"
];

value prim_and x y = match (!x, !y) with
[ (Bool a, Bool b) -> Bool (a && b)
| _                -> runtime_error "&&: invalid argument"
];

value prim_not x = match !x with
[ Bool a -> Bool (not a)
| _      -> runtime_error "not: invalid argument"
];

(* comparisons *)

value rec cmp x y = match (!x, !y) with
[ (Unbound, _)                 -> runtime_error "argument unbound during comparison"
| (_, Unbound)                 -> runtime_error "argument unbound during comparison"
| (Constraint a, Constraint b) -> (a == b)
| (Bool a,       Bool b)       -> (a = b)
| (Char a,       Char b)       -> (a = b)
| (Symbol a,     Symbol b)     -> (a = b)
| (Nil,          Nil)          -> True
| (List a1 a2,   List b1 b2)   -> cmp a1 b1 && cmp a2 b2
| (Number a,     Number b)     -> (a =/ b)
| (Number a,     LinForm lin)  -> do
    {
      Evaluate.evaluate_lin_form y lin;

      match !y with
      [ Number b -> (b =/ a)
      | _        -> False
      ]
    }
| (LinForm lin, Number a) -> do
    {
      Evaluate.evaluate_lin_form x lin;

      match !x with
      [ Number b -> (b =/ a)
      | _        -> False
      ]
    }
| (LinForm a, LinForm b) -> do
    {
      Evaluate.evaluate_lin_form x a;
      Evaluate.evaluate_lin_form y b;

      let l = LinForm.lin_comb num_one a (minus_num num_one) b;
      let z = ref (LinForm l);

      Evaluate.evaluate_lin_form z l;

      match !z with
      [ Number c -> (c =/ num_zero)
      | _        -> False
      ]
    }
| (Tuple a, Tuple b) -> do
    {
      let len = Array.length a;
      if Array.length b <> len then
        False
      else
        iter 0

      where rec iter i = do
      {
        if i >= len then
          True
        else if cmp a.(i) b.(i) then
          iter (i+1)
        else
          False
      }
    }
| (Dictionary a, Dictionary b) -> do
    {
      let l0 = map_to_list a;
      let l1 = map_to_list b;

      iter l0 l1

      where rec iter l0 l1 = match (l0, l1) with
      [ ([], []) -> True
      | ([], _)  -> False
      | (_, [])  -> False
      | ([(k0, v0) :: kv0],
         [(k1, v1) :: kv1]) -> do
         {
           if k0 <> k1 then
             False
           else if cmp v0 v1 then
             iter kv0 kv1
           else
             False
         }
      ]
    }
| (Primitive1 a,      Primitive1 b)      -> (a == b)
| (Primitive2 a,      Primitive2 b)      -> (a == b)
| (PrimitiveN a1 a2,  PrimitiveN b1 b2)  -> (a1 = b1 && a2 == b2)
| (Function a1 a2 a3, Function b1 b2 b3) -> (a1 == b1 && a2 = b2 && a3 == b3)
| (Relation a1 a2,    Relation b1 b2)    -> (a1 = b1 && a2 = b2)
| _                                      -> False
];

value prim_eq x y = do
{
  Bool (cmp x y)
};

value prim_neq x y = do
{
  Bool (not (cmp x y))
};

value rec prim_gt x y = match (!x, !y) with
[ (Number m, Number n) -> Bool (m >/ n)
| (Char a,   Char b)   -> Bool (a > b)
| (Nil, Nil)           -> Bool False
| (Nil, List _ _)      -> Bool False
| (List _ _, Nil)      -> Bool True
| (List a b, List c d) -> do
  {
    match prim_gt a c with
    [ Bool True  -> Bool True
    | _          -> do
      {
        if cmp a b then
          prim_gt b d
        else
          Bool False
      }
    ]
  }
| (Tuple xs, Tuple ys) -> do
  {
    if Array.length xs <> Array.length ys then
      runtime_error ">: invalid argument"
    else
      iter 0

    where rec iter i = do
    {
      if i >= Array.length xs then
        Bool False
      else do
      {
        match prim_gt xs.(i) ys.(i) with
        [ Bool True -> Bool True
        | _         -> do
          {
            if cmp xs.(i) ys.(i) then
              iter (i+1)
            else
              Bool False
          }
        ]
      }
    }
  }
| (LinForm l, _)   -> do
  {
    Evaluate.evaluate_lin_form x l;

    match !x with
    [ Number _ -> prim_gt x y
    | _        -> runtime_error (">: invalid argument")
    ]
  }
| (_, LinForm l) -> do
  {
    Evaluate.evaluate_lin_form y l;
    match !y with
    [ Number _ -> prim_gt x y
    | _        -> runtime_error (">: invalid argument")
    ]
  }
| _ -> runtime_error ">: invalid argument"
];

value rec prim_lt x y = match (!x, !y) with
[ (Number m, Number n) -> Bool (m </ n)
| (Char a,   Char b)   -> Bool (a < b)
| (Nil, Nil)           -> Bool False
| (Nil, List _ _)      -> Bool True
| (List _ _, Nil)      -> Bool False
| (List a b, List c d) -> do
  {
    match prim_lt a c with
    [ Bool True  -> Bool True
    | _          -> do
      {
        if cmp a b then
          prim_lt b d
        else
          Bool False
      }
    ]
  }
| (Tuple xs, Tuple ys) -> do
  {
    if Array.length xs <> Array.length ys then
      runtime_error "<: invalid argument"
    else
      iter 0

    where rec iter i = do
    {
      if i >= Array.length xs then
        Bool False
      else do
      {
        match prim_lt xs.(i) ys.(i) with
        [ Bool True -> Bool True
        | _         -> do
          {
            if cmp xs.(i) ys.(i) then
              iter (i+1)
            else
              Bool False
          }
        ]
      }
    }
  }
| (LinForm l, _)   -> do
  {
    Evaluate.evaluate_lin_form x l;

    match !x with
    [ Number _ -> prim_lt x y
    | _        -> runtime_error ("<: invalid argument")
    ]
  }
| (_, LinForm l) -> do
  {
    Evaluate.evaluate_lin_form y l;

    match !y with
    [ Number _ -> prim_lt x y
    | _        -> runtime_error ("<: invalid argument")
    ]
  }
| _ -> runtime_error "<: invalid argument"
];

value rec prim_ge x y = match (!x, !y) with
[ (Number m, Number n) -> Bool (m >=/ n)
| (Char a,   Char b)   -> Bool (a >= b)
| (Nil, Nil)           -> Bool True
| (Nil, List _ _)      -> Bool False
| (List _ _, Nil)      -> Bool True
| (List a b, List c d) -> do
  {
    match prim_gt a c with
    [ Bool True  -> Bool True
    | _          -> do
      {
        if cmp a b then
          prim_ge b d
        else
          Bool False
      }
    ]
  }
| (Tuple xs, Tuple ys) -> do
  {
    if Array.length xs <> Array.length ys then
      runtime_error ">=: invalid argument"
    else
      iter 0

    where rec iter i = do
    {
      if i >= Array.length xs then
        Bool True
      else do
      {
        match prim_gt xs.(i) ys.(i) with
        [ Bool True -> Bool True
        | _         -> do
          {
            if cmp xs.(i) ys.(i) then
              iter (i+1)
            else
              Bool False
          }
        ]
      }
    }
  }
| (LinForm l, _)   -> do
  {
    Evaluate.evaluate_lin_form x l;

    match !x with
    [ Number _ -> prim_ge x y
    | _        -> runtime_error (">=: invalid argument")
    ]
  }
| (_, LinForm l) -> do
  {
    Evaluate.evaluate_lin_form y l;

    match !y with
    [ Number _ -> prim_ge x y
    | _        -> runtime_error (">=: invalid argument")
    ]
  }
| _ -> runtime_error ">=: invalid argument"
];

value rec prim_le x y = match (!x, !y) with
[ (Number m, Number n) -> Bool (m <=/ n)
| (Char a,   Char b)   -> Bool (a <= b)
| (Nil, Nil)           -> Bool True
| (Nil, List _ _)      -> Bool True
| (List _ _, Nil)      -> Bool False
| (List a b, List c d) -> do
  {
    match prim_lt a c with
    [ Bool True  -> Bool True
    | _          -> do
      {
        if cmp a b then
          prim_le b d
        else
          Bool False
      }
    ]
  }
| (Tuple xs, Tuple ys) -> do
  {
    if Array.length xs <> Array.length ys then
      runtime_error "<=: invalid argument"
    else
      iter 0

    where rec iter i = do
    {
      if i >= Array.length xs then
        Bool True
      else do
      {
        match prim_lt xs.(i) ys.(i) with
        [ Bool True -> Bool True
        | _         -> do
          {
            if cmp xs.(i) ys.(i) then
              iter (i+1)
            else
              Bool False
          }
        ]
      }
    }
  }
| (LinForm l, _)   -> do
  {
    Evaluate.evaluate_lin_form x l;

    match !x with
    [ Number _ -> prim_le x y
    | _        -> runtime_error ("<=: invalid argument")
    ]
  }
| (_, LinForm l) -> do
  {
    Evaluate.evaluate_lin_form y l;

    match !y with
    [ Number _ -> prim_le x y
    | _        -> runtime_error ("<=: invalid argument")
    ]
  }
| _ -> runtime_error "<=: invalid argument"
];

value prim_min x y = do
{
  match prim_le x y with
  [ Bool True -> !x
  | _         -> !y
  ]
};

value prim_max x y = do
{
  match prim_ge x y with
  [ Bool True -> !x
  | _         -> !y
  ]
};

(* general arithmetic *)

value rec unary_number_prim f name x = match !x with
[ Number n    -> do
  {
    try
      Number (f n)
    with
    [ _ -> runtime_error (name ^ ": invalid argument") ]
  }
| LinForm l   -> do
  {
    Evaluate.evaluate_lin_form x l;

    match !x with
    [ Number n -> do
      {
        try
          Number (f n)
        with
        [ _ -> runtime_error (name ^ ": invalid argument") ]
      }
    | _ -> runtime_error (name ^ ": invalid argument")
    ]
  }
| _ -> runtime_error (name ^ ": invalid argument")
];

value rec binary_number_prim f name x y = match (!x, !y) with
[ (Number m, Number n) -> do
  {
    try
      Number (f m n)
    with
    [ _ -> runtime_error (name ^ ": invalid argument") ]
  }
| (LinForm l, _) -> do
  {
    Evaluate.evaluate_lin_form x l;

    match !x with
    [ Number _ -> binary_number_prim f name x y
    | _        -> runtime_error (name ^ ": invalid argument")
    ]
  }
| (_, LinForm l) -> do
  {
    Evaluate.evaluate_lin_form y l;

    match !y with
    [ Number _ -> binary_number_prim f name x y
    | _        -> runtime_error (name ^ ": invalid argument")
    ]
  }
| _ -> runtime_error (name ^ ": invalid argument")
];

value prim_quot = binary_number_prim quo_num "quot";
value prim_mod  = binary_number_prim mod_num "mod";
value prim_pow  = binary_number_prim
                    (fun x y -> if is_integer_num y then
                                  power_num x y
                                else
                                  num_of_float (float_of_num x ** float_of_num y))
                    "^";

value rec prim_negate x = match !x with
[ Number n  -> Number (minus_num n)
| LinForm l -> LinForm (LinForm.scale num_minus_one l)
| Unbound   -> LinForm (LinForm.of_scaled_unknown compare_unknowns num_minus_one x)
| Tuple xs  -> do
  {
    let len = Array.length xs;

    Tuple
      (Array.init len
        (fun i -> ref (prim_negate xs.(i))))
  }
| _ -> runtime_error "~: invalid argument"
];

(* integer arithmetic *)

value prim_round    = unary_number_prim round_num   "round";
value prim_truncate = unary_number_prim integer_num "trunacate";
value prim_ceiling  = unary_number_prim ceiling_num "ceiling";
value prim_floor    = unary_number_prim floor_num   "floor";

value prim_land = binary_number_prim land_num "land";
value prim_lor  = binary_number_prim lor_num  "lor";
value prim_lxor = binary_number_prim lxor_num "lxor";
value prim_lneg = unary_number_prim  lneg_num "lneg";

value num_two = num_of_int 2;

value prim_lsr = binary_number_prim (fun m n -> m // power_num num_two n) "lsr";
value prim_lsl = binary_number_prim (fun m n -> m */ power_num num_two n) "lsl";

(* "real" arithmetic *)

value pi      = 4.0 *. atan 1.0;
value pi_inv  = 1.0 /. pi;
value num_180 = num_of_int 180;

value float_wrapper f x = num_of_float (f (float_of_num x));

value sind x    = num_of_float (sin (pi *. float_of_num (x // num_180)));
value cosd x    = num_of_float (cos (pi *. float_of_num (x // num_180)));
value tand x    = num_of_float (tan (pi *. float_of_num (x // num_180)));
value arcsind x = num_of_float (pi_inv *. asin (float_of_num x)) */ num_180;
value arccosd x = num_of_float (pi_inv *. acos (float_of_num x)) */ num_180;
value arctand x = num_of_float (pi_inv *. atan (float_of_num x)) */ num_180;

value arcsinh x = log (x +. sqrt(x *. x +. 1.0));
value arccosh x = log (x +. sqrt(x *. x -. 1.0));
value arctanh x = 0.5 *. (log (1.0 +. x) -. log (1.0 -. x));

value prim_sqrt    = unary_number_prim (float_wrapper sqrt)    "sqrt";
value prim_exp     = unary_number_prim (float_wrapper exp)     "exp";
value prim_log     = unary_number_prim (float_wrapper log)     "log";
value prim_sin     = unary_number_prim (float_wrapper sin)     "sin";
value prim_cos     = unary_number_prim (float_wrapper cos)     "cos";
value prim_tan     = unary_number_prim (float_wrapper tan)     "tan";
value prim_arcsin  = unary_number_prim (float_wrapper asin)    "arcsin";
value prim_arccos  = unary_number_prim (float_wrapper acos)    "arccos";
value prim_arctan  = unary_number_prim (float_wrapper atan)    "arctan";
value prim_sind    = unary_number_prim sind                    "sind";
value prim_cosd    = unary_number_prim cosd                    "cosd";
value prim_tand    = unary_number_prim tand                    "tand";
value prim_arcsind = unary_number_prim arcsind                 "arcsind";
value prim_arccosd = unary_number_prim arcsind                 "arccosd";
value prim_arctand = unary_number_prim arcsind                 "arctand";
value prim_sinh    = unary_number_prim (float_wrapper sinh)    "sinh";
value prim_cosh    = unary_number_prim (float_wrapper cosh)    "cosh";
value prim_tanh    = unary_number_prim (float_wrapper tanh)    "tanh";
value prim_arcsinh = unary_number_prim (float_wrapper arcsinh) "arcsinh";
value prim_arccosh = unary_number_prim (float_wrapper arccosh) "arccosh";
value prim_arctanh = unary_number_prim (float_wrapper arctanh) "arctanh";

value rec prim_abs x = match !x with
[ Number n    -> Number (abs_num n)
| LinForm l   -> do
  {
    Evaluate.evaluate_lin_form x l;

    match !x with
    [ Number n -> Number (abs_num n)
    | _        -> runtime_error "abs: invalid argument"
    ]
  }
| Tuple xs -> do
  {
    let len = Array.length xs;
    let ss  = Array.init len create_unbound;

    for i = 1 to len do
    {
      Evaluate.mul_unknowns ss.(len - i) xs.(len - i) xs.(len - i)
    };

    Number
      (float_wrapper sqrt
        (Array.fold_left
           (fun sum s -> match !s with
             [ Number c -> sum +/ c
             | _        -> runtime_error "abs: invalid argument"
             ])
           num_zero
           ss))
  }
| _ -> runtime_error "abs: invalid argument"
];

(* lists *)

value prim_length x = do
{
  Number (num_of_int (count_len 0 x))

  where rec count_len len x = match !x with
  [ Nil          -> len
  | List _ a     -> count_len (len + 1) a
  | Tuple xs     -> Array.length xs
  | Dictionary d -> SymbolMap.fold (fun _ _ n -> n + 1) d 0
  | Unbound
  | Constraint _ -> runtime_error "length: argument undefined"
  | _            -> 1
  ]
};

(* FIX: tuples and lists *)

value rec prim_to_string x = match !x with
[ Number n -> do
  {
    if n </ num_zero then
      List (ref (Char 126)) (ref (ascii_to_char_list (string_of_num (minus_num n))))
    else do
    {
      let str = string_of_num n;

      (* If |n| is an integer we remove the suffix "/1". *)

      if str.[String.length str - 2] = '/' &&
         str.[String.length str - 1] = '1' then
        ascii_to_char_list (String.sub str 0 (String.length str - 2))
      else
        ascii_to_char_list str
    }
  }
| Bool b    -> if b then
                 ascii_to_char_list "True"
               else
                 ascii_to_char_list "False"
| Char _    -> List x (ref Nil)
| Symbol s  -> uc_string_to_char_list (symbol_to_string s)
| Nil       -> ascii_to_char_list "[]"
| LinForm l -> do
  {
    Evaluate.evaluate_lin_form x l;

    match !x with
    [ Number _ -> prim_to_string x
    | _        -> runtime_error "to_string: argument undefined"
    ]
  }
| Unbound
| Constraint _ -> runtime_error "to_string: argument undefined"
| _            -> runtime_error "to_string: invalid argument"
];

(* format_string *)

type number_format =
[ NF_Decimal
| NF_Hexadecimal
| NF_HEXADECIMAL
| NF_Roman
| NF_ROMAN
| NF_Alpha
| NF_ALPHA
];

type format_spec =
[ FS_Literal of uc_list
| FS_String of bool and int
| FS_List of format_spec and uc_list
| FS_Number of bool and bool and int and int and number_format
];

value parse_token fmt = do
{
  let rec parse_type align sign len len2 fmt = match fmt with
  [ [115 :: xs] -> do (* s *)
    {
      if sign || len2 > 0 then
        runtime_error "format_string: invalid format"
      else
        (FS_String align len, xs)
    }
  | [100 :: xs] -> (* d *)
      (FS_Number align sign len len2 NF_Decimal,     xs)
  | [120 :: xs] -> (* x *)
      (FS_Number align sign len len2 NF_Hexadecimal, xs)
  | [88 :: xs]  -> (* X *)
      (FS_Number align sign len len2 NF_HEXADECIMAL, xs)
  | [114 :: xs] -> (* r *)
      (FS_Number align sign len len2 NF_Roman,       xs)
  | [82 :: xs]  -> (* R *)
      (FS_Number align sign len len2 NF_ROMAN,       xs)
  | [97 :: xs] -> (* a *)
      (FS_Number align sign len len2 NF_Alpha,       xs)
  | [65 :: xs]  -> (* A *)
      (FS_Number align sign len len2 NF_ALPHA,       xs)
  | _ -> runtime_error "format_string: invalid format"
  ]
  and parse_len2 align sign len len2 fmt = match fmt with
  [ [c :: cs] -> do
    {
      if c >= 48 && c < 58 then
        parse_len2 align sign len (10 * len2 + c - 48) cs
      else
        parse_type align sign len len2 fmt
    }
  | _ -> runtime_error "format_string: invalid format"
  ]
  and parse_frac align sign len fmt = match fmt with
  [ [46 :: xs] -> parse_len2 align sign len 0 xs    (* . *)
  | _          -> parse_type align sign len 0 fmt
  ]
  and parse_len align sign len fmt = match fmt with
  [ [c :: cs] -> do
    {
      if c >= 48 && c < 58 then
        parse_len align sign (10 * len + c - 48) cs
      else
        parse_frac align sign len fmt
    }
  | _ -> runtime_error "format_string: invalid format"
  ]
  and parse_sign align fmt = match fmt with
  [ [43 :: xs] -> parse_len align True  0 xs   (* + *)
  | _          -> parse_len align False 0 fmt
  ]
  and parse_align fmt = match fmt with
  [ [37 :: xs] -> (FS_Literal [37], xs)  (* % *)
  | [91 :: xs] -> parse_list_spec xs     (* [ *)
  | [45 :: xs] -> parse_sign True  xs    (* - *)
  | _          -> parse_sign False fmt
  ]
  and parse_list_spec fmt = do
  {
    let (spec, f) = parse_align fmt;

    iter [] f

    where rec iter sep f = match f with
    [ []         -> runtime_error "format_string: invalid format"
    | [93 :: cs] -> (FS_List spec (List.rev sep), cs) (* ] *)
    | [c :: cs]  -> iter [c :: sep] cs
    ]
  };

  parse_align fmt
};

value parse_format_string fmt = do
{
  iter [] 0 fmt

  where rec iter res n fmt = match fmt with
  [ []         -> (List.rev res, n)
  | [37 :: xs] -> do            (* % *)
    {
      let (t, ys) = parse_token xs;

      match t with
      [ FS_Literal _ -> iter [t :: res] n     ys
      | _            -> iter [t :: res] (n+1) ys
      ]
    }
  | _ -> do
    {
      iter2 [] fmt

      where rec iter2 lit fmt = match fmt with
      [ []
      | [37 :: _] -> iter [FS_Literal (List.rev lit) :: res] n fmt
      | [c :: cs] -> iter2 [c :: lit] cs
      ]
    }
  ]
};

value number_to_string sign len2 nf x = do
{
  let to_str nf x = match nf with
  [ NF_Decimal     -> Format.num_to_arabic  10 x
  | NF_Hexadecimal -> Format.num_to_arabic  10 x
  | NF_HEXADECIMAL -> Format.num_to_ARABIC  10 x
  | NF_Roman       -> Format.num_to_roman      x
  | NF_ROMAN       -> Format.num_to_ROMAN      x
  | NF_Alpha       -> Format.num_to_alphabetic x
  | NF_ALPHA       -> Format.num_to_ALPHABETIC x
  ];

  let pos_num_to_string len2 nf x = do
  {
    let y  = floor_num x;
    let z  = x -/ y;
    let s1 = to_str nf y;

    if len2 <= 0 then do
    {
      if z <=/ num_of_ints 1 10000000 then
        s1
      else
        s1 @ add_fractional 7 z
    }
    else
      s1 @ add_fractional len2 z
  }
  where add_fractional len x = do
  {
    let s = to_str nf (floor_num (x */ power_num num_ten (num_of_int len)));
    let l = List.length s;

    [46 :: XList.repeat (len - l) 48 @ s]
  };

  if x </ num_zero then
    [45 :: pos_num_to_string len2 nf (minus_num x)]
  else if sign then
    [43 :: pos_num_to_string len2 nf x]
  else
    pos_num_to_string len2 nf x
};

value rec output_format_string fmt args = do
{
  let rec add_string tail str = match str with
  [ []      -> tail
  | [c::cs] -> do
    {
      let x = ref Unbound;

      !tail := List (ref (Char c)) x;

      add_string x cs
    }
  ];
  let rec add_aligned_string tail align pad len str = do
  {
    let rec add_padding tail pad n = do
    {
      if n <= 0 then
        tail
      else do
      {
        let x = ref Unbound;

        !tail := List (ref (Char pad)) x;

        add_padding x pad (n-1)
      }
    };

    if len <= 0 then
      add_string tail str
    else do
    {
      let l = List.length str;

      if align then
        add_padding
          (add_string tail str)
          pad
          (len - l)
      else
        add_string
          (add_padding tail pad (len - l))
          str
    }
  };
  let rec format_argument tail fmt arg = match fmt with
  [ FS_Literal str -> do
    {
      add_string tail str
    }
  | FS_String align len -> do
    {
      match !arg with
      [ Char c   -> add_aligned_string tail align 32 len [c]
      | Symbol s -> add_aligned_string tail align 32 len (Array.to_list (symbol_to_string s))
      | Nil      -> add_aligned_string tail align 32 len []
      | List _ _ -> do
        {
          let str = evaluate_char_list "format_string" arg;

          add_aligned_string tail align 32 len str
        }
      | _ -> runtime_error "format_string: invalid argument for %s"
      ]
    }
  | FS_List spec sep -> do
    {
      let lst = Evaluate.evaluate_list "format_string" arg;

      match lst with
      [ []      -> tail
      | [x::xs] -> do
        {
          let r = format_argument tail spec x;

          iter r xs

          where rec iter tail lst = match lst with
          [ []      -> tail
          | [x::xs] -> do
            {
              iter
                (format_argument (add_string tail sep) spec x)
                xs
            }
          ]
        }
      ]
    }
  | FS_Number align sign len len2 nf -> do
    {
      let n = Evaluate.evaluate_num "format_string" arg;

      add_aligned_string tail align 32 len
        (number_to_string sign len2 nf n)
    }
  ];

  let result = ref Unbound;

  iter result fmt args

  where rec iter tail fmt args = match fmt with
  [ [] -> do
    {
      !tail := Nil;
      !result
    }
  | [FS_Literal str :: fs] -> iter (add_string tail str) fs args
  | [f :: fs] -> match args with
                 [ [b::bs] -> iter (format_argument tail f b) fs bs
                 | _       -> assert False
                 ]
  ]
};

value prim_format_string fmt_string = do
{
  let fmt = evaluate_char_list "format_string" fmt_string;

  let (f, n) = parse_format_string fmt;

  if n = 0 then
    !fmt_string  (* FIX: map %% to % in fmt_string *)
  else
    PrimitiveN n (output_format_string f)
};

value prim_sort_strings cmp key_val = do
{
  (* parse <key-val> *)
  let kv      = Evaluate.evaluate_list "sort_strings" key_val;

  (* parse <cmp> *)
  let classes =
    List.map
      (evaluate_char_list "sort_strings")
      (Evaluate.evaluate_list "sort_strings" cmp);
  let class_map =
    snd
      (List.fold_left
        (fun (n,map) cls ->
          (n+1,
           List.fold_left
             (fun m c -> DynamicCharMap.add c n m)
             map
             cls))
        (0, DynamicCharMap.empty)
        classes);
  let default_class = try
    DynamicCharMap.find 46 class_map  (* . *)
  with [ Not_found -> 0 ];

  let char_class c = try
    DynamicCharMap.find c class_map
  with [ Not_found -> default_class ];

  (* sort *)

  iter DynUCTrie.empty kv

  where rec iter trie kv = match kv with
  [ [] -> do
    {
      (* construct result *)
      let result = ref Unbound;

      let tail =
        DynUCTrie.fold
          (fun _ vals res -> do
            {
              let group =
                List.fold_left
                  (fun l v -> List v (ref l))
                  Nil
                  vals;
              let new_res = ref Unbound;
              !res := List (ref group) new_res;
              new_res
            })
          trie
          result;

      !tail := Nil;

      !result
    }
  | [ x :: xs ] -> match !x with
      [ Tuple arr -> do
        {
          if Array.length arr <> 2 then
            runtime_error ("sort_strings: pair expected but got tuple of length " ^ string_of_int (Array.length arr))
          else do
          {
            (* add current element to <trie> *)
            let k  = evaluate_char_list "sort_strings" arr.(0);
            let v  = arr.(1);
            let kc = List.map char_class k;

            let old = try
              DynUCTrie.find_list kc trie
            with [ Not_found -> [] ];

            iter (DynUCTrie.add_list kc [v :: old] trie) xs
          }
        }
      | _ -> runtime_error ("sort_strings: pair expected but got " ^ type_name !x)
      ]
  ]
};

value prim_to_tuple x = do
{
  Tuple (Array.of_list (Evaluate.evaluate_list "to_tuple" x))
};

value rec prim_to_list x = match !x with
[ Tuple xs     -> Array.fold_right
                    (fun a b -> List a (ref b))
                    xs
                    Nil
| Unbound
| Constraint _ -> runtime_error "to_list: argument undefined"
| _            -> runtime_error "to_list: invalid argument"
];

value rec unary_vec2_prim f name x = match !x with
[ Tuple [| a; b |] -> match (!a, !b) with
    [ (Number n, Number m) -> do
      {
        try
          f n m
        with
        [ _ -> runtime_error (name ^ ": invalid argument") ]
      }
    | _ -> runtime_error (name ^ ": invalid argument")
    ]
| LinForm l -> do
  {
    Evaluate.evaluate_lin_form x l;

    match !x with
    [ Tuple _ -> unary_vec2_prim f name x
    | _       -> runtime_error (name ^ ": invalid argument")
    ]
  }
| _ -> runtime_error (name ^ ": invalid argument")
];

value rec prim_dir x = do
{
  Tuple
    [| ref (prim_cosd x);
       ref (prim_sind x) |]
};

value prim_angle = unary_vec2_prim
  (fun x y -> match sign_num x with
    [  1 -> Number (arctand (y // x))
    |  0 -> if y >/ num_zero then
              Number (90 /: 1)
            else if y =/ num_zero then
              runtime_error "angle: invalid argument"
            else
              Number (270 /: 1)
    | -1 -> Number (180 /: 1 -/ arctand (minus_num y // x))
    | _  -> assert False
    ])
  "angle";

value rec prim_rotate a vec = match !a with
[ Number n    -> unary_vec2_prim
                   (fun x y -> do
                     {
                       let x2 = cosd n */ x -/ sind n */ y;
                       let y2 = sind n */ x +/ cosd n */ y;

                       Tuple [| ref (Number x2); ref (Number y2) |]
                     })
                   "rotate"
                   vec
| LinForm l   -> do
  {
    Evaluate.evaluate_lin_form a l;
    match !a with
    [ Number _ -> prim_rotate a vec
    | _        -> runtime_error "rotate: invalid argument"
    ]
  }
| _ -> runtime_error "rotate: invalid argument"
];

value rec prim_add_to_dict args = match args with
[ [sym; val; dict] -> match !sym with
  [ Symbol s -> match !dict with
    [ Dictionary d -> Dictionary (SymbolMap.add s val d)
    | Unbound
    | Constraint _ -> runtime_error "add_to_dict: argument undefined"
    | _            -> runtime_error ("add_to_dict: invalid argument (got " ^ type_name !dict ^ " instead of dictionary)")
    ]
  | Unbound
  | Constraint _ -> runtime_error "add_to_dict: argument undefined"
  | _            -> runtime_error ("add_to_dict: invalid argument (got " ^ type_name !dict ^ " instead of symbol)")
  ]
| _ -> assert False
];

(* characters *)

value rec unary_char_prim f name x = match !x with
[ Char n -> do
  {
    try f n with
    [ _ -> runtime_error (name ^ ": invalid argument") ]
  }
| _ -> runtime_error (name ^ ": invalid argument")
];

value prim_is_letter    = unary_char_prim (fun c -> Bool (UChar.is_letter c))    "is_letter";
value prim_is_mark      = unary_char_prim (fun c -> Bool (UChar.is_mark c))      "is_mark";
value prim_is_number    = unary_char_prim (fun c -> Bool (UChar.is_number c))    "is_number";
value prim_is_punct     = unary_char_prim (fun c -> Bool (UChar.is_punct c))     "is_punct";
value prim_is_symbol    = unary_char_prim (fun c -> Bool (UChar.is_symbol c))    "is_symbol";
value prim_is_separator = unary_char_prim (fun c -> Bool (UChar.is_separator c)) "is_separator";
value prim_is_control   = unary_char_prim (fun c -> Bool (UChar.is_control c))   "is_control";
value prim_is_space     = unary_char_prim (fun c -> Bool (UChar.is_space c))     "is_space";
value prim_to_upper     = unary_char_prim (fun c -> Char (UChar.to_upper c))     "to_upper";
value prim_to_lower     = unary_char_prim (fun c -> Char (UChar.to_lower c))     "to_lower";
value prim_to_title     = unary_char_prim (fun c -> Char (UChar.to_title c))     "to_title";
value prim_char_name    = unary_char_prim (fun c -> ascii_to_char_list (UChar.name c))  "char_name";

value symbol_Lu = Symbol (string_to_symbol (UString.uc_string_of_ascii "Lu"));
value symbol_Ll = Symbol (string_to_symbol (UString.uc_string_of_ascii "Ll"));
value symbol_Lt = Symbol (string_to_symbol (UString.uc_string_of_ascii "Lt"));
value symbol_Lm = Symbol (string_to_symbol (UString.uc_string_of_ascii "Lm"));
value symbol_Lo = Symbol (string_to_symbol (UString.uc_string_of_ascii "Lo"));
value symbol_Mn = Symbol (string_to_symbol (UString.uc_string_of_ascii "Mn"));
value symbol_Mc = Symbol (string_to_symbol (UString.uc_string_of_ascii "Mc"));
value symbol_Me = Symbol (string_to_symbol (UString.uc_string_of_ascii "Me"));
value symbol_Nd = Symbol (string_to_symbol (UString.uc_string_of_ascii "Nd"));
value symbol_Nl = Symbol (string_to_symbol (UString.uc_string_of_ascii "Nl"));
value symbol_No = Symbol (string_to_symbol (UString.uc_string_of_ascii "No"));
value symbol_Pc = Symbol (string_to_symbol (UString.uc_string_of_ascii "Pc"));
value symbol_Pd = Symbol (string_to_symbol (UString.uc_string_of_ascii "Pd"));
value symbol_Ps = Symbol (string_to_symbol (UString.uc_string_of_ascii "Ps"));
value symbol_Pe = Symbol (string_to_symbol (UString.uc_string_of_ascii "Pe"));
value symbol_Pi = Symbol (string_to_symbol (UString.uc_string_of_ascii "Pi"));
value symbol_Pf = Symbol (string_to_symbol (UString.uc_string_of_ascii "Pf"));
value symbol_Po = Symbol (string_to_symbol (UString.uc_string_of_ascii "Po"));
value symbol_Sm = Symbol (string_to_symbol (UString.uc_string_of_ascii "Sm"));
value symbol_Sc = Symbol (string_to_symbol (UString.uc_string_of_ascii "Sc"));
value symbol_Sk = Symbol (string_to_symbol (UString.uc_string_of_ascii "Sk"));
value symbol_So = Symbol (string_to_symbol (UString.uc_string_of_ascii "So"));
value symbol_Zs = Symbol (string_to_symbol (UString.uc_string_of_ascii "Zs"));
value symbol_Zl = Symbol (string_to_symbol (UString.uc_string_of_ascii "Zl"));
value symbol_Zp = Symbol (string_to_symbol (UString.uc_string_of_ascii "Zp"));
value symbol_Cc = Symbol (string_to_symbol (UString.uc_string_of_ascii "Cc"));
value symbol_Cf = Symbol (string_to_symbol (UString.uc_string_of_ascii "Cf"));
value symbol_Cs = Symbol (string_to_symbol (UString.uc_string_of_ascii "Cs"));
value symbol_Co = Symbol (string_to_symbol (UString.uc_string_of_ascii "Co"));
value symbol_Cn = Symbol (string_to_symbol (UString.uc_string_of_ascii "Cn"));

value prim_char_category =
  unary_char_prim
    (fun c -> match UChar.category c with
     [ UChar.Lu -> symbol_Lu
     | UChar.Ll -> symbol_Ll
     | UChar.Lt -> symbol_Lt
     | UChar.Lm -> symbol_Lm
     | UChar.Lo -> symbol_Lo
     | UChar.Mn -> symbol_Mn
     | UChar.Mc -> symbol_Mc
     | UChar.Me -> symbol_Me
     | UChar.Nd -> symbol_Nd
     | UChar.Nl -> symbol_Nl
     | UChar.No -> symbol_No
     | UChar.Pc -> symbol_Pc
     | UChar.Pd -> symbol_Pd
     | UChar.Ps -> symbol_Ps
     | UChar.Pe -> symbol_Pe
     | UChar.Pi -> symbol_Pi
     | UChar.Pf -> symbol_Pf
     | UChar.Po -> symbol_Po
     | UChar.Sm -> symbol_Sm
     | UChar.Sc -> symbol_Sc
     | UChar.Sk -> symbol_Sk
     | UChar.So -> symbol_So
     | UChar.Zs -> symbol_Zs
     | UChar.Zl -> symbol_Zl
     | UChar.Zp -> symbol_Zp
     | UChar.Cc -> symbol_Cc
     | UChar.Cf -> symbol_Cf
     | UChar.Cs -> symbol_Cs
     | UChar.Co -> symbol_Co
     | UChar.Cn -> symbol_Cn
     ])
    "char_category";

value prim_to_symbol x = do
{
  let str = evaluate_char_list "to_symbol" x;

  Symbol (string_to_symbol (Array.of_list str))
};

value prim_generate_symbol _ = do
{
  Symbol (alloc_symbol ())
};

(* serialisation *)

value prim_serialise file val = do
{
  let str = evaluate_char_list "serialise" file;

  try do
  {
    let os = IO.make_out_stream (UString.bytes_to_string str);
    Serialise.serialise_unknown os val;
    IO.free os;
    Bool True
  }
  with [ _ -> Bool False ]
};

value prim_unserialise file = do
{
  let str = evaluate_char_list "unserialise" file;

  try do
  {
    let is  = IO.make_rand_in_stream (UString.bytes_to_string str);
    let res = Serialise.unserialise_unknown is;
    IO.free is;
    res
  }
  with
  [ Sys_error _ -> do
    {
      Logging.log_warn ("",0,0) ("Cannot open file " ^ (UString.bytes_to_string str) ^ "!");
      Unbound
    }
  ]
};

(* initialisation *)

value bind_primitive scope name v = do
{
  Scope.add_global scope (string_to_symbol (UString.uc_string_of_ascii name)) v
};

value bind_bin_op_l scope name pri v = do
{
  let sym = string_to_symbol (UString.uc_string_of_ascii name);

  Scope.add_bin_op scope pri Lexer.Left sym;
  Scope.add_global scope sym v
};

value bind_bin_op_n scope name pri v = do
{
  let sym = string_to_symbol (UString.uc_string_of_ascii name);

  Scope.add_bin_op scope pri Lexer.NonA sym;
  Scope.add_global scope sym v
};

value bind_bin_op_r scope name pri v = do
{
  let sym = string_to_symbol (UString.uc_string_of_ascii name);

  Scope.add_bin_op scope pri Lexer.Right sym;
  Scope.add_global scope sym v
};

value bind_pre_op scope name v = do
{
  let sym = string_to_symbol (UString.uc_string_of_ascii name);

  Scope.add_pre_op scope sym;
  Scope.add_global scope sym v
};

value bind_post_op scope name v = do
{
  let sym = string_to_symbol (UString.uc_string_of_ascii name);

  Scope.add_post_op scope sym;
  Scope.add_global  scope sym v
};

value initial_scope () = do
{
  let scope = Scope.create ();

  let add = bind_primitive scope;

  let add1 name f = bind_primitive scope name (Primitive1 f);
  let add2 name f = bind_primitive scope name (Primitive2 f);

  (* control *)

  add1 "error"    prim_error;

  (* types *)

  add1 "is_unbound"  prim_is_unbound;
  add1 "is_bool"     prim_is_bool;
  add1 "is_number"   prim_is_number;
  add1 "is_char"     prim_is_char;
  add1 "is_symbol"   prim_is_symbol;
  add1 "is_function" prim_is_function;
  add1 "is_list"     prim_is_list;
  add1 "is_tuple"    prim_is_tuple;

  (* logical operators *)

  add2 "||"       prim_or;
  add2 "&&"       prim_and;
  add1 "not"      prim_not;

  (* comparisons *)

  add2 "=="       prim_eq;
  add2 "<>"       prim_neq;
  add2 ">"        prim_gt;
  add2 "<"        prim_lt;
  add2 ">="       prim_ge;
  add2 "<="       prim_le;
  add2 "min"      prim_min;
  add2 "max"      prim_max;

  (* general arithmetic *)

  add2 "+"        Evaluate.prim_add;
  add2 "-"        Evaluate.prim_sub;
  add2 "*"        Evaluate.prim_mul;
  add2 "/"        Evaluate.prim_div;
  add2 "^"        prim_pow;
  add2 "quot"     prim_quot;
  add2 "mod"      prim_mod;
  add1 "~"        prim_negate;
  add1 "abs"      prim_abs;

  (* integer arithmetic *)

  add1 "round"    prim_round;
  add1 "truncate" prim_truncate;
  add1 "ceiling"  prim_ceiling;
  add1 "floor"    prim_floor;
  add2 "land"     prim_land;
  add2 "lor"      prim_lor;
  add2 "lxor"     prim_lxor;
  add2 "lneg"     prim_lxor;
  add2 "lsr"      prim_lsr;
  add2 "lsl"      prim_lsl;

  (* "real" arithmetic *)

  add  "pi"  (Number (num_of_float pi));

  add1 "sqrt"     prim_sqrt;
  add1 "exp"      prim_exp;
  add1 "log"      prim_log;
  add1 "sin"      prim_sin;
  add1 "cos"      prim_cos;
  add1 "tan"      prim_tan;
  add1 "arcsin"   prim_arcsin;
  add1 "arccos"   prim_arccos;
  add1 "arctan"   prim_arctan;
  add1 "sind"     prim_sind;
  add1 "cosd"     prim_cosd;
  add1 "tand"     prim_tand;
  add1 "arcsind"  prim_arcsind;
  add1 "arccosd"  prim_arccosd;
  add1 "arctand"  prim_arctand;
  add1 "sinh"     prim_sinh;
  add1 "cosh"     prim_cosh;
  add1 "tanh"     prim_tanh;
  add1 "arcsinh"  prim_arcsinh;
  add1 "arccosh"  prim_arccosh;
  add1 "arctanh"  prim_arctanh;

  (* lists, tuples, and dictionaries *)

  add1 "length"        prim_length;
  add1 "to_string"     prim_to_string;
  add1 "format_string" prim_format_string;
  add2 "sort_strings"  prim_sort_strings;
  add1 "to_list"       prim_to_list;
  add1 "to_tuple"      prim_to_tuple;
  add1 "dir"           prim_dir;
  add1 "angle"         prim_angle;
  add2 "rotate"        prim_rotate;
  add  "add_to_dict"   (PrimitiveN 3 prim_add_to_dict);

  (* characters *)

  add1 "char_is_letter"     prim_is_letter;
  add1 "char_is_mark"       prim_is_mark;
  add1 "char_is_number"     prim_is_number;
  add1 "char_is_punct"      prim_is_punct;
  add1 "char_is_symbol"     prim_is_symbol;
  add1 "char_is_separator"  prim_is_separator;
  add1 "char_is_control"    prim_is_control;
  add1 "char_is_space"      prim_is_space;
  add1 "to_upper"           prim_to_upper;
  add1 "to_lower"           prim_to_lower;
  add1 "to_title"           prim_to_title;
  add1 "char_name"          prim_char_name;
  add1 "char_category"      prim_char_category;

  (* symbols *)

  add1 "to_symbol"       prim_to_symbol;
  add1 "generate_symbol" prim_generate_symbol;

  (* serialisation *)

  add2 "serialise"       prim_serialise;
  add1 "unserialise"     prim_unserialise;

  (* dimensions *)

  let scale x =
    Function [] 1
      [|BConst (Number x);
        BConst (Primitive2 Evaluate.prim_mul);
        BApply 2;
        BReturn|];

  bind_post_op scope "pt" (scale num_one);
  bind_post_op scope "bp" (scale ( 7227 /:  7200));
  bind_post_op scope "cc" (scale (14856 /:  1157));
  bind_post_op scope "cm" (scale ( 7227 /:   254));
  bind_post_op scope "dd" (scale ( 1238 /:  1157));
  bind_post_op scope "in" (scale ( 7227 /:   100));
  bind_post_op scope "mm" (scale ( 7227 /:  2540));
  bind_post_op scope "pc" (scale (   12 /:     1));
  bind_post_op scope "sp" (scale (    1 /: 65536));

  (* paths *)

  add1 "make_path"               Path.make_path;
  add2 "close_path"              Path.close_path;
  add2 "path_add_point"          Path.add_point;
  add2 "path_add_in_dir"         Path.add_in_dir;
  add2 "path_add_in_angle"       Path.add_in_angle;
  add2 "path_add_in_curl"        Path.add_in_curl;
  add2 "path_add_in_tension"     Path.add_in_tension;
  add2 "path_add_out_dir"        Path.add_out_dir;
  add2 "path_add_out_angle"      Path.add_out_angle;
  add2 "path_add_out_curl"       Path.add_out_curl;
  add2 "path_add_out_tension"    Path.add_out_tension;
  add  "path_add_control_points" (PrimitiveN 3 Path.add_control_points);

  scope
};

(*
  char:   ucode_of_char, char_of_ucode
  string: xxx_to_string, string_to_xxx
  list:   head, tail, reverse, append, concat, flatten, map, foldl, foldr, member,
          find, filter, partition, assoc, mem_assoc, remove_assoc, zip, unzip, sort,
          sub_list
  path:   point t of p, tangent t of p, concat, .., --, {xx}..{xx},
          flex [z_1,...,z_n]
*)


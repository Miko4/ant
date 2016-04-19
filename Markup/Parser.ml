
open XNum;
open Runtime;
open Unicode;
open Logging;
open Dim;
open Engine;
open CharCode;

(* simple parsing routines ************************************************************************)

(*
  |match_substring <stream> <str> <i>| tests whether the given string appears at position <i> in <stream>.
*)

value rec match_substring stream str i = match str with
[ []      -> True
| [c::cs] -> if c = UCStream.get_char stream i then
               match_substring stream cs (i + 1)
             else
               False
];

(*
  |newline_to_par <stream>| checks whether the first character of <stream> is a newline and replaces it
  by the string |\par|.
*)

value newline_to_par stream = do
{
  if cat_code (UCStream.next_char stream) = Newline then
      UCStream.insert_string stream (UString.uc_string_of_ascii "\\par")
  else ()
};

(*
  |skip_spaces <stream>| removes all initial spaces from <stream>.
*)

value rec skip_spaces stream = do
{
  while cat_code (UCStream.next_char stream) = Space do
  {
    UCStream.remove stream 1
  }
};

(*
  |skip_blanks <stream>| removes all initial spaces from <stream>. If two newlines are encountered
  the second one is replaced by the string |\par|.
*)

value rec skip_blanks stream = do
{
  match cat_code (UCStream.next_char stream) with
  [ Space -> do
    {
      UCStream.remove stream 1;
      skip_blanks stream
    }
  | Newline -> do
    {
      UCStream.remove stream 1;
      skip_spaces stream;
      newline_to_par stream
    }
  | _ -> ()
  ]
};

(*
  |skip_comment <stream>| removes all characters from <stream> until the first newlines is found.
*)

value rec skip_comment stream = do
{
  if cat_code (UCStream.pop stream) = Newline then do
  {
    skip_spaces stream;
    newline_to_par stream
  }
  else
    skip_comment stream
};

(* parsing tokens and arguments *)

(*
  |is_command_sequence <str>| checks whether <str> consists of one backslash followed by letters.
  |is_token <str>| checks whether <str> consists of exactly one TeX token.
*)

value is_command_sequence str = match str with
[ []      -> False
| [_]     -> False
| [c::cs] -> c = escape_char && List.for_all (fun x -> cat_code x = Letter) cs
];

value is_token str = match str with
[ []      -> False
| [_]     -> True
| [c; _]  -> c = escape_char
| [c::cs] -> c = escape_char && List.for_all (fun x -> cat_code x = Letter) cs
];

(* |read_token_tail <stream>| reads the rest of a \TeX-token after the initial backslash is already read. *)

value read_token_tail stream = do
{
  let rec read_command_sequence () = do
  {
    let c = UCStream.next_char stream;

    if cat_code c = Letter then do
    {
      UCStream.remove stream 1;

      [c :: read_command_sequence ()]
    }
    else do
    {
      skip_blanks stream;

      []
    }
  };

  let char = UCStream.pop stream;

  if cat_code char = Letter then
    [escape_char; char :: read_command_sequence ()]
  else
    [escape_char; char]
};

(* |read_token <stream>| reads a whole \TeX-token. *)

value read_token stream = do
{
  let char = UCStream.pop stream;

  match cat_code char with
  [ EOF    -> []
  | Escape -> read_token_tail stream
  | _      -> [char]
  ]
};

(* |peek_token <stream>| reads a whole \TeX-token without removing it from <stream>. *)

value peek_token stream = do
{
  let rec read_command_sequence i () = do
  {
    let c = UCStream.get_char stream i;

    if cat_code c = Letter then
      [c :: read_command_sequence (i+1) ()]
    else
      []
  };

  let char = UCStream.next_char stream;

  if char = (-1) then
    []
  else if cat_code char = Escape then do
  {
    let c2 = UCStream.get_char stream 1;

    if cat_code c2 = Letter then
      [char; c2 :: read_command_sequence 2 ()]
    else
      [char; c2]
  }
  else
    [char]
};

(* |read_group <prefix> <stream>| reads a group.
   It is assumed that the opening brace was already removed from the stream.
   The result is appended in reversed order to <prefix>.
*)

value read_group prefix stream = do
{
  iter 0 prefix

  where rec iter nest prefix = do
  {
    let c = UCStream.pop stream;

    match cat_code c with
    [ BeginGroup -> iter (nest + 1) [c :: prefix]
    | EndGroup   -> if nest = 0 then
                      prefix
                    else
                      iter (nest - 1) [c :: prefix]
    | _          -> iter nest [c :: prefix]
    ]
  }
};

(* |read_argument| reads the next token or group. *)

value read_argument stream = do
{
  skip_blanks stream;

  if cat_code (UCStream.next_char stream) = BeginGroup then do
  {
    UCStream.pop stream;
    List.rev (read_group [] stream)
  }
  else
    read_token stream
};

(*
  |read_optional <stream> <default>| reads an optional argument. If none is present <default> is returned.
*)

value read_optional stream default = do
{
  let rec read_group nest = do
  {
    let c = UCStream.pop stream;

    match cat_code c with
    [ BeginOptional -> [c :: read_group (nest + 1)]
    | EndOptional   -> if nest = 0 then
                         []
                       else
                         [c :: read_group (nest - 1)]
    | _             -> [c :: read_group nest]
    ]
  };

  skip_blanks stream;

  if cat_code (UCStream.next_char stream) = BeginOptional then do
  {
    UCStream.pop stream;
    read_group 0
  }
  else
    default
};

(* |read_bool <stream>| checks whether the next character is a star. *)

value read_bool stream = do
{
  skip_blanks stream;

  if UCStream.next_char stream = 42 then do
  {
    UCStream.pop stream;

    True
  }
  else
    False
};

(* |read_true_false <stream>| checks whether the next word is "true" or "false". *)

value read_true_false stream = do
{
  skip_blanks stream;

  if UCStream.match_prefix stream [116; 114; 117; 101] then do
  {
    UCStream.remove stream 4;

    True
  }
  else if UCStream.match_prefix stream [102; 97; 108; 115; 101] then do
  {
    UCStream.remove stream 5;

    False
  }
  else
    False
};

(* |read_list <stream>| parses a list separated by commas or semicolons. *)

value rec read_list stream = do
{
  let rec get_next result = do
  {
    let rec strip_blanks str = match str with
    [ []      -> []
    | [c::cs] -> match cat_code c with
      [ Space | Newline -> strip_blanks cs
      | _               -> str
      ]
    ];

    match UCStream.pop stream with
    [ (-1) | 44 | 59 -> List.rev (strip_blanks result)
    | c              -> get_next [c :: result]
    ]
  };

  let l = ListBuilder.make ();

  iter ()

  where rec iter () = do
  {
    skip_blanks stream;

    if UCStream.next_char stream = (-1) then
      ListBuilder.get l
    else do
    {
      match get_next [] with
      [ [] -> ()
      | v  -> ListBuilder.add l v
      ];

      iter ()
    }
  }
};

(* |read_keyword <stream>| reads a keyword of a key-value list. *)

value read_keyword stream = do
{
  iter ()

  where rec iter () = do
  {
    let c = UCStream.next_char stream;

    match cat_code c with
    [ Letter | Other -> match c with
      [ 44 | 59 | 61 -> []             (* , ; = *)
      | _            -> do
        {
          UCStream.pop stream;

          [c :: iter ()]
        }
      ]
    | _ -> []
    ]
  }
};

(* |read_key_val_list <stream>| reads a list of key-value pairs. *)

value rec read_key_val_list stream = do
{
  let rec read_value () = do
  {
    let val    = ListBuilder.make ();
    let blanks = ListBuilder.make ();

    let add_char c = do
    {
      ListBuilder.append val blanks;
      ListBuilder.add    val c
    };

    let rec read_nested nest = do
    {
      let c = UCStream.pop stream;

      match c with
      [ (-1)    -> ListBuilder.get val
      | 44 | 59 -> do                   (* , ; *)
        {
          if nest <= 0 then
            ListBuilder.get val
          else do
          {
            add_char c;
            read_nested nest
          }
        }
      | 123 -> do                       (* { *)
        {
          add_char c;
          read_nested (nest + 1)
        }
      | 125 -> do                       (* } *)
        {
          if nest <= 0 then
            ListBuilder.get val
          else do
          {
            add_char c;
            read_nested (nest - 1)
          }
        }
      | _ -> do
        {
          match cat_code c with
          [ Space | Newline -> ListBuilder.add blanks c
          | _               -> add_char c
          ];
          read_nested nest
        }
      ]
    };

    skip_blanks stream;

    if UCStream.next_char stream = 123 then do          (* { *)
    {
      UCStream.pop stream;

      let first_part = read_group [] stream;

      let rest = ListBuilder.make ();

      read_blanks ()

      where rec read_blanks () = do
      {
        let c = UCStream.pop stream;

        match cat_code c with
        [ Space | Newline -> do
          {
            ListBuilder.add rest c;
            read_blanks ()
          }
        | _ -> match c with
          [ (-1) | 44 | 59 | 125 -> List.rev first_part   (* , ; } *)
          | _ -> do
            {
              let tail = read_nested 0;

              ListBuilder.add_list rest tail;

              [123 :: List.rev_append
                        [125 :: first_part]
                        (ListBuilder.get rest)]
            }
          ]
        ]
      }
    }
    else
      read_nested 0
  };

  iter DynUCTrie.empty

  where rec iter dict = do
  {
    skip_blanks stream;

    match cat_code (UCStream.next_char stream) with
    [ Comment -> do
      {
        skip_comment stream;
        iter dict
      }
    | EOF -> dict
    | _   -> do
      {
        let key = read_keyword stream;

        skip_blanks stream;

        if UCStream.next_char stream = 61 then do        (* = *)
        {
          UCStream.pop stream;

          let val = read_value ();

          iter (DynUCTrie.add_list key (Some val) dict)
        }
        else do
        {
          skip_rest ()

          where rec skip_rest () = match UCStream.pop stream with
          [ 44 | 59 | (-1) -> do              (* , ; *)
            {
              iter (DynUCTrie.add_list key None dict)
            }
          | _ -> skip_rest ()
          ]
        }
      }
    ]
  }
};



(*** parsing numbers and dimensions *****************************************************)



value read_digit stream base = do
{
  let c = UCStream.next_char stream;

  if c >= 48 && c <= 57 then do
  {
    if c < 48 + base then do
    {
      UCStream.remove stream 1;
      c - 48
    }
    else
      -1
  }
  else if base > 10 then do
  {
    if c >= 65 && c < 55 + base then do
    {
      UCStream.remove stream 1;
      c - 55
    }
    else if c >= 97 && c < 87 + base then do
    {
      UCStream.remove stream 1;
      c - 87
    }
    else
      -1
  }
  else
    -1
};

value read_unsigned_int stream = do
{
  let base = if UCStream.next_char stream = 48 then
               match UCStream.get_char stream 1 with
               [ 120 | 88 -> do { UCStream.remove stream 2; 16 }
               | 111 | 79 -> do { UCStream.remove stream 2;  8 }
               |  98 | 66 -> do { UCStream.remove stream 2;  2 }
               | _        -> 10
               ]
             else
               10;
  iter 0

  where rec iter n = do
  {
    let d = read_digit stream base;

    if d > -1 then
      iter (base * n + d)
    else
      (n, base)
  }
};

value read_int stream = do
{
  if UCStream.next_char stream = 45 then do
  {
    UCStream.remove stream 1;

    let (n, b) = read_unsigned_int stream;

    (~-n, b)
  }
  else
    read_unsigned_int stream
};

value rec read_number stream = do
{
  if UCStream.next_char stream = 45 then do
  {
    UCStream.remove stream 1;
    minus_num (read_number stream)
  }
  else do
  {
    let (n, base) = read_int stream;

    let rec read_fraction x r = do
    {
      let d = read_digit stream base;

      if d >= 0 then
        read_fraction (x +/ r */ num_of_int d) (r // num_of_int base)
      else
        x
    };

    match UCStream.next_char stream with
    [ 44 | 46 -> do
                 {
                   UCStream.pop stream;
                   num_of_int n +/ read_fraction num_zero (num_of_ints 1 base)
                 }
    | _       -> num_of_int n
    ]
  }
};

(*
  |conv_dimen <number> <$c_1$> <$c_2$>| converts a number in units <c_1c_2> to either points, em, ex, or mu units.
  |read_skip| reads a fixed dimension (e.g., |-12pt|), |read_skip_with_order| additionally allows orders (e.g.,
  |3.4fill|), and |read_dim| reads an entire dimension (e.g., |-4.5cm plus 1fill minus 12pt|).
*)

value conv_dimen x c1 c2 = match (c1, c2) with
[ ( 98, 112) -> Some (Evaluate.const_pt (x */ (num_of_ints  7227 7200))) (* bp *)
| ( 99,  99) -> Some (Evaluate.const_pt (x */ (num_of_ints 14856 1157))) (* cc *)
| ( 99, 109) -> Some (Evaluate.const_pt (x */ (num_of_ints  7227  254))) (* cm *)
| (100, 100) -> Some (Evaluate.const_pt (x */ (num_of_ints  1238 1157))) (* dd *)
| (101, 109) -> Some (Evaluate.const_em x)                               (* em *)
| (101, 120) -> Some (Evaluate.const_ex x)                               (* ex *)
| (105, 110) -> Some (Evaluate.const_pt (x */ (num_of_ints  7227  100))) (* in *)
| (109, 109) -> Some (Evaluate.const_pt (x */ (num_of_ints  7227 2540))) (* mm *)
| (109, 117) -> Some (Evaluate.const_mu x)                               (* mu *)
| (112,  99) -> Some (Evaluate.const_pt (x */ num_of_int 12))            (* pc *)
| (112, 116) -> Some (Evaluate.const_pt x)                               (* pt *)
| (115, 112) -> Some (Evaluate.const_pt (x // num_of_int 65536))         (* sp *)
| _          -> None
];

value read_skip_or_number stream = do
{
  let x  = read_number stream;
  let c1 = UCStream.get_char stream 0;
  let c2 = UCStream.get_char stream 1;

  match conv_dimen x c1 c2 with
  [ None   -> `Number x
  | Some y -> do
              {
                UCStream.remove stream 2;
                `Skip y
              }
  ]
};

value number_to_skip loc n = do
{
  log_warn loc "Unit expected! Assuming points.";

  Evaluate.const_pt n
};

value read_skip stream = do
{
  match read_skip_or_number stream with
  [ `Skip s   -> s
  | `Number x -> number_to_skip (UCStream.location stream) x
  ]
};

value read_skip_with_order stream = do
{
  let x  = read_number stream;
  let c1 = UCStream.get_char stream 0;
  let c2 = UCStream.get_char stream 1;

  if c1 = 102 && c2 = 105 then do   (* fi... *)
  {
    UCStream.remove stream 2;

    iter 0

    where rec iter ord = do
    {
      if UCStream.next_char stream = 108 then do  (* l *)
      {
        UCStream.remove stream 1;
        iter (ord + 1)
      }
      else
        (fun _ -> x, ord)
    }
  }
  else match conv_dimen x c1 c2 with
  [ None   -> (number_to_skip (UCStream.location stream) x, 0)
  | Some y -> do
              {
                UCStream.remove stream 2;
                (y, 0)
              }
  ]
};

value read_dim_or_number stream = do
{
  let read_plus_minus key = do
  {
    find_key 0

    where rec find_key i = do
    {
      if cat_code (UCStream.get_char stream i) = Space then
        find_key (i + 1)
      else if match_substring stream key i then do   (* plus *)
      {
        find_skip (i + List.length key)

        where rec find_skip i = do
        {
          if cat_code (UCStream.get_char stream i) = Space then
            find_skip (i + 1)
          else do
          {
            UCStream.remove stream i;
            read_skip_with_order stream
          }
        }
      }
      else
        (fun _ -> num_zero, 0)
    }
  };

  match read_skip_or_number stream with
  [ `Number x  -> `Number x
  | `Skip base -> do
    {
      let (st_f, st_o) = read_plus_minus [112; 108; 117; 115];
      let (sh_f, sh_o) = read_plus_minus [109; 105; 110; 117; 115];

      `Dim (fun e ->
            {
              d_base = base e;
              d_stretch_factor = st_f e;
              d_stretch_order  = st_o;
              d_shrink_factor  = sh_f e;
              d_shrink_order   = sh_o
            })
    }
  ]
};

value number_to_dim loc n = do
{
  log_warn loc "Unit expected! Assuming points.";
  (fun _ -> fixed_dim n)
};

value read_dim stream = do
{
  match read_dim_or_number stream with
  [ `Dim d    -> d
  | `Number x -> number_to_dim (UCStream.location stream) x
  ]
};



(*** parsing arithmetic expressions *****************************************************)



type expr 'a =
[ Atom   of 'a
| Scalar of num
| Add    of expr 'a and expr 'a
| Sub    of expr 'a and expr 'a
| Mul    of expr 'a and expr 'a
| Div    of expr 'a and expr 'a
];

value make_expression x = Atom x;

value add_expr conv x y = match (x,y) with
[ (Scalar s, Scalar t) -> Scalar (s +/ t)
| (Scalar s, _)        -> Add (Atom (conv s)) y
| (_, Scalar s)        -> Add x (Atom (conv s))
| _                    -> Add x y
];

value sub_expr conv x y = match (x,y) with
[ (Scalar s, Scalar t) -> Scalar (s -/ t)
| (Scalar s, _)        -> Sub (Atom (conv s)) y
| (_, Scalar s)        -> Sub x (Atom (conv s))
| _                    -> Sub x y
];

value mul_expr loc x y = match (x,y) with
[ (Scalar s, Scalar t) -> Scalar (s */ t)
| (Scalar _, _)        -> Mul x y
| (_, Scalar _)        -> Mul y x
| _                    -> do
                          {
                            log_warn loc "undefined multiplication! Ignoring the second factor.";
                            x
                          }
];

value div_expr loc x y = match (x,y) with
[ (Scalar s, Scalar t) -> Scalar (s // t)
| (_, Scalar _)        -> Div x y
| _                    -> do
                          {
                            log_warn loc "undefined division! Ignoring the second value.";
                            x
                          }
];

value evaluate_expression expr conv atom add sub mul = do
{
  eval expr

  where rec eval expr = match expr with
  [ Atom x   -> atom x
  | Scalar x -> atom (conv x)
  | Add x y  -> add (eval x) (eval y)
  | Sub x y  -> sub (eval x) (eval y)
  | Mul x y  -> match x with
                [ Scalar s -> mul s (eval y)
                | _        -> assert False
                ]
  | Div x y  -> match y with
                [ Scalar s -> mul (num_one // s) (eval x)
                | _        -> assert False
                ]
  ]
};

(*
  |read_expression <read-atom> <conv> <stream>| reads an arithmetic expression from <stream> where atomic
  expressions are recognised by <read-atom>. <conv> is used in case of a type error to convert a number
  into the type in question.
*)

value rec read_expression read_atom conv stream = do
{
  let rec read_summands result = do
  {
    skip_blanks stream;

    match UCStream.get_char stream 0 with
    [ 43 -> do
            {
              UCStream.remove stream 1;
              let expr = read_summand read_atom conv stream;
              read_summands [(True, expr) :: result]
            }
    | 45 -> do
            {
              UCStream.remove stream 1;
              let expr = read_summand read_atom conv stream;
              read_summands [(False, expr) :: result]
            }
    | _  -> result
    ]
  };

  let loc   = UCStream.location stream;
  let first = read_summand read_atom conv stream;
  let rest  = read_summands [];

  iter first rest

  where rec iter first rest = match rest with
  [ []                 -> first
  | [(True,  e) :: es] -> iter (add_expr (conv loc) first e) es
  | [(False, e) :: es] -> iter (sub_expr (conv loc) first e) es
  ]
}

(* read a product or atom *)

and read_summand read_atom conv stream = do
{
  let rec read_factors result = do
  {
    skip_blanks stream;

    match UCStream.get_char stream 0 with
    [ 42 -> do
            {
              UCStream.remove stream 1;
              let expr = read_simple_expr read_atom conv stream;
              read_factors [(True, expr) :: result]
            }
    | 47 -> do
            {
              UCStream.remove stream 1;
              let expr = read_simple_expr read_atom conv stream;
              read_factors [(False, expr) :: result]
            }
    | _  -> result
    ]
  };

  let loc   = UCStream.location stream;
  let first = read_simple_expr read_atom conv stream;
  let rest  = read_factors [];

  iter first rest

  where rec iter first rest = match rest with
  [ []                 -> first
  | [(True,  e) :: es] -> iter (mul_expr loc first e) es
  | [(False, e) :: es] -> iter (div_expr loc first e) es
  ]
}

(* reading atoms and expressions enclosed in parenthesis *)

and read_simple_expr read_atom conv stream = do
{
  let read_paren brace = do
  {
    UCStream.remove stream 1;  (* skip opening parenthesis *)

    let expr = read_expression read_atom conv stream;

    skip_blanks stream;

    if UCStream.get_char stream 0 = brace then
      UCStream.remove stream 1
    else do
    {
      log_warn (UCStream.location stream) "missing ";
      log_uc_list [brace; UCStream.get_char stream 0; UCStream.get_char stream 1; UCStream.get_char stream 2; UCStream.get_char stream 3]
    };

    expr
  };

  skip_blanks stream;

  match UCStream.get_char stream 0 with
  [ (-1) -> raise (Failure "parse error")
  | 123  -> read_paren 125  (* { *)
  | 40   -> read_paren 41   (* ( *)
(* FIX: does not work since we cannot depend on the Macro module.
  | 92   -> do
    {
      let cmd = read_token stream;

      let s = UCStream.of_string (Macro.expand cmd);

      read_expression read_atom conv s
    }
*)
  | _    -> read_atom stream
  ]
};

value read_num_atom stream = Scalar (read_number stream);

value read_num_expression stream = do
{
  match read_expression read_num_atom (fun _ x -> x) stream with
  [ Scalar x -> x
  | _        -> assert False
  ]
};

value read_simple_num_expression stream = do
{
  match read_simple_expr read_num_atom (fun _ x -> x) stream with
  [ Scalar x -> x
  | _        -> assert False
  ]
};

value read_skip_atom stream = do
{
  match read_skip_or_number stream with
  [ `Skip   s -> Atom s
  | `Number n -> Scalar n
  ]
};

value read_skip_expression stream = do
{
  match read_expression read_skip_atom number_to_skip stream with
  [ Scalar s -> number_to_skip (UCStream.location stream) s
  | expr     -> (fun env ->
                  evaluate_expression
                    expr
                    (number_to_skip (UCStream.location stream))
                    (fun s -> s env)
                    add_num
                    sub_num
                    mult_num)
  ]
};

value read_simple_skip_expression stream = do
{
  match read_simple_expr read_skip_atom number_to_skip stream with
  [ Scalar s -> number_to_skip (UCStream.location stream) s
  | expr     -> (fun env ->
                  evaluate_expression
                    expr
                    (number_to_skip (UCStream.location stream))
                    (fun s -> s env)
                    add_num
                    sub_num
                    mult_num)
  ]
};

value read_dim_atom stream = do
{
  match read_dim_or_number stream with
  [ `Dim    d -> Atom   d
  | `Number n -> Scalar n
  ]
};

value read_dim_expression stream = do
{
  match read_expression read_dim_atom number_to_dim stream with
  [ Scalar s -> (fun _    -> fixed_dim s)
  | expr     -> (fun env ->
                  evaluate_expression
                    expr
                    (number_to_dim (UCStream.location stream))
                    (fun d -> d env)
                    dim_add
                    dim_sub
                    dim_mult)
  ]
};

value read_simple_dim_expression stream = do
{
  match read_simple_expr read_dim_atom number_to_dim stream with
  [ Scalar s -> (fun _   -> fixed_dim s)
  | expr     -> (fun env ->
                  evaluate_expression
                    expr
                    (number_to_dim (UCStream.location stream))
                    (fun d -> d env)
                    dim_add
                    dim_sub
                    dim_mult)
  ]
};


(*** parsing lists and ranges ***********************************************************)


value read_range stream = do
{
  skip_spaces stream;

  let n1 = if UCStream.next_char stream = 45 then  (* - *)
             num_zero
           else
             read_simple_num_expression stream;

  skip_spaces stream;

  if UCStream.next_char stream = 45 then do (* - *)
  {
    UCStream.remove stream 1;

    skip_spaces stream;

    let n2 = if UCStream.next_char stream = (-1) then
               num_zero
             else
               read_simple_num_expression stream;

    skip_spaces stream;

    (n1, n2)
  }
  else
    (n1, n1)
};



(*** conversion functions ***************************************************************)


value str_to_stream str = do
{
  let stream = UCStream.of_list str;

  skip_spaces stream;

  stream
};

value str_to_bool      str = read_true_false        (str_to_stream str);
value str_to_uint      str = fst (read_unsigned_int (str_to_stream str));
value str_to_int       str = fst (read_int          (str_to_stream str));
value str_to_num       str = read_number            (str_to_stream str);
value str_to_skip      str = read_skip              (str_to_stream str);
value str_to_dim       str = read_dim               (str_to_stream str);
value str_expr_to_num  str = read_num_expression    (str_to_stream str);
value str_expr_to_skip str = read_skip_expression   (str_to_stream str);
value str_expr_to_dim  str = read_dim_expression    (str_to_stream str);
value str_to_list      str = read_list              (str_to_stream str);
value str_to_key_val   str = read_key_val_list      (str_to_stream str);


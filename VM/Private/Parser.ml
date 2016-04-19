
open XNum;
open Runtime;
open Unicode;
open Types;
open SymbolTable;
open Lexer;

(*
  Priorities:

   2 right ||
   3 right &&
   4 non   == <> > < >= <=
   5 left  land lor lxor lsr lsl
   6 left  + -
   7 left  * / mod
   8 left  ^
   9 left  function application
  11 right prefix operations
  12 left  postfix operations
*)

type pattern =
[ PAnything
| PId of symbol
| PNumber of num
| PChar of uc_char
| PSymbol of symbol
| PTuple of list pattern
| PList of list pattern
| PListTail of list pattern and pattern
| PAssign of symbol and pattern
];

type term =
[ TUnbound
| TId of symbol
| TNumber of num
| TChar of uc_char
| TSymbol of symbol
| TApp of term and list term
| TTuple of list term
| TList of list term
| TListTail of list term and term
| TFun of list (list pattern * option term * term)
| TLocal of list decl and term
| TSequence of list stmt and term
| TDo of list stmt
| TIfThenElse of term and term and term
| TMatch of term and list (pattern * option term * term)
]
and decl =
[ DFun of symbol and list pattern and option term and term
| DPattern of pattern and term
]
and stmt =
[ SEquation of term and term
| SIfThen of term and stmt
| SIfThenElse of term and stmt and stmt
(*| SForce of array term*)
| SFunction of term
];

type stmt_or_expr =
[ Expression of term
| Statement of stmt
];

(* program -> decl-list EOF *)

value rec parse_program lexer = match parse_decl_list lexer with
[ (ds, EOF) -> ds
| (_, tok)  -> syntax_error_uc lexer
                 (UString.append (UString.uc_string_of_ascii "unexpected ") (token_to_string tok))
]

(* decl-list -> empty | decl ";" decl-list *)

and parse_decl_list lexer = match read_token lexer with
[ EOF -> ([], EOF)
| END -> ([], END)
| tok -> match parse_decl tok lexer with
  [ (d, SEMICOLON) -> do
    {
      let (ds, tok) = parse_decl_list lexer;

      (d @ ds, tok)
    }
  | x -> x
  ]
]

(*
  decl ->
      id arg-list
    | "(" bin-op ")" arg-list
    | pattern bin-op pattern ":=" stmt-list-expr
    | id-list
    | "declare_infix_l" num id-list
    | "declare_infix_n" num id-list
    | "declare_infix_r" num id-list
    | "declare_prefix"  id-list
    | "declare_postfix" id-list
*)

and parse_decl first_token lexer = match first_token with
[ LID id -> do
  {
    parse_arg_list lexer id first_token
  }
| INFIX assoc -> match read_token lexer with
  [ NUMBER n -> match parse_id_list lexer with
    [ (syms, SEMICOLON) -> do
      {
        List.iter (Lexer.add_bin_op lexer (int_of_num (round_num n)) assoc) syms;
        parse_decl (read_token lexer) lexer
      }
    | _ -> syntax_error lexer "; expected"
    ]
  | _ -> syntax_error lexer "number expected"
  ]
| PREFIX -> match parse_id_list lexer with
  [ (syms, SEMICOLON) -> do
    {
      List.iter (Lexer.add_pre_op lexer) syms;
      parse_decl (read_token lexer) lexer
    }
  | _ -> syntax_error lexer "; expected"
  ]
| POSTFIX -> match parse_id_list lexer with
  [ (syms, SEMICOLON) -> do
    {
      List.iter (Lexer.add_post_op lexer) syms;
      parse_decl (read_token lexer) lexer
    }
  | _ -> syntax_error lexer "; expected"
  ]
| _ -> do
  {
    (* check for special case "(" bin-op ")" *)

    let bin_op_in_front = match first_token with
    [ PARENOPEN -> do
      {
        let t1 = read_token lexer;

        match t1 with
        [ BINOP x _ _ -> match read_token lexer with
          [ PARENCLOSE -> Some x
          | t2         -> do
            {
              restore_token lexer t2;
              restore_token lexer t1;
              None
            }
          ]
        | _ -> do { restore_token lexer t1; None }
        ]
      }
    | _ -> None
    ];

    match bin_op_in_front with
    [ Some x -> parse_arg_list lexer x first_token
    | None   -> do
      {
        let (p0, t0) = parse_pattern first_token lexer;

        match t0 with
        [ BINOP x _ _ -> match parse_pattern (read_token lexer) lexer with
          [ (p1, COLON_EQUAL) -> do
            {
              let (e, tok) = parse_stmt_list_expr lexer;

              ([DFun x [p0; p1] None e], tok)
            }
          | (p1, AMPERSAND) -> match parse_expr (read_token lexer) lexer with
            [ (g, COLON_EQUAL) -> do
              {
                let (e, tok) = parse_stmt_list_expr lexer;

                ([DFun x [p0; p1] (Some g) e], tok)
              }
            | _ -> syntax_error lexer ":= expected"
            ]
          | _ -> syntax_error lexer ":= expected"
          ]
        | COLON_EQUAL -> do
          {
            let (e, tok) = parse_stmt_list_expr lexer;
            ([DPattern p0 e], tok)
          }
        | _ -> syntax_error lexer "binary operator or := expected after pattern"
        ]
      }
    ]
  }
]

(* arg-list -> pattern-list ":=" stmt-list-expr *)

and parse_arg_list lexer id first_token = do
{
  let tok = read_token lexer;

  match tok with
  [ BINOP x _ _ -> do
    {
      let p1 = parse_simple_pattern (read_token lexer) lexer;

      match read_token lexer with
      [ COLON_EQUAL -> do
        {
          let (e, tok) = parse_stmt_list_expr lexer;

          ([DFun x [PId id; p1] None e], tok)
        }
      | AMPERSAND -> match parse_expr (read_token lexer) lexer with
        [ (g, COLON_EQUAL) -> do
          {
            let (e, tok) = parse_stmt_list_expr lexer;

            ([DFun x [PId id; p1] (Some g) e], tok)
          }
        | _ -> syntax_error lexer ":= expected"
        ]
      | _ -> syntax_error lexer ":= or & expected"
      ]
    }
  | COLON_EQUAL -> do
    {
      let (e, tok) = parse_stmt_list_expr lexer;

      ([DPattern (PId id) e], tok)
    }
  | AMPERSAND -> match parse_expr (read_token lexer) lexer with
    [ (g, COLON_EQUAL) -> do
      {
        let (e, tok) = parse_stmt_list_expr lexer;

        ([DFun id [] (Some g) e], tok)
      }
    | _ -> syntax_error lexer ":= expected"
    ]
  | EQUAL -> do
    {
      restore_token lexer EQUAL;

      match parse_pattern first_token lexer with
      [ (p, COLON_EQUAL) -> do
        {
          let (e, tok) = parse_stmt_list_expr lexer;
          ([DPattern p e], tok)
        }
      | _ -> syntax_error lexer ":= expected"
      ]
    }
  | END       -> ([DFun id [] None TUnbound], END)
  | SEMICOLON -> ([DFun id [] None TUnbound], SEMICOLON)
  | _ -> match parse_pattern_list tok lexer with
    [ (ps, COLON_EQUAL) -> do
      {
        let (e, tok) = parse_stmt_list_expr lexer;
        ([DFun id ps None e], tok)
      }
    | (ps, AMPERSAND) -> match parse_expr (read_token lexer) lexer with
      [ (g, COLON_EQUAL) -> do
        {
          let (e, tok) = parse_stmt_list_expr lexer;

          ([DFun id ps (Some g) e], tok)
        }
      | _ -> syntax_error lexer ":= expected"
      ]
    | (ids, SEMICOLON) -> do
      {
        (iter ids, SEMICOLON)

        where rec iter ids = match ids with
        [ []            -> [DFun id [] None TUnbound]
        | [PId s :: is] -> [DFun s  [] None TUnbound :: iter is]
        | _             -> syntax_error lexer ":= expected"
        ]
      }
    | (ids, END) -> do
      {
        (iter ids, END)

        where rec iter ids = match ids with
        [ []            -> [DFun id [] None TUnbound]
        | [PId s :: is] -> [DFun s  [] None TUnbound :: iter is]
        | _             -> syntax_error lexer ":= expected"
        ]
      }
    | _ -> syntax_error lexer ":= or & expected"
    ]
  ]
}

(*
  stmt_list_expr =
      expr
    | stmt "," stmt_list_expr
*)

and parse_stmt_list_expr lexer = do
{
  match iter (read_token lexer) with
  [ ([], e, tok) -> (e, tok)
  | (s, e, tok)  -> (TSequence s e, tok)
  ]

  where rec iter first_token = match parse_stmt_or_expr first_token lexer with
  [ (Expression e, tok) -> ([], e, tok)
  | (Statement s,  tok) -> do
    {
      if tok = COMMA then do
      {
        let (ss, e, tok) = iter (read_token lexer);
        ([s::ss], e, tok)
      }
      else
        syntax_error lexer ", expected"
    }
  ]
}

(*
  stmt =
      expr "=" expr
    | "force" simple-expr-list
    | "if" expr "then" stmt ("elseif" expr "then" stmt)^* ["else" stmt] "end"
*)

and parse_stmt first_token lexer = match first_token with
[ IF    -> parse_if_stmt lexer
| _  -> match parse_expr first_token lexer with
  [ (e0, EQUAL) -> do
    {
      let (e1, tok) = parse_expr (read_token lexer) lexer;
      (SEquation e0 e1, tok)
    }
  | _ -> syntax_error lexer "= expected" (* FIX: relations *)
  ]
]

and parse_if_stmt lexer = match parse_expr (read_token lexer) lexer with
[ (p, THEN) -> match parse_stmt (read_token lexer) lexer with
  [ (s0, ELSE) -> match parse_stmt (read_token lexer) lexer with
    [ (s1, END) -> (SIfThenElse p s0 s1, read_token lexer)
    | _         -> syntax_error lexer "end expected"
    ]
  | (s0, ELSEIF) -> do
    {
      let (s1, tok) = parse_if_stmt lexer;

      (SIfThenElse p s0 s1, tok)
    }
  | (s0, END) -> (SIfThen p s0, read_token lexer)
  | _         -> syntax_error lexer "else or end expected"
  ]
| _ -> syntax_error lexer "then expected"
]

(*
  expr =
      simple-expr-list sub-expr
    | number simple-expr-list sub-expr
    | "local" decl-list "in" expr
*)

and multiply_symbol = TId (string_to_symbol [|42|])   (* * *)

and parse_expr first_token lexer = match parse_expr_pri 0 first_token lexer with
[ (e, WHERE) -> match parse_decl_list lexer with
  [ (decls, END) -> (TLocal decls e, read_token lexer)
  | _            -> syntax_error lexer "end expected"
  ]
| x -> x
]

and parse_expr_pri pri first_token lexer = match first_token with
[ LOCAL -> match read_token lexer with
  [ BEGIN -> match parse_decl_list lexer with
    [ (decls, END) -> do
      {
        let (e, tok) = parse_expr_pri pri (read_token lexer) lexer;

        (TLocal decls e, tok)
      }
    | _ -> syntax_error lexer "end exptected"
    ]
  | tok -> match parse_decl tok lexer with
    [ (decl, SEMICOLON) -> do
      {
        let (e, t) = parse_expr_pri pri (read_token lexer) lexer;

        (TLocal decl e, t)
      }
    | _ -> syntax_error lexer "; expected"
    ]
  ]
| _ -> do
  {
    let (term, tok) = match parse_simple_expr_list first_token lexer with
    [ ([e],     tok)           -> (e, tok)
    | ([TNumber n :: es], tok) -> do
      {
        (* transform  n f x_0 ... x_s  to  n * f x_0 ... x_s  *)

        let x = match es with
        [ [c]     -> c
        | [c::cs] -> TApp c cs
        | []      -> assert False
        ];

        (TApp multiply_symbol [TNumber n; x], tok)
      }
    | ([e :: es], tok) -> (TApp e es, tok)
    | (_, _)           -> assert False
    ];

    parse_sub_expr term pri tok lexer
  }
]

and parse_do_expr lexer = do
{
  let convert x = match x with
  [ Statement  s -> s
  | Expression e -> SFunction e
  ];

  TDo (iter (read_token lexer))

  where rec iter tok = match parse_stmt_or_expr tok lexer with
  [ (e, SEMICOLON) -> match read_token lexer with
                      [ END -> [convert e]
                      | tok -> [convert e :: iter tok]
                      ]
  | (e, END)       -> [convert e]
  | _              -> syntax_error lexer "end or ; expected"
  ]
(*  where rec iter tok = match parse_expr tok lexer with
  [ (e, SEMICOLON) -> match read_token lexer with
                      [ END -> [e]
                      | tok -> [e :: iter tok]
                      ]
  | (e, END)       -> [e]
  | _              -> syntax_error lexer "end or ; expected"
  ]*)
}

and parse_if_expr lexer = match parse_expr (read_token lexer) lexer with
[ (p, THEN) -> match parse_stmt_list_expr lexer with
  [ (e0, ELSE) -> match parse_stmt_list_expr lexer with
    [ (e1, END) -> TIfThenElse p e0 e1
    | _         -> syntax_error lexer "end expected"
    ]
  | (e0, ELSEIF) -> TIfThenElse p e0 (parse_if_expr lexer)
  | _ -> syntax_error lexer "else expected"
  ]
| _ -> syntax_error lexer "then expected"
]

(*
  stmt-or-expr = stmt | expr
*)

and parse_stmt_or_expr first_token lexer = match first_token with
[ IF -> match parse_if_stmt_or_expr lexer with
  [ (Expression expr, tok) -> do
    {
      let (e, t) = parse_sub_expr expr 0 tok lexer;

      (Expression e, t)
    }
  | stmt -> stmt
  ]
| _     -> match parse_expr first_token lexer with
  [ (e0, EQUAL) -> do
    {
      let (e1, tok) = parse_expr (read_token lexer) lexer;
      (Statement (SEquation e0 e1), tok)
    }
  | (e, tok) -> (Expression e, tok)
  ]
]

and parse_if_stmt_or_expr lexer = match parse_expr (read_token lexer) lexer with
[ (p, THEN) -> match parse_stmt_or_expr (read_token lexer) lexer with
  [ (Statement s0, ELSE) -> match parse_stmt (read_token lexer) lexer with
    [ (s1, END) -> (Statement (SIfThenElse p s0 s1), read_token lexer)
    | _         -> syntax_error lexer "end expected"
    ]
  | (Expression e0, ELSE) -> match parse_expr (read_token lexer) lexer with
    [ (e1, END) -> (Expression (TIfThenElse p e0 e1), read_token lexer)
    | _         -> syntax_error lexer "end expected"
    ]
  | (Statement s0, ELSEIF) -> do
    {
      let (s1, tok) = parse_if_stmt lexer;

      (Statement (SIfThenElse p s0 s1), tok)
    }
  | (Expression e0, ELSEIF) -> do
    {
      let e1 = parse_if_expr lexer;

      (Expression (TIfThenElse p e0 e1), read_token lexer)
    }
  | (Expression e0, END) -> (Statement (SIfThen p (SFunction e0)), read_token lexer)
  | (Statement s0,  END) -> (Statement (SIfThen p s0),             read_token lexer)
  | _                    -> syntax_error lexer "else or end expected"
  ]
| _ -> syntax_error lexer "then expected"
]

(*
  sub-expr =
      nothing
    | bin-op expr
    | "where" decl-list "end"
*)

and parse_sub_expr term pri first_token lexer = match first_token with
[ BINOP x p a -> do
  {
    if p >= pri then match read_token lexer with
    [ PARENCLOSE -> do
      {
        restore_token lexer PARENCLOSE;
        (term, first_token)
      }
    | tok -> match a with
      [ Left -> do
        {
          let (e,t) = parse_expr_pri (p+1) tok lexer;

          parse_sub_expr (TApp (TId x) [term; e]) pri t lexer
        }
      | Right -> do
        {
          let (e,t) = parse_expr_pri p tok lexer;

          parse_sub_expr (TApp (TId x) [term; e]) pri t lexer
        }
      | NonA -> do
        {
          let (e,t) = parse_expr_pri (p+1) tok lexer;

          if p > pri then
            parse_sub_expr (TApp (TId x) [term; e]) pri t lexer
          else
            (TApp (TId x) [term; e], t)
        }
      ]
    ]
    else
      (term, first_token)
  }
| _ -> (term, first_token)
]

(*
  simple-expr-with-post-op =
      simple-expr
    | simple-expr-with-post-op post-op
*)

and parse_simple_expr_with_post_op first_tok lexer = do
{
  let (e,tok) = parse_simple_expr first_tok lexer;
  iter e tok

  where rec iter e tok = match tok with
  [ POSTOP x -> iter (TApp (TId x) [e]) (read_token lexer)
  | _        -> (e, tok)
  ]
}

(*
  simple-expr =
      uid
    | lid
    | number
    | character
    | string
    | "_"
    | pre-op simple-expr
    | "(" bin-op ")"
    | "(" bin-op expr ")"
    | "(" expr-comma-list ")"
    | "[" expr-comma-list ":" expr "]"
    | "[" expr-comma-list "]"
    | "{" function-body "}"
    | "begin" stmt-list_expr "end"
    | "if" expr "then" stmt-list-expr
      ("elseif" expr "then" stmt-list-expr)^*
      "else" stmt-list-expr "end" sub-expr
    | "match" expr "{" match-body "}" sub-expr
*)

and parse_simple_expr first_tok lexer = match first_tok with
[ UID x       -> (TSymbol x, read_token lexer)
| LID x       -> (TId x,     read_token lexer)
| UNDERSCORE  -> (TUnbound,  read_token lexer)
| NUMBER x    -> (TNumber x, read_token lexer)
| CHARACTER x -> (TChar x,   read_token lexer)
| STRING str  -> (TList (List.map (fun c -> TChar c) str),
                  read_token lexer)
| PREOP x     -> match read_token lexer with
  [ PARENCLOSE -> (TId x, PARENCLOSE)
  | tok        -> do
    {
      let (e,t) = parse_simple_expr_with_post_op tok lexer;

      (TApp (TId x) [e], t)
    }
  ]
| PARENOPEN   -> do
  {
    match read_token lexer with
    [ BINOP x p _ -> match read_token lexer with
      [ PARENCLOSE -> (TId x, read_token lexer)
      | tok        -> match parse_expr_pri p tok lexer with
        [ (e, PARENCLOSE) -> do
          {
            let v = alloc_symbol ();

            (TFun [([PId v], None, TApp (TId x) [TId v; e])],
             read_token lexer)
          }
        | _ -> syntax_error lexer ") expected"
        ]
      ]
    | tok -> match parse_expr_comma_list tok lexer with
      [ ([e], PARENCLOSE)  -> (e,        read_token lexer)
      | (e, PARENCLOSE)    -> (TTuple e, read_token lexer)
      | ([e], BINOP x _ _) -> match read_token lexer with
        [ PARENCLOSE -> (TApp (TId x) [e], read_token lexer)
        | _          -> syntax_error lexer ") expected"
        ]
      | (_, _) -> syntax_error lexer ", or ) expected"
      ]
    ]
  }
| BRACKETOPEN -> match parse_expr_comma_list (read_token lexer) lexer with
  [ (e, BRACKETCLOSE) -> (TList e, read_token lexer)
  | ([e::es], COLON)  -> match parse_expr (read_token lexer) lexer with
                         [ (tl, BRACKETCLOSE) -> (TListTail [e::es] tl, read_token lexer)
                         | (_,_)              -> syntax_error lexer "] expected"
                         ]
  | (_, _)            -> syntax_error lexer ", or ] expected"
  ]
| BRACEOPEN -> match parse_function_body lexer with
  [ (pats, BRACECLOSE) -> (TFun pats, read_token lexer)
  | _                  -> syntax_error lexer "} expected"
  ]
| BEGIN -> match parse_stmt_list_expr lexer with
  [ (e, END) -> (e, read_token lexer)
  | _        -> syntax_error lexer "end expected"
  ]
| DO    -> let e = parse_do_expr lexer in
           (e, read_token lexer)
| IF    -> let e = parse_if_expr lexer in
           (e, read_token lexer)
| MATCH -> match parse_expr (read_token lexer) lexer with
  [ (e, WITH) -> do
    {
      if read_token lexer = BRACEOPEN then
        match parse_match_body lexer with
        [ (ps, BRACECLOSE) -> (TMatch e ps, read_token lexer)
        | (_, _)           -> syntax_error lexer "} expected"
        ]
      else
        syntax_error lexer "{ expected"
    }
  | _ -> syntax_error lexer "with expected"
  ]
| tok -> syntax_error_uc lexer
           (UString.append (UString.uc_string_of_ascii "unexpected ")
                           (token_to_string tok))
]

(*
  expr-comma-list = expr  |  expr "," expr-comma-list
*)

and parse_expr_comma_list first_token lexer = match first_token with
[ PARENCLOSE
| BRACKETCLOSE
| BRACECLOSE   -> ([], first_token)
| _            -> do
  {
    let (e, tok) = parse_expr first_token lexer;

    match tok with
    [ COMMA -> do
      {
        let (es, t) = parse_expr_comma_list (read_token lexer) lexer;
        ([e :: es], t)
      }
    | _ -> ([e], tok)
    ]
  }
]

(*
  simple-expr-list =
      simple-expr-with-post-op
    | simple-expr-with-post-op simple-expr-list
    | num "/" num simple-expr-list
*)

and divide_symbol = string_to_symbol [|47|]   (* / *)

and parse_simple_expr_list first_token lexer = do
{
  let (e, tok) = parse_simple_expr_with_post_op first_token lexer;

  match tok with
  [ UID _
  | LID _
  | NUMBER _
  | CHARACTER _
  | STRING _
  | PREOP _
  | DO
  | PARENOPEN
  | BRACKETOPEN
  | BRACEOPEN -> do
    {
      let (es, t) = parse_simple_expr_list tok lexer;

      ([e :: es], t)
    }
  | BINOP x p _ when x = divide_symbol -> match e with
    [ TNumber m -> match read_token lexer with
      [ NUMBER n -> match read_token lexer with
        [ (BINOP _ q _ as t) -> do
          {
            if q > p then do
            {
              restore_token lexer t;
              restore_token lexer (NUMBER n);
              ([e], tok)
            }
            else do
            {
              let (es, t) = parse_simple_expr_list tok lexer;

              ([TNumber (m // n) :: es], t)
            }
          }
        | POSTOP y -> do
          {
            restore_token lexer (POSTOP y);
            restore_token lexer (NUMBER n);
            ([e], tok)
          }
        | _ -> do
          {
            let (es, t) = parse_simple_expr_list tok lexer;

            ([TNumber (m // n) :: es], t)
          }
        ]
      | t -> do
        {
          restore_token lexer t;
          ([e], tok)
        }
      ]
    | _ -> ([e], tok)
    ]
  | _  -> ([e], tok)
  ]
}

(*
  pattern =
      simple-pattern
    | lid "=" simple-pattern
    | simple-pattern "=" lid
*)

and parse_pattern first_token lexer = do
{
  let p = parse_simple_pattern first_token lexer;

  match read_token lexer with
  [ EQUAL -> do
    {
      let p2 = parse_simple_pattern (read_token lexer) lexer;

      match (p,p2) with
      [ (PId x, _) -> (PAssign x p2, read_token lexer)
      | (_, PId x) -> (PAssign x p,  read_token lexer)
      | _          -> syntax_error lexer "= expects identifier"
      ]
    }
  | tok -> (p, tok)
  ]
}

(*
  simple-pattern =
      "_"
    | lid
    | number
    | symbol
    | "(" pattern-comma-list ")"
    | "[" pattern-comma-list ":" pattern "]"
    | "[" pattern-comma-list "]"
*)

and parse_simple_pattern first_token lexer = match first_token with
[ UNDERSCORE  -> PAnything
| LID x       -> PId x
| NUMBER x    -> PNumber x
| UID x       -> PSymbol x
| CHARACTER x -> PChar x
| STRING str  -> PList (List.map (fun c -> PChar c) str)
| PARENOPEN   -> match parse_pattern_comma_list lexer with
  [ ([e], PARENCLOSE) -> e
  | (e, PARENCLOSE)   -> PTuple e
  | (_, _)            -> syntax_error lexer ", or ) expected"
  ]
| BRACKETOPEN -> match parse_pattern_comma_list lexer with
  [ (e, BRACKETCLOSE) -> PList e
  | ([e::es], COLON)  -> match parse_pattern (read_token lexer) lexer with
                         [ (tl, BRACKETCLOSE) -> PListTail [e::es] tl
                         | (_,_)              -> syntax_error lexer "] expected"
                         ]
  | (_, _)            -> syntax_error lexer ", or ] expected"
  ]
| BRACEOPEN -> syntax_error lexer "6FIX"
| tok       -> syntax_error_uc lexer
                 (UString.append (UString.uc_string_of_ascii "unexpected ") (token_to_string tok))
]

(*
  pattern-comma-list = pattern  |  pattern "," pattern-comma-list
*)

and parse_pattern_comma_list lexer = do
{
  let first_token = read_token lexer;

  match first_token with
  [ PARENCLOSE
  | BRACKETCLOSE
  | BRACECLOSE   -> ([], first_token)
  | _            -> do
    {
      let (e, tok) = parse_pattern first_token lexer;

      match tok with
      [ COMMA -> do
        {
          let (es, t) = parse_pattern_comma_list lexer;
          ([e :: es], t)
        }
      | _ -> ([e], tok)
      ]
    }
  ]
}

(*
  pattern-list = pattern  |  pattern pattern-list
*)

and parse_pattern_list first_token lexer = do
{
  let (e, tok) = parse_pattern first_token lexer;

  match tok with
  [ UNDERSCORE
  | LID _
  | NUMBER _
  | CHARACTER _
  | STRING _
  | PARENOPEN
  | BRACKETOPEN
  | BRACEOPEN -> do
    {
      let (es, t) = parse_pattern_list tok lexer;

      ([e :: es], t)
    }
  | _  -> ([e], tok)
  ]
}

(*
  match-body = match-clause  |  match-clause "|" match-body
*)

and parse_match_body lexer = match parse_match_clause (read_token lexer) lexer with
[ (p, g, e, BAR) -> do
  {
    let (cs,tok) = parse_match_body lexer;

    ([(p, g, e) :: cs], tok)
  }
| (p, g, e, tok) -> ([(p, g, e)], tok)
]

(*
  match-clause = pattern ["&" expr] ":=" stmt-list-expr
*)

and parse_match_clause first_token lexer = do
{
  match parse_pattern first_token lexer with
  [ (p, COLON_EQUAL) -> do
    {
      let (e, tok) = parse_stmt_list_expr lexer;
      (p, None, e, tok)
    }
  | (p, AMPERSAND) -> match parse_expr (read_token lexer) lexer with
    [ (g, COLON_EQUAL) -> do
      {
        let (e, tok) = parse_stmt_list_expr lexer;
        (p, Some g, e, tok)
      }
    | _ -> syntax_error lexer ":= expected"
    ]
  | (_, _) -> syntax_error lexer ":= or & expected"
  ]
}

(*
  function-body = function-clause  |  function-clause "|" function-body
*)

and parse_function_body lexer = match read_token lexer with
[ BRACECLOSE -> ([], BRACECLOSE)
| tok        -> match parse_function_clause tok lexer with
  [ (p, g, e, BAR) -> do
    {
      let (cs,tok) = parse_function_body lexer;

      ([(p, g, e) :: cs], tok)
    }
  | (p, g, e, tok) -> ([(p, g, e)], tok)
  ]
]

(*
  function-clause = pattern-list ["&" expr] ":=" stmt-list-expr
*)

and parse_function_clause first_token lexer = do
{
  match parse_pattern_list first_token lexer with
  [ (p, COLON_EQUAL) -> do
    {
      let (e, tok) = parse_stmt_list_expr lexer;
      (p, None, e, tok)
    }
  | (p, AMPERSAND) -> match parse_expr (read_token lexer) lexer with
    [ (g, COLON_EQUAL) -> do
      {
        let (e, tok) = parse_stmt_list_expr lexer;
        (p, Some g, e, tok)
      }
    | _ -> syntax_error lexer ":= expected"
    ]
  | (_, _) -> syntax_error lexer ":= or & expected"
  ]
}

(*
  id-list = lid  |  lid id-list
*)

and parse_id_list lexer = do
{
  let tok = read_token lexer;

  match tok with
  [ LID x -> do
    {
      let (ids, t) = parse_id_list lexer;

      ([x :: ids], t)
    }
  | _ -> ([], tok)
  ]
};

value parse_expression lexer = do
{
  match parse_expr (read_token lexer) lexer with
  [ (e, EOF) -> e
  | (_, tok) -> syntax_error_uc lexer
                  (UString.append (UString.uc_string_of_ascii "unexpected ") (token_to_string tok))
  ]
};


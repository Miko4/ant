
open XNum;
open Types;
open Runtime;
open Unicode.Types;
open Unicode.SymbolTable;

module UString = Unicode.UString;  (* we cannot open Unicode because of the name clash with Types *)

value true_sym  = string_to_symbol (UString.uc_string_of_ascii "True");
value false_sym = string_to_symbol (UString.uc_string_of_ascii "False");

value make_tuple_pat elements = match elements with
[ []  -> assert False
| [x] -> x
| _   -> Parser.PTuple elements
];

value compile_pattern off pat = do
{
  let (stack_depth, num_vars, var_names, checks) = compile pat (0, 0, [], []);

  (stack_depth, num_vars, List.rev var_names, checks)
}
where rec compile pat ((stack_depth, num_vars, var_names, checks) as state) = match pat with
[ Parser.PAnything   -> (stack_depth, num_vars,     var_names,          [PCAnything                  :: checks])
| Parser.PId sym     -> (stack_depth, num_vars + 1, [sym :: var_names], [PCVariable (off + num_vars) :: checks])
| Parser.PNumber x   -> (stack_depth, num_vars,     var_names,          [PCNumber x                  :: checks])
| Parser.PChar x     -> (stack_depth, num_vars,     var_names,          [PCChar x                    :: checks])
| Parser.PSymbol sym -> (stack_depth, num_vars,     var_names,          [PCSymbol sym                :: checks])
| Parser.PAssign x p -> do
  {
    let (s,n,v,c) = compile p state;

    (s, n + 1, [x :: v], [PCAssign (off + n) :: c])
  }
| Parser.PTuple xs   -> do
  {
    let len       = List.length xs;
    let (s,n,v,c) = List.fold_right compile xs state;

    (max (s + len - 1) stack_depth, n, v, [PCTuple len :: c])
  }
| Parser.PList xs -> do
  {
    List.fold_right
      (fun x s1 -> do
        {
          let (s,n,v,c) = compile x s1;
          (max (s + 1) stack_depth, n, v, [PCConsList :: c])
        })
      xs
      (stack_depth, num_vars, var_names, [PCNil :: checks])
  }
| Parser.PListTail xs tail -> do
  {
    List.fold_right
      (fun x s1 -> do
        {
          let (s,n,v,c) = compile x s1;
          (max (s + 1) stack_depth, n, v, [PCConsList :: c])
        })
      xs
      (compile tail state)
  }
];

(* Creates a SymbolMap with all declared functions in |decls|. *)

value get_declared_variables decls = do
{
  iter SymbolMap.empty decls

  where rec iter names decls = match decls with
  [ []                                      -> names
  | [(Parser.DFun name pat _ _ as d) :: ds] -> do
    {
      let defs =
        try
          SymbolMap.find name names
        with
        [ Not_found -> [] ];

      match defs with
      [ [Parser.DFun _ p _ _ :: _] -> do
        {
          (* check arity *)

          if List.length p <> List.length pat then
            raise (Syntax_error
                     ("", 0, 0)
                     (UString.append (UString.uc_string_of_ascii "arity mismatch in declaration of ")
                                     (symbol_to_string name)))
          else ()
        }
      | _ -> ()
      ];

      iter (SymbolMap.add name [d :: defs] names) ds
    }
  | [Parser.DPattern p _ :: ds] -> do
    {
      iter (iter_pattern names p) ds

      where rec iter_pattern names p = match p with
      [ Parser.PId name       -> SymbolMap.add name [] names
      | Parser.PAnything
      | Parser.PNumber _
      | Parser.PChar _
      | Parser.PSymbol _      -> names
      | Parser.PTuple ps      -> List.fold_left iter_pattern names ps
      | Parser.PList ps       -> List.fold_left iter_pattern names ps
      | Parser.PListTail ps p -> List.fold_left iter_pattern (iter_pattern names p) ps
      | Parser.PAssign name p -> SymbolMap.add name [] (iter_pattern names p)
      ]
    }
  ]
};

value rec compile_expr scope expr = match expr with
[ Parser.TUnbound -> [BConst Unbound]
| Parser.TId x    -> do
  {
    try
      [Scope.lookup scope x]
    with
    [ Not_found -> raise (Syntax_error
                            ("", 0, 0)
                            (UString.append (UString.uc_string_of_ascii "undefined symbol ")
                                            (symbol_to_string x)))
    ]
  }
| Parser.TNumber x  -> [BConst (Number x)]
| Parser.TChar x    -> [BConst (Char x)]
| Parser.TFun cases -> compile_function scope cases
| Parser.TSymbol x  -> do
  {
    if x = true_sym then
      [BConst (Bool True)]
    else if x = false_sym then
      [BConst (Bool False)]
    else
      [BConst (Symbol x)]
  }
| Parser.TApp f args -> do
  {
    [BApply (List.length args)
     :: compile_expr scope f
      @ List.fold_right
          (fun arg code -> compile_expr scope arg @ code)
          args
          []
    ]
  }
| Parser.TTuple xs -> do
  {
    let rec compile_elements is_const n els xs = match xs with
    [ []      -> (is_const, n, els)
    | [y::ys] -> do
      {
        let e = compile_expr scope y;
        let c = is_const
                && match e with
                   [ [BConst _] -> True
                   | _          -> False
                   ];

        compile_elements c (n+1) [e :: els] ys
      }
    ];

    let (is_const, n, elements) = compile_elements True 0 [] xs;

    if is_const then do
    {
      let values = Array.make n (ref Unbound);

      List.fold_left
        (fun i t -> match t with
          [ [BConst v] -> do
            {
              values.(i) := ref v;
              i-1
            }
          | _ -> assert False
          ])
        (n-1)
        elements;

      [BConst (Tuple values)]
    }
    else
      [BTuple n :: List.fold_left (fun es e -> e @ es) [] elements]
  }
| Parser.TList xs -> do
  {
    iter xs

    where rec iter xs = match xs with
    [ []      -> [BConst Nil]
    | [y::ys] -> do
      {
        let t    = compile_expr scope y;
        let tail = iter ys;

        match (t,tail) with
        [ ([BConst a], [BConst b]) -> [BConst (List (ref a) (ref b))]
        | _                        -> [BPair :: t @ tail]
        ]
      }
    ]
  }
| Parser.TListTail xs tail   -> do
  {
    iter xs

    where rec iter xs = match xs with
    [ []      -> compile_expr scope tail
    | [y::ys] -> do
      {
        let t    = compile_expr scope y;
        let rest = iter ys;

        match (t,rest) with
        [ ([BConst a], [BConst b]) -> [BConst (List (ref a) (ref b))]
        | _                        -> [BPair :: t @ rest]
        ]
      }
    ]
  }
| Parser.TLocal decls expr -> do
  {
    let (new_scope, init_code) = compile_local_declarations scope decls;

    [BEndLocal :: compile_expr new_scope expr @ init_code]
  }
| Parser.TSequence stmts expr -> do
  {
      compile_expr scope expr
    @ List.fold_left
        (fun code s -> compile_statement scope s @ code)
        []
        stmts
  }
| Parser.TDo exprs -> do
  {
    [BFunction 1
      (XList.rev_to_array
        [ BReturn ::
          List.fold_left
            (fun code s -> compile_monad scope s @ code)
            []
            exprs])]
  }
| Parser.TIfThenElse p e0 e1 -> do
  {
    let then_code = compile_expr scope e0;
    let else_code = compile_expr scope e1;

      else_code
    @ [BJump (List.length else_code + 1)
       :: then_code]
    @ [BCondJump (List.length then_code + 2)
       :: compile_expr scope p]
  }
| Parser.TMatch expr pats -> do
  {
      compile_matching scope (List.map (fun (p,g,e) -> ([p],g,e)) pats)
    @ compile_expr scope expr
  }
]
and compile_matching scope cases = do
{
  let num_pats = List.length cases;

  let (_, size, cases_code) =
    List.fold_left
      (fun (i, size, cases) (pats,g,e) -> do
        {
          let (stack_depth, num_vars, vars, compiled_pats) =
            List.fold_left
              (fun (depth, num_vars, vars, pats) pat -> do
                {
                  let (sd, n, v, p) = compile_pattern num_vars pat;

                  (max depth sd,
                   num_vars + n,
                   [v :: vars],
                   [p :: pats])
                 })
              (0, 0, [], [])
              pats;
(* FIX: ensure that the variables are all distinct *)
          let (new_scope, _) = Scope.push scope (List.fold_left (fun a b -> b @ a) [] vars);

          let expr_code  = compile_expr new_scope e;
          let expr_size  = List.length expr_code;

          let guard_code = match g with
            [ None   -> []
            | Some e -> [BCondJump (expr_size + 3)
                         :: compile_expr new_scope e]
            ];
          let guard_size = List.length guard_code;

          let off = if i < num_pats then
                       guard_size + expr_size + 4
                     else
                       0;
          let cpat = XList.rev_to_array compiled_pats;
          let pop  = if Array.length cpat = 1 then
                       BPop
                     else
                       BPopN (Array.length cpat);
          let match_cmd = if Array.length cpat = 1 then
                            BMatch1 cpat.(0) stack_depth num_vars off
                          else
                            BMatchN cpat     stack_depth num_vars off;
          let code = expr_code @ [pop :: guard_code] @ [match_cmd];

          (i + 1,
           size + expr_size + guard_size + 4,
           [code :: cases])
        })
      (1, 0, [])
      cases;

  let (_, code) =
    List.fold_right
      (fun case (size, code) -> do
        {
          let new_size = size - List.length case - 2;

          if new_size > 0 then
            (new_size, [BEndLocal; BJump new_size :: case @ code])
          else
            (new_size, [BEndLocal :: case @ code])
        })
      cases_code
      (size, []);

  code
}
and compile_function scope cases = do
{
  let arity = match cases with
  [ [(ps, _, _) :: _] -> List.length ps
  | _                 -> 0
  ];

  let rec make_dictionary dict cases = match cases with
  [ []                                    -> Some dict
  | [([Parser.PSymbol s], None, e) :: cs] ->
      make_dictionary
        (SymbolMap.add s (compile_expr scope e) dict)
        cs
  | _ -> None
  ];

  match make_dictionary SymbolMap.empty cases with
  [ Some d -> do
    {
      let (code, syms) =
        SymbolMap.fold
          (fun s c (code, syms) -> (c @ code, [s :: syms]))
          d
          ([], []);
      [ BDictionary (Array.of_list syms) :: code ]
    }
  | None   -> [ BFunction arity
                 (XList.rev_to_array
                   [BReturn :: compile_matching scope cases]) ]
  ]
}
and compile_local_declarations scope decls = do
{
  (* extract a list of names *)

  let names = get_declared_variables decls;
  let vars  = SymbolMap.fold
                (fun n _ vars -> [n :: vars])
                names [];
  let (new_scope, num_vars) = Scope.push scope vars;

  (* define the new symbols: first the functions, then the patterns *)

  let fun_defs =
    SymbolMap.fold
      (fun name defs code -> match defs with
       [ [] -> code   (* a pattern, not a function *)
       | _  -> do
         {
           let cases =
             List.fold_left
               (fun cases d -> match d with
                [ Parser.DFun _ pats guard term -> [(pats, guard, term) :: cases]
                | _                             -> assert False
                ])
               []
               defs;
           let (i1,i2) = Scope.lookup_local new_scope name;

           match cases with
           [ [([], None, term)] -> [BSet i1 i2 :: compile_expr (Scope.shift scope 1) term @ code] (* a variable declaration *)
           | _                  -> [BSet i1 i2 :: compile_function new_scope cases @ code]        (* a real function        *)
           ]
         }
       ])
      names
      [ BLocal num_vars ];

  iter fun_defs decls

  where rec iter code decls = match decls with
  [ []                               -> (new_scope, code)
  | [Parser.DFun _ _ _ _ :: ds]      -> iter code ds
  | [Parser.DPattern pat term :: ds] -> match pat with
      [ Parser.PId id -> do
        {
          (* Optimise the most common case. *)
          let (i1,i2) = Scope.lookup_local new_scope id;
          iter [ BSet i1 i2 :: compile_expr new_scope term @ code] ds
        }
      | _ -> do
        {
          let (sd, nv, v, c)   = compile_pattern 0 pat;
          let (local_scope, _) = Scope.push new_scope v;

          let pat_code =
            List.fold_left
              (fun code var -> do
                {
                  let (a1,a2) = Scope.lookup_local new_scope   var;
                  let (b1,b2) = Scope.lookup_local local_scope var;
                  [BSet (a1 + 1) a2; BVariable b1 b2 :: code]
                })
                [ BPop; BMatch1 c sd nv 0 ]
                v;

          iter [BEndLocal :: pat_code @ compile_expr (Scope.shift scope 1) term @ code] ds
        }
      ]
  ]
}
and compile_statement scope stmt = match stmt with
[ Parser.SEquation x y -> [BUnify :: compile_expr scope x @ compile_expr scope y]
| Parser.SFunction t   -> [BPop   :: compile_expr scope t]
| Parser.SIfThen p s   -> do
  {
    let then_code = compile_statement scope s;

      then_code
    @ [BCondJump (List.length then_code)
       :: compile_expr scope p]
  }
| Parser.SIfThenElse p s0 s1 -> do
  {
    let then_code = compile_statement scope s0;
    let else_code = compile_statement scope s1;

      else_code
    @ [BJump (List.length else_code)
       :: then_code]
    @ [BCondJump (List.length then_code + 1)
       :: compile_expr scope p]
  }
]
and compile_monad scope stmt = match stmt with
[ Parser.SEquation x y -> [BUnify :: compile_expr scope x @ compile_expr scope y]
| Parser.SFunction t   -> [BApply 1 :: compile_expr scope t]
| Parser.SIfThen p s   -> do
  {
    let then_code = compile_monad scope s;

      then_code
    @ [BCondJump (List.length then_code + 1)
       :: compile_expr scope p]
  }
| Parser.SIfThenElse p s0 s1 -> do
  {
    let then_code = compile_monad scope s0;
    let else_code = compile_monad scope s1;

      else_code
    @ [BJump (List.length else_code + 1)
       :: then_code]
    @ [BCondJump (List.length then_code + 2)
       :: compile_expr scope p]
  }
];

value rec compile_global_declarations scope decls = do
{
  let names = get_declared_variables decls;

  (* add the names to the global scope *)

  SymbolMap.iter
    (fun n _ ->
      Scope.add_global scope n Unbound)
    names;

  (* define the new symbols *)

  let fun_defs =
    SymbolMap.fold
      (fun name defs code -> match defs with
       [ [] -> code   (* a pattern, not a function *)
       | _  -> do
         {
           let cases =
             List.fold_left
               (fun cases d -> match d with
                [ Parser.DFun _ pats guard term -> [(pats, guard, term) :: cases]
                | _                             -> assert False
                ])
               []
               defs;
           let var = Scope.lookup_global scope name;

           [BUnify; BGlobal var :: compile_function scope cases @ code]
         }
       ])
      names
      [];

  iter fun_defs decls

  where rec iter code decls = match decls with
  [ []                               -> XList.rev_to_array code
  | [Parser.DFun _ _ _ _ :: ds]      -> iter code ds
  | [Parser.DPattern pat term :: ds] -> do
    {
      let (sd, nv, v, c)   = compile_pattern 0 pat;
      let (local_scope, _) = Scope.push scope v;

      let pat_code =
        List.fold_left
          (fun code var -> do
            {
              let v       = Scope.lookup_global scope      var;
              let (b1,b2) = Scope.lookup_local local_scope var;
              [BUnify; BGlobal v; BVariable b1 b2 :: code]
            })
            [ BMatch1 c sd nv 0 ]
            v;

      iter (pat_code @ compile_expr scope term @ code) ds
    }
  ]
};

value compile_declarations scope stream = do
{
  let lexer = Lexer.make_lexer
                (Scope.symbol_table scope)
                stream;
  let decls = Parser.parse_program lexer;

  compile_global_declarations scope decls
};

value compile_expression scope stream = do
{
  let lexer = Lexer.make_lexer
                (Scope.symbol_table scope)
                stream;
  let expr  = Parser.parse_expression lexer;

  Array.of_list (List.rev (compile_expr scope expr))
};


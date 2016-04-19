
open XNum;
open Runtime;
open Logging;
open VM;
open Typesetting;
open Engine;
open ParseState;
open ALCoding;
open ALDim;
open ALEnvironment;
open ALNodes;
open ALGraphics;

module UString     = Unicode.UString;
module SymbolTable = Unicode.SymbolTable;
module SymbolMap   = SymbolTable.SymbolMap;

value tracing_al_commands = ref False;

(* Opaque type for parse-states. *)

value apply_ps _ _ = Types.runtime_error "application of non-function";

value cmp_ps p1 p2 = p1 == p2;

value (ps_wrapper, ps_unwrapper) = Opaque.declare_type "parse-state" apply_ps cmp_ps cmp_ps;

value wrap_ps ps = Types.Opaque (ps_wrapper ps);

value unwrap_ps = Machine.evaluate_opaque "parse-state" ps_unwrapper;

value decode_ps = decode_opaque "parse-state" ps_unwrapper;

value execute_ps_command_unknown name f ps = do
{
  try
    ignore
      (decode_ps name
        (Machine.evaluate_function f [ref (wrap_ps ps)]))
  with
  [ VM.Types.Syntax_error loc msg -> log_warn loc (UString.to_string (Array.to_list msg))
  | VM.Types.Runtime_error msg    -> log_warn (location ps) (UString.to_string (Array.to_list msg))
  ]
};

value execute_ps_command name stream ps = do
{
  try
    ignore
      (decode_ps name
        (ref (Machine.evaluate_monad_expr ps.al_scope stream (wrap_ps ps))))
  with
  [ VM.Types.Syntax_error loc msg -> log_warn loc (UString.to_string (Array.to_list msg))
  | VM.Types.Runtime_error msg    -> log_warn (location ps) (UString.to_string (Array.to_list msg))
  ]
};

value dummy_parse_state = ParseState.create Job.empty;

value ps_cmd name parse_state f = do
{
  let ps = unwrap_ps name parse_state;

  if !tracing_al_commands then do
  {
    log_string "\n#AL: ";
    log_string name
  }
  else ();

  f ps;

  !parse_state
};

(* primitives *)

(* globals *)

value ps_get_global args = match args with
[ [var; sym; parse_command] -> do
  {
    ps_cmd "ps_get_global" parse_command
      (fun ps -> do
        {
          let s = decode_symbol "ps_get_global" sym;

          try
            Machine.set_unknown var (SymbolTable.SymbolMap.find s ps.global_variables)
          with
          [ Not_found -> () ];
        })
  }
| _ -> assert False
];

value ps_set_global args = match args with
[ [sym; val; parse_command] -> do
  {
    ps_cmd "ps_set_global" parse_command
      (fun ps -> do
        {
          let s = decode_symbol "ps_set_global" sym;

          ps.global_variables := SymbolTable.SymbolMap.add s !val ps.global_variables
        })
  }
| _ -> assert False
];

(* two helper functions *)

value set_num_global ps sym n = do
{
  ps.global_variables :=
    SymbolTable.SymbolMap.add
      (SymbolTable.string_to_symbol sym)
      (Types.Number n)
      ps.global_variables
};

value set_string_global ps sym str = do
{
  ps.global_variables :=
    SymbolTable.SymbolMap.add
      (SymbolTable.string_to_symbol sym)
      (Machine.uc_string_to_char_list str)
      ps.global_variables
};

(* stream commands *)

value ps_next_char c parse_command = do
{
  ps_cmd "ps_next_char" parse_command
    (fun ps -> do
      {
        Machine.set_unknown c (Types.Char (UCStream.next_char ps.input_stream))
      })
};

value ps_get_char args = match args with
[ [c; pos; parse_command] -> do
  {
    ps_cmd "ps_get_char" parse_command
      (fun ps -> do
        {
          let n = decode_int "ps_get_char" pos;

          Machine.set_unknown c (Types.Char (UCStream.get_char ps.input_stream n))
        })
  }
| _ -> assert False
];

value ps_remove_chars n parse_command = do
{
  ps_cmd "ps_remove_chars" parse_command
    (fun ps -> do
      {
        let k = decode_int "ps_remove_chars" n;

        UCStream.remove ps.input_stream k
      })
};

value ps_insert_string str parse_command = do
{
  ps_cmd "ps_insert_string" parse_command
    (fun ps -> do
      {
        let s  = Machine.decode_string "ps_insert_string" str;

        UCStream.insert_list ps.input_stream s
      })
};

value ps_location loc parse_command = do
{
  ps_cmd "ps_location" parse_command
    (fun ps -> do
      {
        Machine.set_unknown loc (encode_location (location ps))
      })
};

value ps_read_arg arg parse_command = do
{
  ps_cmd "ps_read_arg" parse_command
    (fun ps -> do
      {
        let str = Parser.read_argument ps.input_stream;

        Machine.set_unknown arg (Machine.uc_list_to_char_list str)
      })
};

value ps_arg_expanded arg parse_command = do
{
  ps_cmd "ps_arg_expanded" parse_command
    (fun ps -> do
      {
        let str = ParseArgs.arg_expanded ps;

        Machine.set_unknown arg (Machine.uc_list_to_char_list str)
      })
};

value ps_arg_execute args = match args with
[ [result; mode; parse_command] -> do
  {
    ps_cmd "ps_arg_execute" parse_command
      (fun ps -> do
        {
          let m = decode_mode "ps_arg_execute" mode;
          let n = ParseArgs.arg_execute ps m;

          Machine.set_unknown result (encode_node_list n)
        })
  }
| _ -> assert False
];

value ps_arg_num arg parse_command = do
{
  ps_cmd "ps_arg_num" parse_command
    (fun ps -> do
      {
        let n  = ParseArgs.arg_num ps;

        Machine.set_unknown arg (Types.Number n)
      })
};

value ps_arg_int arg parse_command = do
{
  ps_cmd "ps_arg_int" parse_command
    (fun ps -> do
      {
        let n  = ParseArgs.arg_int ps;

        Machine.set_unknown arg (Types.Number (num_of_int n))
      })
};

value ps_arg_skip arg parse_command = do
{
  ps_cmd "ps_arg_skip" parse_command
    (fun ps -> do
      {
        let s = ParseArgs.arg_skip ps;

        Machine.set_unknown arg (encode_skip_arg s)
      })
};

value ps_arg_dim arg parse_command = do
{
  ps_cmd "ps_arg_dim" parse_command
    (fun ps -> do
      {
        let d = ParseArgs.arg_dim ps;

        Machine.set_unknown arg (encode_dim_arg d)
      })
};

value ps_arg_key_val arg parse_command = do
{
  ps_cmd "ps_arg_key_val" parse_command
    (fun ps -> do
      {
        let kv = ParseArgs.arg_key_val ps;

        let code v = match v with
                     [ None   -> ref (Types.Symbol sym_None)
                     | Some x -> ref (Machine.uc_list_to_char_list x)
                     ];

        Machine.set_unknown arg
          (Types.Dictionary
            (DynUCTrie.fold
              (fun k v m -> SymbolMap.add (SymbolTable.string_to_symbol k) (code v) m)
              kv
              SymbolMap.empty))
      })
};

value ps_arg_dict args = match args with
[ [arg; dict; parse_command] -> do
  {
    ps_cmd "ps_arg_dict" parse_command
      (fun ps -> do
        {
          let d  = decode_dict "ps_arg_dict" dict;
          let kv = ParseArgs.arg_key_val ps;

          let key_map =
            SymbolMap.fold
              (fun sym key map ->
                DynUCTrie.add_list (Machine.decode_string "ps_arg_dict" key) sym map)
              d
              DynUCTrie.empty;

          let add_entry key val map = match DynUCTrie.lookup_string key key_map with
          [ Some sym -> do
            {
              let v = match val with
              [ None   -> ref (Types.Symbol sym_None)
              | Some x -> ref (Machine.uc_list_to_char_list x)
              ];
              SymbolMap.add sym v map
            }
          | None -> do
            {
              log_warn (location ps)
                ("Unknown key `" ^ (UString.bytes_to_string (Array.to_list key)) ^ "'!");
              map
            }
          ];

          Machine.set_unknown arg
            (Types.Dictionary
              (DynUCTrie.fold add_entry kv SymbolMap.empty))
        })
  }
| _ -> assert False
];

value ps_opt_expanded args = match args with
[ [arg; default; parse_command] -> do
  {
    ps_cmd "ps_opt_expanded" parse_command
      (fun ps -> do
        {
          let d  = Machine.decode_string "ps_opt_expanded" default;

          Machine.set_unknown arg (Machine.uc_list_to_char_list (ParseArgs.opt_expanded ps d))
        })
  }
| _ -> assert False
];

value ps_opt_key_val arg parse_command = do
{
  ps_cmd "ps_opt_key_val" parse_command
    (fun ps -> do
      {
        let kv = ParseArgs.opt_key_val ps;

        let code v = match v with
                     [ None   -> ref (Types.Symbol sym_None)
                     | Some x -> ref (Machine.uc_list_to_char_list x)
                     ];

        Machine.set_unknown arg
          (Types.Dictionary
            (DynUCTrie.fold
              (fun k v m -> SymbolMap.add (SymbolTable.string_to_symbol k) (code v) m)
              kv
              SymbolMap.empty))
      })
};

value ps_opt_int args = match args with
[ [arg; default; parse_command] -> do
  {
    ps_cmd "ps_opt_int" parse_command
      (fun ps -> do
        {
          let d = decode_int "ps_opt_int" default;

          Machine.set_unknown arg (Types.Number (num_of_int (ParseArgs.opt_int ps d)))
        })
  }
| _ -> assert False
];

value ps_arg_TeX_dim arg parse_command = do
{
  ps_cmd "ps_arg_TeX_dim" parse_command
    (fun ps -> do
      {
        let d = ParseArgs.arg_TeX_dim ps;

        Machine.set_unknown arg (encode_dim_arg d)
      })
};

(* modes and node-list *)

value ps_current_mode m parse_command = do
{
  ps_cmd "ps_current_mode" parse_command
    (fun ps -> do
      {
        Machine.set_unknown m (encode_mode (current_mode ps))
      })
};

value ps_open_node_list mode parse_command = do
{
  ps_cmd "ps_open_node_list" parse_command
    (fun ps -> do
      {
        open_node_list ps (decode_mode "ps_open_node_list" mode)
      })
};

value ps_close_node_list args = match args with
[ [result; mode; parse_command] -> do
  {
    ps_cmd "ps_close_node_list" parse_command
      (fun ps -> do
        {
          let nodes = close_node_list ps (decode_mode "ps_close_node_list" mode);

          Machine.set_unknown result (encode_node_list nodes)
        })
  }
| _ -> assert False
];

value ps_add_node node parse_command = do
{
  ps_cmd "ps_add_node" parse_command
    (fun ps -> do
      {
        add_node ps (decode_node "ps_add_node" node)
      })
};

(* commands *)

value decode_command name execute expand = do
{
  let exe ps = execute_ps_command_unknown name execute ps;
  let exp ps tok = try do
    {
      let result  = ref Types.Unbound;
      let command = Machine.evaluate_function
                      expand
                      [result;
                       ref ((Machine.uc_list_to_char_list tok));
                       ref (wrap_ps ps)];

      decode_ps name command;

      Machine.decode_string name result
    }
    with
    [ VM.Types.Syntax_error loc msg -> do
      {
        log_warn loc (UString.to_string (Array.to_list msg));
        []
      }
    | VM.Types.Runtime_error msg -> do
      {
        log_warn (location ps) (UString.to_string (Array.to_list msg));
        []
      }
    ];

  { execute = exe; expand = exp }
};

value decode_unexpandable_command name execute = do
{
  { execute = execute_ps_command_unknown name execute;
    expand  = Macro.noexpand }
};

value ps_set_default_char_cmd args = match args with
[ [execute; expand; parse_command] -> do
  {
    ps_cmd "ps_set_default_char_cmd" parse_command
      (fun ps -> match !expand with
        [ Types.Symbol s -> do
          {
            if s = sym_None then
              set_default_char_cmd ps
                (decode_unexpandable_command "default_char_cmd" execute)
            else
              log_warn (location ps) "ps_set_default_char_cmd: None or a function expected"
          }
        | _ -> do
          {
            set_default_char_cmd ps
              (decode_command "default_char_cmd" execute expand)
          }
        ])
  }
| _ -> assert False
];

value ps_define_command args = match args with
[ [name; execute; expand; parse_command] -> do
  {
    ps_cmd "ps_define_command" parse_command
      (fun ps -> do
        {
          let name_uc  = Machine.decode_string "ps_define_command" name;
          let name_str = UString.to_string name_uc;

          match !expand with
          [ Types.Symbol s -> do
            {
              if s = sym_None then
                define_command ps name_uc
                  (decode_unexpandable_command name_str execute)
              else
                log_warn (location ps) "ps_define_command: None or a function expected"
            }
          | _ -> do
            {
              define_command ps name_uc
                (decode_command name_str execute expand)
            }
          ]
        })
  }
| _ -> assert False
];

value ps_define_pattern args = match args with
[ [name; execute; expand; parse_command] -> do
  {
    ps_cmd "ps_define_pattern" parse_command
      (fun ps -> do
        {
          let name_uc  = Machine.decode_string "ps_define_pattern" name;
          let name_str = UString.to_string name_uc;

          match !expand with
          [ Types.Symbol s -> do
            {
              if s = sym_None then
                define_pattern ps name_uc
                  (decode_unexpandable_command name_str execute)
              else
                log_warn (location ps) "ps_define_pattern: None or a function expected"
            }
          | _ -> do
            {
              define_pattern ps name_uc
                (decode_command name_str execute expand)
            }
          ]
        })
  }
| _ -> assert False
];

value decode_arg_templ name args = do
{
  iter (Machine.decode_list name args)

  where rec iter args = match args with
  [ []      -> []
  | [a::aa] -> match !a with
    [ Types.Symbol s -> do
      {
        if s = sym_Mandantory then
          [ Macro.Arg :: iter aa ]
        else if s = sym_Optional then
          [ Macro.Opt (UString.of_ascii "\\NoValue") :: iter aa ]
        else if s = sym_Bool then
          [ Macro.Bool :: iter aa ]
        else
          Types.runtime_error ("unknown argument specifier " ^ UString.to_string (Array.to_list (SymbolTable.symbol_to_string s)))
      }
    | Types.Tuple x -> match x with
      [ [| y; z |] -> match !y with
          [ Types.Symbol s -> do
            {
              if s = sym_Optional then
                [ Macro.Opt (Machine.decode_string name z) :: iter aa ]
              else
                Types.runtime_error (name ^ ": unknown argument specifier "
                                          ^ UString.to_string (Array.to_list (SymbolTable.symbol_to_string s)))
            }
          | _ -> Types.runtime_error (name ^ ": symbol expected but got " ^ Types.type_name !y)
          ]
      | _ -> Types.runtime_error (name ^ ": pair expected")
      ]
    | _ -> Types.runtime_error (name ^ ": symbol expected but got " ^ Types.type_name !a)
    ]
  ]
};

value ps_define_macro args = match args with
[ [name; arg_template; body; parse_command] -> do
  {
    ps_cmd "ps_define_macro" parse_command
      (fun ps -> do
        {
          let name = Machine.decode_string "ps_define_macro" name;
          let body = Machine.decode_string "ps_define_macro" body;
          let args = decode_arg_templ "ps_define_macro" arg_template;

          define_command ps name
            { execute = Macro.execute_macro args body;
              expand  = Macro.expand_macro  args body }
            })
  }
| _ -> assert False
];

value ps_save_command name parse_command = do
{
  ps_cmd "ps_save_command" parse_command
    (fun ps -> do
      {
        let n = Machine.decode_string "ps_save_command" name;

        save_command ps n
      })
};

value ps_restore_command name parse_command = do
{
  ps_cmd "ps_restore_command" parse_command
    (fun ps -> do
      {
        let n = Machine.decode_string "ps_restore_command" name;

        restore_command ps n
      })
};

value ps_save_pattern name parse_command = do
{
  ps_cmd "ps_save_pattern" parse_command
    (fun ps -> do
      {
        let n = Machine.decode_string "ps_save_pattern" name;

        save_pattern ps n
      })
};

value ps_restore_pattern name parse_command = do
{
  ps_cmd "ps_restore_pattern" parse_command
    (fun ps -> do
      {
        let n = Machine.decode_string "ps_restore_pattern" name;

        restore_pattern ps n
      })
};

value encode_command name command = do
{
  let execute parse_command = do
    {
      command.execute (decode_ps name parse_command);

      !parse_command
    };
  let expand args = match args with
  [ [result; tok; parse_command] -> do
    {
      let ps = decode_ps name parse_command;

      let t = Machine.decode_string name tok;

      Machine.set_unknown result (Machine.uc_list_to_char_list (command.expand ps t));

      !parse_command;
    }
  | _ -> assert False
  ];

  Types.Tuple
    [|ref (Types.Primitive1 execute);
      ref (Types.PrimitiveN 3 expand)|]
};

value ps_lookup_command args = match args with
[ [command; name; parse_command] -> do
  {
    ps_cmd "ps_lookup_command" parse_command
      (fun ps -> do
        {
          let name_uc  = Machine.decode_string "ps_lookup_command" name;
          let name_str = UString.to_string name_uc;

          match lookup_command ps name_uc with
          [ None   -> Machine.set_unknown command (Types.Symbol sym_None)
          | Some c -> Machine.set_unknown command (encode_command name_str c)
          ]
        })
  }
| _ -> assert False
];

value ps_push_env args = match args with
[ [name; arg; parse_command] -> do
  {
    ps_cmd "ps_push_env" parse_command
      (fun ps -> do
        {
          let n    = Machine.decode_string "ps_push_env" name;
          let args = List.map
                       (Machine.decode_string "ps_push_env")
                       (Machine.decode_list "ps_push_env" arg);

          push_env ps n args
        })
  }
| _ -> assert False
];

value ps_pop_env args = match args with
[ [name; arg; parse_command] -> do
  {
    ps_cmd "ps_pop_env" parse_command
      (fun ps -> do
        {
          let (n,args) = pop_env ps;

          Machine.set_unknown name (Machine.uc_list_to_char_list n);
          Machine.set_unknown arg
            (List.fold_right
              (fun a l -> Types.List
                            (ref (Machine.uc_list_to_char_list a))
                            (ref l))
              args
              Types.Nil)
        })
  }
| _ -> assert False
];

value ps_set_env_args args parse_command = do
{
  ps_cmd "ps_set_env_args" parse_command
    (fun ps -> do
      {
        let a = List.map
                  (Machine.decode_string "ps_set_env_args")
                  (Machine.decode_list "ps_set_env_args" args);

        set_env_args ps a
      })
};

value ps_top_env args = match args with
[ [name; arg; parse_command] -> do
  {
    ps_cmd "ps_top_env" parse_command
      (fun ps -> do
        {
          let (n,args) = top_env ps;

          Machine.set_unknown name (Machine.uc_list_to_char_list n);
          Machine.set_unknown arg
            (List.fold_right
              (fun a l -> Types.List
                            (ref (Machine.uc_list_to_char_list a))
                            (ref l))
              args
              Types.Nil)
        })
  }
| _ -> assert False
];

value ps_lookup_env args = match args with
[ [commands; name; parse_command] -> do
  {
    ps_cmd "ps_lookup_env" parse_command
      (fun ps -> do
        {
          let name_uc  = Machine.decode_string "ps_lookup_env" name;
          let name_str = UString.to_string name_uc;

          match lookup_env ps name_uc with
          [ None       -> Machine.set_unknown commands (Types.Symbol sym_None)
          | Some (b,e) -> Machine.set_unknown commands
                            (Types.Tuple [|ref (encode_command name_str b);
                                           ref (encode_command name_str e)|])
          ]
        })
  }
| _ -> assert False
];

value ps_define_env args = match args with
[ [name; execute_begin; expand_begin; execute_end; expand_end; parse_command] -> do
  {
    ps_cmd "ps_define_env" parse_command
      (fun ps -> do
        {
          let name_uc  = Machine.decode_string "ps_define_env" name;
          let name_str = UString.to_string name_uc;

          match (!expand_begin, !expand_end) with
          [ (Types.Symbol s1, Types.Symbol s2) -> do
            {
              if s1 = sym_None && s2 = sym_None then
                define_env ps name_uc
                  (decode_unexpandable_command name_str execute_begin)
                  (decode_unexpandable_command name_str execute_end)
              else
                log_warn (location ps) "ps_define_env: None or a function expected"
            }
          | (Types.Symbol s, _) -> do
            {
              if s = sym_None then
                define_env ps name_uc
                  (decode_unexpandable_command name_str execute_begin)
                  (decode_command              name_str execute_end expand_end)
              else
                log_warn (location ps) "ps_define_env: None or a function expected"
            }
          | (_, Types.Symbol s) -> do
            {
              if s = sym_None then
                define_env ps name_uc
                  (decode_command              name_str execute_begin expand_begin)
                  (decode_unexpandable_command name_str execute_end)
              else
                log_warn (location ps) "ps_define_env: None or a function expected"
            }
          | _ -> do
            {
              define_env ps name_uc
                (decode_command name_str execute_begin expand_begin)
                (decode_command name_str execute_end   expand_end)
            }
          ]
        })
  }
| _ -> assert False
];

(* page layout *)

value ps_shipout_pages args = match args with
[ [number; even; odd; parse_command] -> do
  {
    ps_cmd "ps_shipout_pages" parse_command
      (fun ps -> do
        {
          let n        = decode_int "ps_shipout_pages" number;
          let even_str = Array.of_list (Machine.decode_string "ps_shipout_pages" even);
          let odd_str  = Array.of_list (Machine.decode_string "ps_shipout_pages" odd);

          add_node ps (Node.ShipOut (location ps) even_str odd_str (max 0 n))
        })
  }
| _ -> assert False
];

value ps_new_page_layout args = match args with
[ [name; width; height; parse_command] -> do
  {
    ps_cmd "ps_new_page_layout" parse_command
      (fun ps -> do
        {
          let name_str = Array.of_list (Machine.decode_string "ps_new_page_layout" name);
          let w        = Machine.decode_num "ps_new_page_layout" width;
          let h        = Machine.decode_num "ps_new_page_layout" height;

          add_node ps
            (Node.NewLayout (location ps)
               name_str
               (fun _ -> w)
               (fun _ -> h))
        })
  }
| _ -> assert False
];

value encode_page_info pi = do
{
  Types.Dictionary
    (SymbolMap.add
      sym_Width    (ref (Types.Number pi.Box.pi_width))
    (SymbolMap.add
      sym_Height   (ref (Types.Number pi.Box.pi_height))
    (SymbolMap.add
      sym_PageNo   (ref (Types.Number (num_of_int pi.Box.pi_page_no)))
    (SymbolMap.add
      sym_OldMarks (ref (List.fold_right
                          (fun (a,b) c ->
                              Types.List
                                (ref (Types.Tuple
                                       [|ref (Machine.uc_string_to_char_list a);
                                         ref (Machine.uc_string_to_char_list b)|]))
                                (ref c))
                          pi.Box.pi_old_marks
                          Types.Nil))
    (SymbolMap.add
      sym_NewMarks (ref (List.fold_right
                          (fun (a,b) c ->
                              Types.List
                                (ref (Types.Tuple
                                       [|ref (Machine.uc_string_to_char_list a);
                                         ref (Machine.uc_string_to_char_list b)|]))
                                (ref c))
                          pi.Box.pi_new_marks
                          Types.Nil))
    SymbolMap.empty)))))
};

value ps_new_area args = match args with
[ [name; pos_x; pos_y; width; height; max_top; max_bot; area_type; param; parse_command] -> do
  {
    ps_cmd "ps_new_area" parse_command
      (fun ps -> do
      {
        let name_str = Array.of_list (Machine.decode_string "ps_new_area" name);
        let x        = Machine.decode_num "ps_new_area" pos_x;
        let y        = Machine.decode_num "ps_new_area" pos_y;
        let w        = Machine.decode_num "ps_new_area" width;
        let h        = Machine.decode_num "ps_new_area" height;
        let t        = Machine.decode_num "ps_new_area" max_top;
        let b        = Machine.decode_num "ps_new_area" max_bot;
        let at       = decode_symbol      "ps_new_area" area_type;

        if at = sym_Galley then do
        {
          let ap = decode_dict "ps_new_area" param;

          add_node ps
            (Node.NewArea (location ps)
               name_str (fun _ -> x) (fun _ -> y) (fun _ -> w) (fun _ -> h) (fun _ -> t) (fun _ -> b)
               (`Galley
                 (Option.from_option [||]
                    (lookup_string  "ps_new_area" ap sym_Name),
                  Option.from_option (Evaluate.const_em num_one)
                     (lookup_skip   "ps_new_area" ap sym_TopSkip),
                  Option.from_option (Evaluate.const_em num_one)
                     (lookup_skip   "ps_new_area" ap sym_BottomSkip),
                  Option.from_option (Evaluate.const_em (num_of_int 5))
                     (lookup_skip   "ps_new_area" ap sym_MinSize),
                  Option.from_option (Evaluate.const_em num_one)
                     (lookup_skip   "ps_new_area" ap sym_GridSize))))
        }
        else if at = sym_Float then do
        {
          let ap    = decode_dict "ps_new_area" param;
          let align = match lookup_symbol "ps_new_area" ap sym_Alignment with
            [ None   -> FloatVertical.Top
            | Some s -> if s = sym_Bottom then
                          FloatVertical.Bottom
                        else
                          FloatVertical.Top
            ];

          add_node ps
            (Node.NewArea (location ps)
               name_str (fun _ -> x) (fun _ -> y) (fun _ -> w) (fun _ -> h) (fun _ -> t) (fun _ -> b)
               (`Float
                 (align,
                  Option.from_option (Evaluate.const_em num_one)
                    (lookup_skip    "ps_new_area" ap sym_TopSkip),
                  Option.from_option (Evaluate.const_em num_one)
                    (lookup_skip    "ps_new_area" ap sym_BottomSkip),
                  Option.from_option (Evaluate.const_fixed_dim (Evaluate.const_em num_one))
                    (lookup_dim     "ps_new_area" ap sym_FloatSep))))
        }
        else if at = sym_Footnote then do
        {
          let lookup_dict dict key = try
            SymbolMap.find key dict
          with
          [ Not_found -> ref (Types.Dictionary SymbolMap.empty) ];

          let ap                = decode_dict "ps_new_area" param;
          let line_params       = lookup_dict ap sym_LineParams;
          let par_params        = lookup_dict ap sym_ParParams;
          let line_break_params = lookup_dict ap sym_LineBreakParams;
          let hyphen_params     = lookup_dict ap sym_HyphenParams;
          let space_params      = lookup_dict ap sym_SpaceParams;
          let math_params       = lookup_dict ap sym_MathParams;

          add_node ps
            (Node.NewArea (location ps)
               name_str (fun _ -> x) (fun _ -> y) (fun _ -> w) (fun _ -> h) (fun _ -> t) (fun _ -> b)
               (`Footnote
                 (Option.from_option []
                    (lookup (decode_node_list "ps_new_area") ap sym_Separator),
                  Option.from_option (Evaluate.const_em num_one)
                    (lookup_skip "ps_new_area" ap sym_TopSkip),
                  Option.from_option (Evaluate.const_em num_one)
                    (lookup_skip "ps_new_area" ap sym_BottomSkip),
                  Option.from_option (Evaluate.const_em num_one)
                    (lookup_skip "ps_new_area" ap sym_GridSize),
                  decode_line_params "ps_new_area" line_params,
                  decode_par_params "ps_new_area" par_params,
                  decode_line_break_params "ps_new_area" line_break_params,
                  decode_hyphen_params "ps_new_area" hyphen_params,
                  decode_space_params "ps_new_area" space_params,
                  decode_math_params "ps_new_area" math_params)))
        }
        else if at = sym_Direct then match !param with
        [ Types.Nil
        | Types.List _ _ -> do
          {
            (* <param> is a string with ant code. *)

            let code       = Machine.decode_string "ps_new_area" param;
            let current_ps = duplicate ps;
            let stream     = UCStream.of_list code;

            let f pi _ = do
            {
              UCStream.assign current_ps.input_stream stream;

              set_counter current_ps (UString.uc_string_of_ascii "page") pi.Box.pi_page_no;

              List.iter
                (fun (m,v) -> do
                  {
                    set_string_global current_ps
                      (Array.of_list (UString.of_ascii "OldMark" @ Array.to_list m))
                      v;
                    set_string_global current_ps
                      (Array.of_list (UString.of_ascii "NewMark" @ Array.to_list m))
                      v
                  })
                (List.rev pi.Box.pi_old_marks);
              List.iter
                (fun (m,v) ->
                    set_string_global current_ps
                      (Array.of_list (UString.of_ascii "NewMark" @ Array.to_list m))
                      v
                )
                (List.rev pi.Box.pi_new_marks);

              run_parser current_ps `VBox;
            };

            add_node ps
              (Node.NewArea (location ps)
                 name_str (fun _ -> x) (fun _ -> y) (fun _ -> w) (fun _ -> h) (fun _ -> t) (fun _ -> b)
                 (`Direct f))
          }
        | _ -> do
          {
            (* <param> is a function that returns a node list. *)

            let f pi (x,y) = do
            {
              decode_node_list
                "<anonymous>"
                (Machine.evaluate_function
                  param
                  [ref (encode_page_info pi);
                   ref (Types.Tuple [|ref (Types.Number x); ref (Types.Number y)|])])
            };

            add_node ps
              (Node.NewArea (location ps)
                 name_str (fun _ -> x) (fun _ -> y) (fun _ -> w) (fun _ -> h) (fun _ -> t) (fun _ -> b)
                 (`Direct f))
          }
        ]
        else do
        {
          log_warn (location ps) "unknown area type ";
          log_uc_string (SymbolTable.symbol_to_string at);
          log_string "!\n"
        }
        })
  }
| _ -> assert False
];

value ps_new_galley args = match args with
[ [name; measure; parse_command] -> do
  {
    ps_cmd "ps_new_galley" parse_command
      (fun ps -> do
        {
          let name_str = Array.of_list (Machine.decode_string "ps_new_galley" name);
          let m        = Machine.decode_num "ps_new_galley" measure;

          add_node ps
            (Node.NewGalley (location ps) name_str (fun _ -> m))
        })
  }
| _ -> assert False
];

(* fonts *)

value decode_glyph_spec name g = match !g with
[ Types.Number _ -> FontMetric.GlyphIndex (decode_int name g)
| Types.Char c   -> FontMetric.GlyphChar c
| Types.List _ _ -> FontMetric.GlyphName (UString.to_string (Array.to_list (decode_uc_string name g)))
| _              -> Types.runtime_error (name ^ ": invalid glyph specification")
];

value decode_extra_kern name k = do
{
  let x = decode_tuple name k;
  let n = Array.length x;

  if n < 2 || n > 7 then
    Types.runtime_error (name ^ ": invalid border kern data")
  else
    (decode_glyph_spec name x.(0),
     {
       GlyphMetric.ki_after_margin   = if n < 2 then num_zero else Machine.decode_num name x.(1);
       GlyphMetric.ki_before_margin  = if n < 3 then num_zero else Machine.decode_num name x.(2);
       GlyphMetric.ki_after_foreign  = if n < 4 then num_zero else Machine.decode_num name x.(3);
       GlyphMetric.ki_before_foreign = if n < 5 then num_zero else Machine.decode_num name x.(4);
       GlyphMetric.ki_after_space    = if n < 6 then num_zero else Machine.decode_num name x.(5);
       GlyphMetric.ki_before_space   = if n < 7 then num_zero else Machine.decode_num name x.(6)
     })
};

value decode_font_load_params name params = do
{
  let get_glyph g = if g < 0 then Substitute.Undef else Substitute.Simple g;

  let p = decode_dict name params;

  let encoding      = Array.map
                        (decode_uc_string name)
                        (Option.from_option [||]
                          (lookup_tuple name p sym_Encoding));
  let hyphen        = Option.from_option (-1)
                       (lookup_int name p sym_HyphenGlyph);
  let skew          = Option.from_option (-1)
                       (lookup_int name p sym_SkewGlyph);
  let scale         = Option.from_option num_one
                       (lookup_num name p sym_Scale);
  let letterspacing = Option.from_option num_zero
                       (lookup_num name p sym_LetterSpacing);
  let adjustments   = Option.from_option []
                       (lookup_list name p sym_Adjustments);
  let auto_lig      = Option.from_option False
                       (lookup_bool name p sym_AutoLigatures);
  let extra_kern    = List.map
                        (decode_extra_kern name)
                        (Option.from_option []
                          (lookup_list name p sym_BorderKern));

  let ligs = if auto_lig then do
    {
      let rec lookup_glyph i char = do
      {
        if i >= Array.length encoding then
          FontMetric.GlyphIndex (-1)
        else if encoding.(i) = [|char|] then
          FontMetric.GlyphIndex i
        else
          lookup_glyph (i+1) char
      };

      iter 0 FontMetric.GlyphSpecTrie.empty

      where rec iter i l = do
      {
        if i >= Array.length encoding then
          l
        else do
        {
          let n = Array.length encoding.(i);

          if n > 1 then
            iter (i+1) (FontMetric.GlyphSpecTrie.add_array
                         (Array.map (lookup_glyph 0) encoding.(i))
                         (FontMetric.AdjLig (FontMetric.GlyphIndex i))
(*                         (Substitute.replace_with_single_glyph_cmd n (Substitute.Simple i), 0) *)
                         l)
          else
            iter (i+1) l
        }
      }
    }
    else
      FontMetric.GlyphSpecTrie.empty;

  iter FontMetric.GlyphSpecTrie.empty ligs adjustments

  where rec iter extra_pos extra_subst adjustments = match adjustments with
  [ [] -> {
            FontMetric.flp_encoding       = encoding;
            FontMetric.flp_letter_spacing = letterspacing;
            FontMetric.flp_size           = scale;
            FontMetric.flp_hyphen_glyph   = get_glyph hyphen;
            FontMetric.flp_skew_glyph     = get_glyph skew;
            FontMetric.flp_extra_kern     = extra_kern;
            FontMetric.flp_extra_pos      = extra_pos;
            FontMetric.flp_extra_subst    = extra_subst
          }
  | [a::adjs] -> match decode_tuple name a with
    [ [| glyphs; sym; val |] -> do
      {
        let gs = List.map
                   (decode_glyph_spec name)
                   (Machine.decode_list name glyphs);
        let s  = decode_symbol name sym;

        if s = sym_Kern then do
        {
          let v = Machine.decode_num name val;
          iter
            (FontMetric.GlyphSpecTrie.add_list gs (FontMetric.AdjKern v) extra_pos)
            extra_subst
            adjs
(*          let c = Substitute.simple_pair_kerning_cmd v;

          iter (DynUCTrie.add_list gs (c, 1) extra_pos) extra_subst adjs *)
        }
        else if s = sym_Ligature then do
        {
          let v = decode_glyph_spec name val;
          iter
            extra_pos
            (FontMetric.GlyphSpecTrie.add_list gs (FontMetric.AdjLig v) extra_pos)
            adjs
(*          let c = Substitute.replace_with_single_glyph_cmd
                    2 (Substitute.Simple v);

          iter extra_pos (DynUCTrie.add_list gs (c, 0) extra_subst) adjs*)
        }
        else
          Types.runtime_error (name ^ ": unknown adjustment command, Kern or Ligature expected")
      }
    | _ -> Types.runtime_error (name ^ ": triple expected")
    ]
  ]
};

value ps_declare_font args = match args with
[ [name; family; series; shape; sizes; params; parse_command] -> do
  {
    ps_cmd "ps_declare_font" parse_command
      (fun ps -> do
        {
          let n       = decode_uc_string "ps_declare_font" name;
          let fam     = decode_uc_string "ps_declare_font" family;
          let ser     = decode_uc_string "ps_declare_font" series;
          let sha     = decode_uc_string "ps_declare_font" shape;
          let (s1,s2) = match decode_tuple "ps_declare_font" sizes with
            [ [| s1; s2 |] -> (s1,s2)
            | _ -> Types.runtime_error "ps_declare_font: pair expected"
            ];
          let flp = decode_font_load_params "ps_declare_font" params;

          add_node ps
            (Node.Command (location ps)
              (Environment.declare_font
                n fam ser sha
                (Machine.decode_num "ps_declare_font" s1,
                 Machine.decode_num "ps_declare_font" s2)
                flp))
        })
  }
| _ -> assert False
];

value ps_define_math_symbol args = match args with
[ [name; math_code; font; glyph; parse_command] -> do
  {
    ps_cmd "ps_define_math_symbol" parse_command
      (fun ps -> do
        {
          let name = Machine.decode_string "ps_define_math_symbol" name;
          let mc   = decode_math_code      "ps_define_math_symbol" math_code;
          let f    = decode_int            "ps_define_math_symbol" font;
          let g    = decode_int            "ps_define_math_symbol" glyph;

          define_command ps name
            { execute = (fun ps -> add_node ps
                                                (Node.MathChar (location ps) (mc, (f, f), (g, g))));
              expand  = Macro.noexpand }
        })
  }
| _ -> assert False
];

value ps_define_root_symbol args = match args with
[ [name; small_font; small_glyph; large_font; large_glyph; parse_command] -> do
  {
    ps_cmd "ps_define_root_symbol" parse_command
      (fun ps -> do
        {
          let name = Machine.decode_string "ps_define_root_symbol" name;
          let sf   = decode_int            "ps_define_root_symbol" small_font;
          let sg   = decode_int            "ps_define_root_symbol" small_glyph;
          let lf   = decode_int            "ps_define_root_symbol" large_font;
          let lg   = decode_int            "ps_define_root_symbol" large_glyph;

          define_command ps name
            { execute = (fun ps -> add_node ps
                                                (Node.Root (location ps) sf sg lf lg (ParseArgs.arg_execute ps `Math)));
              expand  = Macro.noexpand }
        })
  }
| _ -> assert False
];

value ps_define_math_accent args = match args with
[ [name; font; glyph; parse_command] -> do
  {
    ps_cmd "ps_define_math_accent" parse_command
      (fun ps -> do
        {
          let name = Machine.decode_string "ps_define_math_accent" name;
          let f    = decode_int            "ps_define_math_accent" font;
          let g    = decode_int            "ps_define_math_accent" glyph;

          define_command ps name
            { execute = (fun ps -> add_node ps
                                     (Node.MathAccent (location ps) f g (ParseArgs.arg_execute ps `Math)));
              expand  = Macro.noexpand }
        })
  }
| _ -> assert False
];

value ps_set_math_code args = match args with
[ [char; math_code; font1; glyph1; font2; glyph2; parse_command] -> do
  {
    ps_cmd "ps_set_math_code" parse_command
      (fun ps -> do
        {
          let c  = decode_char      "ps_set_math_code" char;
          let mc = decode_math_code "ps_set_math_code" math_code;
          let f1 = decode_int       "ps_set_math_code" font1;
          let g1 = decode_int       "ps_set_math_code" glyph1;
          let f2 = decode_int       "ps_set_math_code" font2;
          let g2 = decode_int       "ps_set_math_code" glyph2;

          set_math_code ps c mc f1 g1 f2 g2
        })
  }
| _ -> assert False
];

(* boxes *)

(* graphics *)

value decode_coord name z = match !z with
[ Types.Tuple [|x;y|] -> (Machine.decode_num name x,
                          Machine.decode_num name y)
| _                   -> Types.runtime_error (name ^ ": pair expected but got " ^ Types.type_name !z)
];

value decode_bezier name z = match !z with
[ Types.Tuple [|a;b;c;d|] -> do
  {
    let (ax,ay) = decode_coord name a;
    let (bx,by) = decode_coord name b;
    let (cx,cy) = decode_coord name c;
    let (dx,dy) = decode_coord name d;

    (fun _ -> Dim.fixed_dim ax, fun _ -> Dim.fixed_dim ay,
     fun _ -> Dim.fixed_dim bx, fun _ -> Dim.fixed_dim by,
     fun _ -> Dim.fixed_dim cx, fun _ -> Dim.fixed_dim cy,
     fun _ -> Dim.fixed_dim dx, fun _ -> Dim.fixed_dim dy)
  }
| _ -> Types.runtime_error (name ^ ": 4-tuple expected but got " ^ Types.type_name !z)
];

value decode_path name p = do
{
  List.map (decode_bezier name)
    (Machine.decode_list name p)
};

value ps_set_colour colour parse_command = do
{
  ps_cmd "ps_set_colour" parse_command
    (fun ps -> do
      {
        let c = decode_colour "ps_set_colour" colour;

        add_node ps
          (Node.GfxCommand (location ps) (Graphic.SetColour c))
      })
};

value ps_set_bg_colour colour parse_command = do
{
  ps_cmd "ps_set_bg_colour" parse_command
    (fun ps -> do
      {
        let c = decode_colour "ps_set_bg_colour" colour;

        add_node ps
          (Node.GfxCommand (location ps) (Graphic.SetBgColour c))
      })
};

value ps_set_alpha alpha parse_command = do
{
  ps_cmd "ps_set_alpha" parse_command
    (fun ps -> do
      {
        let a = Machine.decode_num "ps_set_alpha" alpha;

        add_node ps
          (Node.GfxCommand (location ps) (Graphic.SetAlpha a))
      })
};

value ps_draw name mode path parse_command = do
{
  ps_cmd "ps_draw" parse_command
    (fun ps -> do
      {
        let p = decode_path name path;

        add_node ps
          (Node.GfxCommand (location ps) (Graphic.Draw mode p))
      })
};

value ps_set_line_width width parse_command = do
{
  ps_cmd "ps_set_line_width" parse_command
    (fun ps -> do
      {
        let w = Machine.decode_num "ps_set_line_width" width;

        add_node ps
          (Node.GfxCommand (location ps) (Graphic.SetLineWidth w))
      })
};

value ps_set_line_cap cap parse_command = do
{
  ps_cmd "ps_set_line_cap" parse_command
    (fun ps -> do
      {
        let c = decode_line_cap "ps_set_line_cap" cap;

        add_node ps
          (Node.GfxCommand (location ps) (Graphic.SetLineCap c))
      })
};

value ps_set_line_join join parse_command = do
{
  ps_cmd "ps_set_line_join" parse_command
    (fun ps -> do
      {
        let j = decode_line_join "ps_set_line_join" join;

        add_node ps
          (Node.GfxCommand (location ps) (Graphic.SetLineJoin j))
      })
};

value ps_set_miter_limit limit parse_command = do
{
  ps_cmd "ps_set_miter_limit" parse_command
    (fun ps -> do
      {
        let l = Machine.decode_num "ps_set_miter_limit" limit;

        add_node ps
          (Node.GfxCommand (location ps) (Graphic.SetMiterLimit l))
      })
};

(* page commands *)

value ps_page_command cmd parse_command = do
{
  ps_cmd "ps_page_command" parse_command
    (fun ps -> do
      {
        let f pi (x,y) = do
        {
          execute_ps_command_unknown "ps_page_command"
            (ref (Types.Application !cmd 1
                  [ref (encode_page_info pi);
                   ref (Types.Tuple [|ref (Types.Number x);
                                      ref (Types.Number y)|])]))
            ps
        };

        add_node ps
          (Node.CommandBox (location ps) (`PageCmd (Box.CallPageFunction f)))
      })
};

value ps_par_command cmd parse_command = do
{
  ps_cmd "ps_par_command" parse_command
    (fun ps -> do
      {
        let f line = do
        {
          execute_ps_command_unknown "ps_par_command"
            (ref (Types.Application !cmd 1 [ref (Types.Number (num_of_int line))]))
            ps
        };

        add_node ps
          (Node.CommandBox (location ps) (`ParCmd (Box.CallParFunction f)))
      })
};

value ps_dvi_special special parse_command = do
{
  ps_cmd "ps_dvi_special" parse_command
    (fun ps -> do
      {
        let s = Machine.decode_string "ps_dvi_special" special;

        add_node ps
          (Node.CommandBox
            (location ps)
            (`Special (`DVI_Special (UString.to_string s))))
      })
};

(* counters *)

value ps_new_counter args = match args with
[ [name; val; super; parse_command] -> do
  {
    ps_cmd "ps_new_counter" parse_command
      (fun ps -> do
        {
          let n = decode_uc_string "ps_new_counter" name;
          let v = decode_int       "ps_new_counter" val;
          let s = decode_option    "ps_new_counter" decode_uc_string super;

          new_counter ps n v s
        })
  }
| _ -> assert False
];

value ps_get_counter args = match args with
[ [val; name; parse_command] -> do
  {
    ps_cmd "ps_get_counter" parse_command
      (fun ps -> do
        {
          let n = decode_uc_string "ps_get_counter" name;

          Machine.set_unknown val (Types.Number (num_of_int (get_counter ps n)))
        })
  }
| _ -> assert False
];

value ps_set_counter args = match args with
[ [name; val; parse_command] -> do
  {
    ps_cmd "ps_set_counter" parse_command
      (fun ps -> do
        {
          let n = decode_uc_string "ps_set_counter" name;
          let v = decode_int       "ps_set_counter" val;

          set_counter ps n v
        })
  }
| _ -> assert False
];

(* misc *)

value ps_warning msg parse_command = do
{
  ps_cmd "ps_warning" parse_command
    (fun ps -> do
      {
        let s = Machine.decode_string "ps_warning" msg;

        log_warn (location ps) (UString.to_string s)
      })
};

value ps_error msg parse_command = do
{
  ps_cmd "ps_error" parse_command
    (fun ps -> do
      {
        let s = Machine.decode_string "ps_error" msg;

        log_error (location ps) (UString.to_string s)
      })
};


(* running the parser *)

value ps_execute_next_char finished parse_command = do
{
  ps_cmd "ps_execute_next_char" parse_command
    (fun ps -> do
      {
        Machine.set_unknown finished (Types.Bool (execute_next_char ps))
      })
};

value ps_execute_stream string parse_command = do
{
  ps_cmd "ps_execute_stream" parse_command
    (fun ps -> do
      {
        let str = Machine.decode_string "ps_execute_stream" string;

        execute_stream ps (UCStream.of_list str)
      })
};

value ps_execute_argument parse_command = do
{
  ps_cmd "ps_execute_argument" parse_command
    (fun ps -> do
      {
        execute_argument ps
      })
};

value ps_run_parser args = match args with
[ [result; mode; parse_command] -> do
  {
    ps_cmd "ps_run_parser" parse_command
      (fun ps -> do
        {
          let m = decode_mode "ps_run_parser" mode;

          Machine.set_unknown result (encode_node_list (run_parser ps m))
        })
  }
| _ -> assert False
];

value call_at_exit ps = do
{
  try
    let f = VM.Machine.lookup_symbol ps.al_scope (Array.of_list (UString.string_to_bytes "ps_at_exit"));
    execute_ps_command_unknown "ps_at_exit" (ref f) ps
  with
  [ Not_found -> () ]
};


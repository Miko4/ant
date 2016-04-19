
open XNum;
open Runtime;
open Unicode;
open Types;
open Logging;
open Typesetting;
open Engine;

value tracing_stacks = ref False;
value tracing_macros = ref False;
value tracing_input  = ref False;

type mode =
[= `Preamble
|  `Galley
|  `Paragraph
|  `Math
|  `HBox
|  `LRBox
|  `RLBox
|  `VBox
|  `Table
];

type parse_state =
{
  job               : Job.job;
  input_stream      : UCStream.istream;
  parse_stack       : Stack.t (mode * ListBuilder.builder Node.node);
  default_char_cmd  : mutable command;
  command_table     : mutable DynUCTrie.t command;
  pattern_table     : mutable DynUCTrie.t command;
  saved_commands    : mutable DynUCTrie.t (list command);
  saved_patterns    : mutable DynUCTrie.t (list command);
  environment_table : mutable DynUCTrie.t (command * command);
  environment_stack : Stack.t (uc_list * list uc_list);
  math_codes        : mutable Charmap.charmap (Box.math_code * (int * int) * (uc_char * uc_char));
  al_scope          : mutable VM.Machine.scope;
  global_variables  : mutable SymbolTable.SymbolMap.t VM.Types.partial_value;
  counter_table     : mutable Counter.counter_table;
  old_references    : mutable DynUCTrie.t uc_string;
  references        : mutable DynUCTrie.t uc_string
}
and command =
{
  execute : parse_state -> unit;
  expand  : parse_state -> uc_list -> uc_list
};

value mode_to_string mode = match mode with
[ `Preamble  -> "preamble"
| `Galley    -> "galley"
| `Paragraph -> "paragraph"
| `Math      -> "math"
| `HBox      -> "hbox"
| `LRBox     -> "lrbox"
| `RLBox     -> "rlbox"
| `VBox      -> "vbox"
| `Table     -> "table"
| _          -> "unknown"
];

(* |create ()| creates an empty parse state. *)

value create job =
{
  job               = job;
  input_stream      = UCStream.create ();
  parse_stack       = Stack.create ();
  default_char_cmd  = { execute = (fun _ -> ()); expand = (fun _ _ -> []) };
  command_table     = DynUCTrie.empty;
  pattern_table     = DynUCTrie.empty;
  saved_commands    = DynUCTrie.empty;
  saved_patterns    = DynUCTrie.empty;
  environment_table = DynUCTrie.empty;
  environment_stack = Stack.create ();
  math_codes        = Charmap.create (Box.NoMath, (0, 0), (0, 0));
  al_scope          = VM.Machine.make_scope ();
  global_variables  = SymbolTable.SymbolMap.empty;
  counter_table     = Counter.empty_table;
  old_references    = DynUCTrie.empty;
  references        = DynUCTrie.empty
};

(*
  |duplicate <parse-state>| creates a new parse-state where the definitions of commands, environments,
  and counters are copied from <parse-state>.
*)

value duplicate ps =
{
  job               = ps.job;
  input_stream      = UCStream.create ();
  parse_stack       = Stack.create ();
  default_char_cmd  = ps.default_char_cmd;
  command_table     = ps.command_table;
  pattern_table     = ps.pattern_table;
  saved_commands    = ps.saved_commands;
  saved_patterns    = ps.saved_patterns;
  environment_table = ps.environment_table;
  environment_stack = Stack.create ();
  math_codes        = Charmap.copy ps.math_codes;
  al_scope          = ps.al_scope;
  global_variables  = ps.global_variables;
  counter_table     = ps.counter_table;
  references        = ps.references;
  old_references    = ps.old_references
};

(* |set_stream <ps> <stream>| replaces the input-stream of <ps>. *)

value set_stream ps stream = do
{
  UCStream.assign ps.input_stream stream
};

value location ps = do
{
  UCStream.location ps.input_stream
};

(*
  The parse stack contains a list of partially constructed lists of nodes. Each such list has an
  associated mode.

  |open_node_list <parse-state> <mode>| puts an empty node-list with the respective mode on the parse-stack.
  |close_node_list <parse-state> <mode>| removes the first element of the parse-stack.
  |add_node <parse-state> <node>| adds a node to the first element of the stack.
*)

value open_node_list ps mode = do
{
  if !tracing_stacks then
    log_string ("\n#S: mode (" ^ mode_to_string mode)
  else ();

  Stack.push (mode, ListBuilder.make ()) ps.parse_stack;
};

value close_node_list ps mode = do
{
  try
    let (m,n) = Stack.top ps.parse_stack;

    if m = mode then do
    {
      if !tracing_stacks then
        log_string ("\n#S: mode " ^ mode_to_string mode ^ ")")
      else ();

      ignore (Stack.pop ps.parse_stack);

      ListBuilder.get n
    }
    else do
    {
      log_warn (location ps)
        ("Mode " ^ mode_to_string mode ^ " expected in " ^ mode_to_string m ^ " mode!");

      []
    }
  with
  [ Stack.Empty -> do
    {
      log_warn (location ps) "There is no node open!";

      []
    }
  ]
};

value add_node ps node = do
{
  try
    let (_,n) = Stack.top ps.parse_stack;

    ListBuilder.add n node
  with
  [ Stack.Empty -> log_warn (location ps) "There is no node open!" ]
};

value current_mode ps = do
{
  try
    let (m,_) = Stack.top ps.parse_stack;

    m
  with
  [ Stack.Empty -> `Preamble ]
};

(* Character commands *)

(* To each character is associated a command which is invoked everytime the character is read. *)

value set_default_char_cmd ps cmd = do
{
  ps.default_char_cmd := cmd;
};

(* commands *)

(*
  |define_command <parse-state> <name> <cmd>| binds the function <cmd> to the name <name>.
  |lookup_command <parse-state> <name>| looks up the definition of <name>.
  |define_buildin_cmd <parse-state> <name> <cmd>| defines a buildin command.
  |define_macro_cmd <parse-state> <name> <body>| defines an argumentless macro.
*)

value define_command ps name cmd = do
{
  (*
    Hack:

    If <name> is a command sequence we have to remove all following white space when executing it.
    Since |execute_next_char| gets the command via |DynUCTrie.lookup_prefix_stream| which does
    not remove spaces, we have to do it here.
  *)

  (*
  if Parser.is_command_sequence name then
    ps.command_table :=
      DynUCTrie.add
        name
        {
          (cmd)

          with

          execute = fun ps -> do
                    {
                      if !tracing_macros then do
                      {
                        log_string "\n#M: ";
                        log_uni_string name
                      }
                      else ();

                      Parser.skip_blanks ps.input_stream;
                      cmd.execute ps
                    }
        }
        ps.command_table
  else
    *)
  ps.command_table := DynUCTrie.add_list name cmd ps.command_table
};

value define_pattern ps name cmd = do
{
  ps.pattern_table := DynUCTrie.add_list name cmd ps.pattern_table
};


value lookup_command ps name = do
{
  DynUCTrie.lookup_list name ps.command_table
};

value lookup_pattern_prefix ps = do
{
  DynUCTrie.lookup_prefix_stream ps.input_stream ps.pattern_table
};

value save_command ps name = do
{
  match DynUCTrie.lookup_list name ps.command_table with
  [ None -> do
    {
      ps.saved_commands := DynUCTrie.add_list name [] ps.saved_commands
    }
  | Some cmd -> do
    {
      let old_list = match DynUCTrie.lookup_list name ps.saved_commands with
      [ None   -> []
      | Some l -> l
      ];

      ps.saved_commands := DynUCTrie.add_list name [cmd :: old_list] ps.saved_commands
    }
  ]
};

value restore_command ps name = do
{
  match DynUCTrie.lookup_list name ps.saved_commands with
  [ None -> do
    {
      log_warn (location ps) "Command ";
      log_uc_list name;
      log_string " undefined!"
    }
  | Some [] -> do
    {
      ps.command_table  := DynUCTrie.remove_list name ps.command_table;
      ps.saved_commands := DynUCTrie.remove_list name ps.saved_commands
    }
  | Some [c::cs] -> do
    {
      ps.command_table  := DynUCTrie.add_list name c  ps.command_table;
      ps.saved_commands := DynUCTrie.add_list name cs ps.saved_commands
    }
  ]
};

value save_pattern ps name = do
{
  match DynUCTrie.lookup_list name ps.pattern_table with
  [ None -> do
    {
      ps.saved_patterns := DynUCTrie.add_list name [] ps.saved_patterns
    }
  | Some pat -> do
    {
      let old_list = match DynUCTrie.lookup_list name ps.saved_patterns with
      [ None   -> []
      | Some l -> l
      ];

      ps.saved_patterns := DynUCTrie.add_list name [pat :: old_list] ps.saved_patterns
    }
  ]
};

value restore_pattern ps name = do
{
  match DynUCTrie.lookup_list name ps.saved_patterns with
  [ None -> do
    {
      log_warn (location ps) "Pattern ";
      log_uc_list name;
      log_string " undefined!"
    }
  | Some [] -> do
    {
      ps.pattern_table  := DynUCTrie.remove_list name ps.pattern_table;
      ps.saved_patterns := DynUCTrie.remove_list name ps.saved_patterns
    }
  | Some [c::cs] -> do
    {
      ps.pattern_table  := DynUCTrie.add_list name c  ps.pattern_table;
      ps.saved_patterns := DynUCTrie.add_list name cs ps.saved_patterns
    }
  ]
};

(* environemnts *)

value push_env ps name args = do
{
  if !tracing_stacks then do
  {
    log_string "\n#S: env (";
    log_uc_list name
  }
  else ();

  Stack.push (name, args) ps.environment_stack;
};

value pop_env ps = do
{
  try do
  {
    let (name, args) = Stack.pop ps.environment_stack;

    if !tracing_stacks then do
    {
      log_string "\n#S: env ";
      log_uc_list name;
      log_string ")"
    }
    else ();

    (name, args)
  }
  with
  [ Stack.Empty -> do
    {
      log_error (location ps) "there is no environment open!";
      ([], [])
    }
  ]
};

value set_env_args ps args = do
{
  try do
  {
    let (name, _) = Stack.pop ps.environment_stack;

    Stack.push (name, args) ps.environment_stack;
  }
  with
  [ Stack.Empty -> log_error (location ps) "there is no environment open!" ]
};

value top_env ps = do
{
  try
    Stack.top ps.environment_stack
  with
  [ Stack.Empty -> do
    {
      log_error (location ps) "there is no environment open!";
      ([], [])
    }
  ]
};

value lookup_env ps name = do
{
  try
    Some (DynUCTrie.find_list name ps.environment_table)
  with
  [ Not_found -> None ]
};

(* |define_env <name> <begin-cmd> <end-cmd> defines the environment <name>. *)

value define_env ps name begin_cmd end_cmd = do
{
  ps.environment_table := DynUCTrie.add_list name (begin_cmd, end_cmd) ps.environment_table
};

(* math codes *)

value set_math_code_table ps table = do
{
  ps.math_codes := table
};

value set_math_code ps char code small_family small_glyph large_family large_glyph = do
{
  Charmap.set
    ps.math_codes
    char
    (code, (small_family, large_family), (small_glyph, large_glyph))
};

value get_math_code ps char = do
{
  match Charmap.lookup ps.math_codes char with
  [ (Box.NoMath, _, _) ->
      (Box.NoMath, (1, 1), (char, char))  (* |NoMath| is the same as |Ordinary| but it indicates *)
                                          (* that |char| is a character, not a glyph number.     *)
  | code -> code
  ]
};

(* counters *)

value new_counter ps name val super = do
{
  ps.counter_table :=
    Counter.new_counter
      (location ps)
      ps.counter_table
      name
      val
      super
};

value get_counter ps name = do
{
  Counter.get_counter
    (location ps)
    ps.counter_table
    name
};

value set_counter ps name val = do
{
  ps.counter_table :=
    Counter.set_counter
      (location ps)
      ps.counter_table
      name
      val
};

(* unique names *)

value unique_name_counter = ref 0;

value gen_unique_name () = do
{
  incr unique_name_counter;

  UString.of_ascii " $uid " @ Format.num_to_arabic 10 (num_of_int !unique_name_counter)
};

(* references *)

value add_reference ps name str = do
{
  ps.references := DynUCTrie.add_list name str ps.references
};

value reference_exists ps name = do
{
  DynUCTrie.mem_list name ps.references
};

value lookup_reference ps name = do
{
  try
    let ref = DynUCTrie.find_list name ps.references;

    ref
  with
  [ Not_found ->
    try
      let ref = DynUCTrie.find_list name ps.old_references;

      ref
    with
    [ Not_found -> do
      {
        log_warn (location ps) "Unknown reference `";
        log_uc_list name;
        log_string "'!";

        [||]
      }
    ]
  ]
};

value iter_references ps f = do
{
  DynUCTrie.iter f ps.references
};

value store_old_references ps = do
{
  ps.old_references := ps.references
};

value compare_references ps = do
{
  (* compare old and new value *)

  let cmp name str differ = do
  {
    try
       differ || (DynUCTrie.find_string name ps.old_references <> str)
    with
    [ Not_found -> True ]
  };

  (* where any references deleted? *)

  let find name _ differ = do
  {
    differ || (not (DynUCTrie.mem_string name ps.references))
  };

     DynUCTrie.fold cmp  ps.references     False
  || DynUCTrie.fold find ps.old_references False
};

value write_references ps name = do
{
  let oc = open_out_bin name;

  output_string oc "\\ALcommand{do\n";

  let print_ref name str = do
  {
    output_string oc "  ps_add_reference \"";
    output_string oc (UString.to_string (Array.to_list name));
    output_string oc "\" \"";
    output_string oc (UString.to_string (Array.to_list str));
    output_string oc "\";\n"
  };

  iter_references ps print_ref;

  output_string oc "  ps_store_old_references;\nend}\n";

  close_out oc
};

(* main loop of the parser *)

(*
  |execute_next_char <parse_state>| reads the next input character and executes the corresponding
  character-command.
*)

value execute_next_char ps = do
{
  if !tracing_input then do
  {
    log_string "\n#I: ";
    log_uc_list (UCStream.take ps.input_stream 5);
  }
  else ();

  (* First, check whether the input starts with a defined pattern. *)

  match lookup_pattern_prefix ps with
  [ Some cmd -> do
    {
      cmd.execute ps;
      True
    }
  | None -> do
    {
      (* Otherwise, execute the default character command. *)

      if UCStream.eof ps.input_stream then
        False
      else do
      {
        let cmd = ps.default_char_cmd;
        cmd.execute ps;
        True
      }
    }
  ]
};

(*
  |run_parser <parse-state> <mode>| calls |execute_next_char| unit the input stream is empty.
*)

value run_parser ps mode = do
{
  open_node_list ps mode;

  while execute_next_char ps do
  {
    ()
  };

  close_node_list ps mode
};

(* |execute_stream <parse-state> <stream>| runs the parser on the contents of <stream>. *)

value execute_stream ps stream = do
{
  UCStream.exchange ps.input_stream stream;

  while execute_next_char ps do
  {
    ()
  };

  UCStream.exchange ps.input_stream stream
};

(*
  |execute_argument <parse_state> <mode>| reads the next token or group, and executes it.
*)

value execute_argument ps = do
{
  let rec execute_group nest = do
  {
    let c = UCStream.next_char ps.input_stream;

    if c < 0 then
      ()
    else match CharCode.cat_code c with
    [ CharCode.BeginGroup -> do
      {
        ignore (execute_next_char ps);
        execute_group (nest + 1)
      }
    | CharCode.EndGroup ->
        if nest = 0 then
          ()
        else do
        {
          ignore (execute_next_char ps);
          execute_group (nest - 1)
        }
    | _ -> do
      {
        ignore (execute_next_char ps);
        execute_group nest
      }
    ]
  };

  if UCStream.next_char ps.input_stream = CharCode.begin_group_char then do
  {
    UCStream.remove ps.input_stream 1;

    execute_group 0;

    UCStream.remove ps.input_stream 1;
  }
  else
    ignore (execute_next_char ps);
};

(*
  |execute_argument_in_mode <parse_state> <mode>| opens a new node of mode <mode> and calls
  |execute_argument|.
*)

value execute_argument_in_mode ps mode = do
{
  open_node_list ps mode;

  execute_argument ps;

  close_node_list ps mode
};

value execute_string_in_mode ps str mode = do
{
  set_stream ps (UCStream.of_list str);

  run_parser ps mode
};


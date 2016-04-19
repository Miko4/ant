
open Unicode;
open Types;

(* locations *)

type location = (string * int * int);

type internal_location =
{
  filename : string;       (* the filename                *)
  line     : mutable int;  (* the current line and column *)
  column   : mutable int;
  fixed    : bool          (* Should <line> and <column> be increased when reading form the file? *)
};

value create_location filename fixed =
{
  filename = filename;
  line     = 0;
  column   = 0;
  fixed    = fixed
};

value make_internal_location (name, line, col) fixed =
{
  filename = name;
  line     = line;
  column   = col;
  fixed    = fixed
};

value duplicate_location loc =
{
  filename = loc.filename;
  line     = loc.line;
  column   = loc.column;
  fixed    = loc.fixed
};

value fix_location loc = do
{
  if loc.fixed then
    loc
  else
    { (loc) with fixed = True }
};

value inc_line loc = do
{
  if not loc.fixed then do
  {
    loc.line   := loc.line + 1;
    loc.column := 0
  }
  else ()
};

value inc_column loc = do
{
  if not loc.fixed then
    loc.column := loc.column + 1
  else ()
};

value update_location loc char = do
{
  if char = 10 then
    inc_line loc
  else
    inc_column loc
};

(* streams *)

type simple_stream =
{
  chars : mutable uc_list;
  loc   : internal_location
};

value duplicate_simple_stream str =
{
  (str) with loc = duplicate_location str.loc
};

type istream =
{
  current : mutable simple_stream;
  stack   : mutable list simple_stream
};

(*
value dump_loc l =
  Printf.eprintf "(%s, %d, %d, %s)" l.filename l.line l.column (if l.fixed then "+" else "-");
value dump_simple s = do
{
  match s.chars with
  [ [] -> Printf.eprintf "[] "
  | [c] -> Printf.eprintf "[%d] " c
  | [c1; c2] -> Printf.eprintf "[%d; %d] " c1 c2
  | [c1; c2; c3] -> Printf.eprintf "[%d; %d; %d] " c1 c2 c3
  | [c1; c2; c3; c4] -> Printf.eprintf "[%d; %d; %d; %d] " c1 c2 c3 c4
  | [c1; c2; c3; c4; c5 :: _] -> Printf.eprintf "[%d; %d; %d; %d; %d] " c1 c2 c3 c4 c5
  ];
  dump_loc s.loc;
  Printf.eprintf "\n"
};
value dump is = do
{
  Printf.eprintf "=> ";
  dump_simple is.current;
  List.iter (fun s -> do { Printf.eprintf "   "; dump_simple s }) is.stack
};
*)

value create () =
{
  stack   = [];
  current =
    {
      chars = [];
      loc   = create_location "" True
    }
};

value of_list s =
{
  stack   = [];
  current =
    {
      chars = s;
      loc   = create_location "" True
    }
};

value to_list is = do
{
  List.concat [is.current.chars
               :: List.map (fun s -> s.chars) is.stack]
};

value of_string s  = of_list (Array.to_list s);
value to_string is = Array.of_list (to_list is);

value assign is1 is2 = do
{
  is1.current := duplicate_simple_stream is2.current;
  is1.stack   := List.map duplicate_simple_stream is2.stack
};

value exchange is1 is2 = do
{
  let temp =
    {
      current = is1.current;
      stack   = is1.stack
    };
  assign is1 is2;
  assign is2 temp
};

value duplicate is =
{
  current = duplicate_simple_stream is.current;
  stack   = List.map duplicate_simple_stream is.stack
};

value location is = (is.current.loc.filename, is.current.loc.line, is.current.loc.column);

value set_location is loc fixed = do
{
  is.current := { (is.current) with loc = make_internal_location loc fixed }
};

value get_char is pos = do
{
  iter pos is.current.chars is.stack

  where rec iter pos chars stack = match chars with
  [ [c::cs] -> do
    {
      if pos = 0 then
        c
      else
        iter (pos - 1) cs stack
    }
  | [] -> match stack with
    [ []      -> (-1)
    | [s::ss] -> iter pos s.chars ss
    ]
  ]
};

value eof is = (get_char is 0 < 0);

value rec next_char is = do
{
  match is.current.chars with
  [ [c::_] -> c
  | []     -> match is.stack with
    [ []      -> (-1)
    | [s::ss] -> do
      {
        is.current := s;
        is.stack   := ss;
        next_char is
      }
    ]
  ]
};

value take is num = do
{
  iter num is.current.chars is.stack

  where rec iter n chars stack = do
  {
    if n <= 0 then
      []
    else match chars with
    [ [c::cs] -> [c :: iter (n-1) cs stack]
    | []      -> match stack with
      [ [s::ss] -> iter n s.chars ss
      | []      -> []
      ]
    ]
  }
};

value rec pop is = match is.current.chars with
[ [c::cs] -> do
  {
    is.current.chars := cs;
    update_location is.current.loc c;
    c
  }
| [] -> match is.stack with
  [ []      -> (-1)
  | [s::ss] -> do
    {
      is.current := s;
      is.stack   := ss;
      pop is
    }
  ]
];

value remove is num = do
{
  iter num

  where rec iter n = do
  {
    if n <= 0 || pop is < 0 then
      ()
    else
      iter (n-1)
  }
};

value clear is = do
{
  iter is.current is.stack

  where rec iter last stack = match stack with
  [ [s::ss] -> iter s ss
  | []      -> do
    {
      is.current := last;
      is.stack   := [];

      while next_char is >= 0 do
      {
        ()
      }
    }
  ]
};

value match_prefix is str = do
{
  iter str is.current.chars is.stack

  where rec iter str chars stack = match str with
  [ []      -> True
  | [c::cs] -> match chars with
    [ [x::xs] -> if c = x then
                   iter cs xs stack
                 else
                   False
    | [] -> match stack with
      [ []      -> False
      | [s::ss] -> iter str s.chars ss
      ]
    ]
  ]
};

(* creating streams *)

value insert_list is str = do
{
  is.stack   := [is.current :: is.stack];
  is.current := {
                  chars = str;
                  loc   = fix_location is.current.loc
                }
};

value insert_string is str = insert_list is (Array.to_list str);

value insert_stream is stream = do
{
  if stream.current.loc.filename = "" then do
  {
    let loc = fix_location is.current.loc;

    is.stack   := List.map (fun s -> { (s) with loc = loc }) stream.stack
                @ [is.current :: is.stack];
    is.current := { (stream.current) with loc = loc }
  }
  else do
  {
    is.stack   := stream.stack @ [is.current :: is.stack];
    is.current := stream.current
  }
};

value include_file is name = do
{
  let buf = ListBuilder.make ();

  try do
  {
    let s = IO.make_in_stream name;

    is.stack   := [is.current :: is.stack];
    is.current := {
                    chars = iter s;
                    loc   = create_location name False
                  };
    IO.free s
  }
  with [_ -> ()]

  where rec iter s = do
  {
    if IO.eof s then
      ListBuilder.get buf
    else do
    {
      let c = UString.read_uc_char s;

      if c >= 0 then
        ListBuilder.add buf c
      else ();

      iter s
    }
  }
};

value of_file name = do
{
  let stream = create ();

  include_file stream name;

  stream
};


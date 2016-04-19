
open Unicode.Types;

type trie 'a =
{
  t_tree     : array int;   (* array of 3n entries: offset, char, data, offset, char, data,... *)
  t_data     : array 'a;    (* array of the actual data                                        *)
  t_data_len : mutable int  (* number of used enties in the |t_data| array                     *)
};

value make len =
{
  t_tree     = Array.make (3*len) (-1);
  t_data     = Array.make len (Obj.magic 0);
  t_data_len = 0
};

value shrink trie len =
{
  t_tree     = Array.sub trie.t_tree 0 (3*len);
  t_data     = Array.sub trie.t_data 0 trie.t_data_len;
  t_data_len = trie.t_data_len
};

value get_offset     trie pos = trie.t_tree.(3*pos);
value get_char       trie pos = trie.t_tree.(3*pos+1);
value get_data_index trie pos = trie.t_tree.(3*pos+2);

value set_offset     trie pos x = trie.t_tree.(3*pos)   := x;
value set_char       trie pos x = trie.t_tree.(3*pos+1) := x;
value set_data_index trie pos x = trie.t_tree.(3*pos+2) := x;

value get_data trie pos = do
{
  let index = get_data_index trie pos;

  if index < 0 then
    None
  else
    Some trie.t_data.(index)
};

value set_data trie pos x = do
{
  let index = get_data_index trie pos;

  if index < 0 then do
  {
    trie.t_data.(trie.t_data_len) := x;
    trie.t_tree.(3*pos+2) := trie.t_data_len;
    trie.t_data_len       := trie.t_data_len + 1
  }
  else
    trie.t_data.(index) := x
};

(* |lookup <trie> <pos> <str>| checks whether <str> occures in <trie> starting at <pos>. *)

value lookup trie pos str = do
{
  if Array.length str = 0 then
    None
  else
    iter pos 0

  where rec iter pos i = do
  {
    let chr = str.(i);

    if get_char trie (pos + chr) <> chr then
      None
    else if i + 1 >= Array.length str then
      get_data trie (pos + chr)
    else
      iter (get_offset trie (pos + chr)) (i+1)
  }
};

value lookup_list trie pos str = match str with
[ []      -> None
| [c::cs] -> iter pos c cs
]
where rec iter pos chr str = do
{
  if get_char trie (pos + chr) <> chr then
    None
  else match str with
  [ []      -> get_data trie (pos + chr)
  | [c::cs] -> iter (get_offset trie (pos + chr)) c cs
  ]
};

(*
  |lookup_prefix <trie> <pos> <str> <start>| checks whether a prefix of the substring of <str> that
  starts at <start> occures in <trie> starting at <pos>.
*)

value lookup_prefix trie pos str start = do
{
  if start >= Array.length str then
    None
  else
    iter pos None start

  where rec iter pos result i = do
  {
    let chr = str.(i);

    if get_char trie (pos + chr) <> chr then
      result
    else do
    {
      let new_result = match get_data trie (pos + chr) with
      [ None -> result
      | x    -> x
      ];

      if i + 1 >= Array.length str then
        new_result
      else
        iter (get_offset trie (pos + chr)) new_result (i+1)
    }
  }
};

value lookup_prefix_list trie pos str = match str with
[ []      -> None
| [c::cs] -> iter pos None c cs
]
where rec iter pos result chr str = do
{
  if get_char trie (pos + chr) <> chr then
    result
  else do
  {
    let new_result = match get_data trie (pos + chr) with
    [ None -> result
    | x    -> x
    ];

    match str with
    [ []      -> new_result
    | [c::cs] -> iter (get_offset trie (pos + chr)) new_result c cs
    ]
  }
};

module SimpleTrie = struct

type node 'a =
{
  tn_offset : int;
  tn_char   : uc_char;
  tn_data   : option 'a
};

type simple_trie 'a = array (list (node 'a));

value make size = do
{
  Array.make size []
};

value next trie pos char = do
{
  iter trie.(pos)

  where rec iter children = match children with
  [ []      -> raise Not_found
  | [c::cs] -> if c.tn_char = char then
                 c.tn_offset
               else
                 iter cs
  ]
};

value add_child trie pos char index = do
{
  let child =
    {
      tn_char   = char;
      tn_offset = index;
      tn_data   = None
    };

  trie.(pos) := iter trie.(pos)

  where rec iter children = match children with
  [ []      -> [child]
  | [c::cs] -> if c.tn_char < char then
                 [c :: iter cs]
               else if c.tn_char = char then
                 [child :: cs]
               else
                 [child :: children]
  ]
};

value set trie pos char data = do
{
  trie.(pos) := iter trie.(pos)

  where rec iter children = match children with
  [ []      -> raise Not_found
  | [c::cs] -> if c.tn_char = char then
                 [{
                    (c)

                    with

                    tn_data = Some data
                  }
                :: cs]
               else
                 [c :: iter cs]
  ]
};

(* |build <data>| creates a simple_trie from a list of (key, value) pairs. *)

value build data = do
{
  let rec calc_size size list = match list with
  [ []                -> size
  | [(str,_) :: tail] -> calc_size (size + Array.length str) tail
  ];

  let size = calc_size 1 data;
  let trie = make size;

  fill_trie 1 data

  where rec fill_trie free data = match data with
  [ []              -> Array.sub trie 0 free
  | [(s,d) :: tail] -> fill_trie (insert 0 free 0 s d) tail
  ]
  where rec insert pos free i str data = do
  {
    if i >= Array.length str then
      free
    else if i + 1 = Array.length str then
      try do
      {
        set trie pos str.(i) data;
        free
      }
      with
      [ Not_found -> do
        {
          add_child trie pos str.(i) free;
          set trie pos str.(i) data;
          free + 1
        }
      ]
    else
      try
        insert (next trie pos str.(i)) free (i+1) str data
      with
      [ Not_found -> do
        {
          add_child trie pos str.(i) free;
          insert free (free+1) (i+1) str data
        }
      ]
  }
};

(* |compress <trie>| translates a simple_trie into a trie. *)

value compress trie = do
{
  let len = Array.length trie;
  let ht  = Hashtbl.create len;

  (* merge duplicates *)

  for i = len - 1 downto 0 do
  {
    trie.(i) := iter trie.(i)

    where rec iter children = match children with
    [ []      -> []
    | [c::cs] -> do
      {
        try
          let x = Hashtbl.find ht trie.(c.tn_offset);

          [{ (c) with tn_offset = x } :: iter cs]
        with
        [ Not_found -> do
          {
            Hashtbl.add ht trie.(c.tn_offset) c.tn_offset;
            [c :: iter cs]
          }
        ];
      }
    ]
  };
};

end;

value build max_code data = do
{
  let simple_trie = SimpleTrie.build data;
  let len = (max_code + 1) * Array.length simple_trie;

  SimpleTrie.compress simple_trie;

  let trie = make len;
  let used = Array.make len False;

  let alloc_node children = do
  {
    let rec find pos children = do
    {
      if used.(pos) then
        find (pos + 1) children
      else
        iter children

      where rec iter ch = match ch with
      [ []      -> pos
      | [c::cs] -> do
        {
          if get_char trie (pos + c.SimpleTrie.tn_char) < 0 then
            iter cs
          else
            find (pos + 1) children
        }
      ]
    };

    let pos = find 0 children;

    used.(pos) := True;

    List.iter
      (fun c -> set_char trie (pos + c.SimpleTrie.tn_char) c.SimpleTrie.tn_char)
      children;

    pos
  };

  (* mapping of indices: simple_trie to trie *)
  let alloc_table = Array.make (Array.length simple_trie) (-1);

  let rec alloc pos = do
  {
    if alloc_table.(pos) < 0 then do
    {
      alloc_table.(pos) := alloc_node simple_trie.(pos);

      List.iter (fun c -> alloc c.SimpleTrie.tn_offset) simple_trie.(pos)
    }
    else
      ()
  };

  alloc 0;

  (* mapping: data to index *)
  let ht = Hashtbl.create 2000;

  fill_trie 0

  where rec fill_trie pos = do
  {
    let trie_pos = alloc_table.(pos);

    let children = simple_trie.(pos);

    simple_trie.(pos) := [];   (* prevent writing this entry multiple times *)

    iter children

    where rec iter children = match children with
    [ []      -> ()
    | [c::cs] -> do
      {
        set_offset trie (trie_pos + c.SimpleTrie.tn_char) alloc_table.(c.SimpleTrie.tn_offset);
        set_char   trie (trie_pos + c.SimpleTrie.tn_char) c.SimpleTrie.tn_char;

        match c.SimpleTrie.tn_data with
        [ None   -> ()
        | Some x -> do
          {
            try
              (* check whether |x| is already somewhere in the data array *)

              let i = Hashtbl.find ht x;

              set_data_index trie (trie_pos + c.SimpleTrie.tn_char) i
            with
            [ Not_found -> do
              {
                set_data trie (trie_pos + c.SimpleTrie.tn_char) x;

                Hashtbl.add ht x (get_data_index trie (trie_pos + c.SimpleTrie.tn_char))
              }
            ]
          }
        ];

        fill_trie c.SimpleTrie.tn_offset;
        iter cs
      }
    ]
  };

  (* remove empty space at the end of the array *)

  strip (len-1)

  where rec strip i = do
  {
    if not used.(i) && i > 0 then
      strip (i-1)
    else
      shrink trie (i + max_code)
  }
};


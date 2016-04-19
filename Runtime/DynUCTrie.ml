
(* A dynamic trie |t 'a| implements a map |uc_string -> 'a| that supports adding new elements. *)

open Unicode.Types;

type t 'a =
{
  children : DynamicCharMap.t (t 'a);
  data     : option 'a
};

value empty =
{
  children = DynamicCharMap.empty;
  data     = None
};

value is_empty trie = (trie.data = None) && DynamicCharMap.is_empty trie.children;

value prefix trie char = do
{
  try
    DynamicCharMap.find char trie.children
  with
  [ Not_found -> empty ]
};

value root_value trie = trie.data;

value generic_lookup fold str trie = do
{
  root_value (fold prefix trie str)
};

(*
  For efficiency reasons the following definitions are expanded:

value lookup_string str trie = generic_lookup Array.fold_left str trie;
value lookup_list   str trie = generic_lookup List.fold_left  str trie;
*)

value lookup_string str trie = do
{
  iter 0 trie

  where rec iter i trie = do
  {
    if i >= Array.length str then
      trie.data
    else do
    {
      try
        let next = DynamicCharMap.find str.(i) trie.children;

        iter (i+1) next
      with
      [ Not_found -> None ]
    }
  }
};

value rec lookup_list str trie = match str with
[ []      -> trie.data
| [c::cs] -> do
  {
    try
      let next = DynamicCharMap.find c trie.children;

      lookup_list cs next
    with
    [ Not_found -> None ]
  }
];

(*
  |lookup_prefix <str> <trie>| returns the data associated to the longest prefix of <str> that
  is found in <trie>.
*)

value generic_lookup_prefix fold str trie = do
{
  root_value (fst
    (fold
      (fun (last, t) c -> do
        {
          let next = prefix t c;

          match next.data with
          [ Some _ -> (next, next)
          | None   -> (last, next)
          ]
        })
      (trie, trie)
      str))
};

(*
  For efficiency reasons the following definitions are expanded:

value lookup_prefix_string str trie = generic_lookup_prefix Array.fold_left str trie;
value lookup_prefix_list   str trie = generic_lookup_prefix List.fold_left  str trie;
*)

value lookup_prefix_string str trie = do
{
  iter 0 trie None

  where rec iter i trie data = do
  {
    if i >= Array.length str then
      match trie.data with
      [ Some _ -> trie.data
      | None   -> data
      ]
    else do
    {
      try
        let next = DynamicCharMap.find str.(i) trie.children;

        match next.data with
        [ Some _ -> iter (i+1) next next.data
        | None   -> iter (i+1) next data
        ]
      with
      [ Not_found -> data ]
    }
  };
};

value lookup_prefix_list str trie = do
{
  iter trie str None

  where rec iter trie str data = match str with
  [ []      -> match trie.data with
               [ Some _ -> trie.data
               | None   -> data
               ]
  | [c::cs] -> do
    {
      try
        let next = DynamicCharMap.find c trie.children;

        match next.data with
        [ Some _ -> iter next cs next.data
        | None   -> iter next cs data
        ]
      with
      [ Not_found -> data ]
    }
  ];
};

(*
  |lookup_prefix_stream <is> <trie>| returns the data associated to the longest prefix of <is> that
  is found in <trie>. The matching prefix will be removed from <is>.
*)

value lookup_prefix_stream is trie = do
{
  let stream = UCStream.duplicate is;

  let rec iter trie len pos data = do
  {
    if UCStream.eof stream then
      match trie.data with
      [ Some _ -> (trie.data, pos)
      | None   -> (data,      len)
      ]
    else
      try do
      {
        let next = DynamicCharMap.find (UCStream.pop stream) trie.children;

        match next.data with
        [ Some _ -> iter next (pos + 1) (pos + 1) next.data
        | None   -> iter next len (pos + 1) data
        ]
      }
      with
      [ Not_found -> (data, len) ]
  };

  let (data, len) = iter trie 0 0 None;

  UCStream.remove is len;
  data
};

value find_string str trie = match lookup_string str trie with
[ Some x -> x
| None   -> raise Not_found
];

value mem_string str trie = match lookup_string str trie with
[ Some _ -> True
| None   -> False
];

value find_list str trie = match lookup_list str trie with
[ Some x -> x
| None   -> raise Not_found
];

value mem_list str trie = match lookup_list str trie with
[ Some _ -> True
| None   -> False
];

value add_string str data trie = do
{
  iter 0 trie

  where rec iter i trie = do
  {
    if i >= Array.length str then
      { (trie) with data = Some data }
    else do
    {
      let next = try
                   DynamicCharMap.find str.(i) trie.children
                 with
                 [ Not_found -> empty ];

      {
        (trie)

        with

        children = DynamicCharMap.add str.(i) (iter (i+1) next) trie.children
      }
    }
  }
};

value remove_string str trie = do
{
  iter 0 trie

  where rec iter i trie = do
  {
    if i >= Array.length str then
      { (trie) with data = None }
    else do
    {
      try
        let next = DynamicCharMap.find str.(i) trie.children;

        {
          (trie)

          with

          children = DynamicCharMap.add str.(i) (iter (i+1) next) trie.children
        }
      with
      [ Not_found -> trie ]
    }
  }
};

value rec add_list str data trie = match str with
[ []      -> { (trie) with data = Some data }
| [c::cs] -> do
  {
    let next = try
                 DynamicCharMap.find c trie.children
               with
               [ Not_found -> empty ];

    {
      (trie)

      with

      children = DynamicCharMap.add c (add_list cs data next) trie.children
    }
  }
];

value rec remove_list str trie = match str with
[ []      -> { (trie) with data = None }
| [c::cs] -> do
  {
    try
      let next = DynamicCharMap.find c trie.children;

      {
        (trie)

        with

        children = DynamicCharMap.add c (remove_list cs next) trie.children
      }
    with
    [ Not_found -> trie ]
  }
];

value merge t1 t2 = do
{
  {
    data =
      match t2.data with
      [ None   -> t1.data
      | Some _ -> t2.data
      ];
    children =
      if DynamicCharMap.is_empty t1.children then
        t2.children
      else
        DynamicCharMap.fold
          (fun c x children -> DynamicCharMap.add c x children)
          t2.children
          t1.children
  }
};

value rec map f trie =
{
  children = DynamicCharMap.map (map f) trie.children;
  data     = match trie.data with
             [ Some x -> Some (f x)
             | None   -> None
             ]
};

value mapi f trie = do
{
  iter [] trie

  where rec iter str trie =
  {
    children = DynamicCharMap.mapi (fun c t -> iter [c :: str] t) trie.children;
    data     = match trie.data with
               [ Some x -> Some (f (XList.rev_to_array str) x)
               | None   -> None
               ]
  }
};

value rec iter f trie = do
{
  loop [] trie

  where rec loop str trie = do
  {
    match trie.data with
    [ Some x -> f (XList.rev_to_array str) x
    | None   -> ()
    ];

    DynamicCharMap.iter (fun c t -> loop [c :: str] t) trie.children
  }
};

value fold f trie e = do
{
  iter [] trie e

  where rec iter str trie e = do
  {
    let x = match trie.data with
    [ Some y -> f (XList.rev_to_array str) y e
    | None   -> e
    ];
    DynamicCharMap.fold (fun c t y -> iter [c :: str] t y) trie.children x;
  }
};

value rec depth trie = do
{
  if DynamicCharMap.is_empty trie.children then
    0
  else do
  {
    let d = DynamicCharMap.fold
              (fun _ t d -> max d (depth t))
              trie.children
              0;

    d + 1
  }
};


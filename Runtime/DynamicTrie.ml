
(* A dynamic trie |t 'a| implements a map |list elt -> 'a| that supports adding new elements. *)

module type OrderedType = Map.OrderedType;

module type S =
sig
  type elt;

  type t 'a;

  value empty                 : t 'a;
  value is_empty              : t 'a -> bool;
  value prefix                : t 'a -> elt -> t 'a;
  value root_value            : t 'a -> option 'a;
  value depth                 : t 'a -> int;

  value find_array            : array elt -> t 'a -> 'a;
  value mem_array             : array elt -> t 'a -> bool;
  value find_list             : list  elt -> t 'a -> 'a;
  value mem_list              : list  elt -> t 'a -> bool;
  value generic_lookup        : ((t 'a -> elt -> t 'a) -> t 'a -> 'b -> t 'a) ->
                                'b -> t 'a -> option 'a;
  value generic_lookup_prefix : (((t 'a * t 'a) -> elt -> (t 'a * t 'a)) ->
                                 (t 'a * t 'a) -> 'b -> (t 'a * t 'a)) ->
                                'b -> t 'a -> option 'a;
  value lookup_array          : array elt -> t 'a -> option 'a;
  value lookup_list           : list  elt -> t 'a -> option 'a;
  value lookup_prefix_array   : array elt -> t 'a -> option 'a;
  value lookup_prefix_list    : list  elt -> t 'a -> option 'a;
  value add_array             : array elt -> 'a -> t 'a -> t 'a;
  value remove_array          : array elt -> t 'a -> t 'a;
  value add_list              : list  elt -> 'a -> t 'a -> t 'a;
  value remove_list           : list  elt -> t 'a -> t 'a;
  value merge                 : t 'a -> t 'a -> t 'a;
  value map                   : ('a -> 'b) -> t 'a -> t 'b;
  value mapi                  : (array elt -> 'a -> 'b) -> t 'a -> t 'b;
  value iter                  : (array elt -> 'a -> unit) -> t 'a -> unit;
  value fold                  : (array elt -> 'a -> 'b -> 'b) -> t 'a -> 'b -> 'b;
end;

module Make(Ord: OrderedType) =
struct

  module ChildMap = Map.Make(Ord);

  type elt = Ord.t;

  type t 'a =
  {
    children : ChildMap.t (t 'a);
    data     : option 'a
  };

  value empty =
  {
    children = ChildMap.empty;
    data     = None
  };

  value is_empty trie = (trie.data = None) && ChildMap.is_empty trie.children;

  value prefix trie char = do
  {
    try
      ChildMap.find char trie.children
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

  value lookup_array str trie = generic_lookup Array.fold_left str trie;
  value lookup_list  str trie = generic_lookup List.fold_left  str trie;
  *)

  value lookup_array str trie = do
  {
    iter 0 trie

    where rec iter i trie = do
    {
      if i >= Array.length str then
        trie.data
      else do
      {
        try
          let next = ChildMap.find str.(i) trie.children;

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
        let next = ChildMap.find c trie.children;

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

  value lookup_prefix_array str trie = generic_lookup_prefix Array.fold_left str trie;
  value lookup_prefix_list  str trie = generic_lookup_prefix List.fold_left  str trie;
  *)

  value lookup_prefix_array str trie = do
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
          let next = ChildMap.find str.(i) trie.children;

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
          let next = ChildMap.find c trie.children;

          match next.data with
          [ Some _ -> iter next cs next.data
          | None   -> iter next cs data
          ]
        with
        [ Not_found -> data ]
      }
    ];
  };

  value find_array str trie = match lookup_array str trie with
  [ Some x -> x
  | None   -> raise Not_found
  ];

  value mem_array str trie = match lookup_array str trie with
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

  value add_array str data trie = do
  {
    iter 0 trie

    where rec iter i trie = do
    {
      if i >= Array.length str then
        { (trie) with data = Some data }
      else do
      {
        let next = try
                     ChildMap.find str.(i) trie.children
                   with
                   [ Not_found -> empty ];

        {
          (trie)

          with

          children = ChildMap.add str.(i) (iter (i+1) next) trie.children
        }
      }
    }
  };

  value remove_array str trie = do
  {
    iter 0 trie

    where rec iter i trie = do
    {
      if i >= Array.length str then
        { (trie) with data = None }
      else do
      {
        try
          let next = ChildMap.find str.(i) trie.children;

          {
            (trie)

            with

            children = ChildMap.add str.(i) (iter (i+1) next) trie.children
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
                   ChildMap.find c trie.children
                 with
                 [ Not_found -> empty ];

      {
        (trie)

        with

        children = ChildMap.add c (add_list cs data next) trie.children
      }
    }
  ];

  value rec remove_list str trie = match str with
  [ []      -> { (trie) with data = None }
  | [c::cs] -> do
    {
      try
        let next = ChildMap.find c trie.children;

        {
          (trie)

          with

          children = ChildMap.add c (remove_list cs next) trie.children
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
        if ChildMap.is_empty t1.children then
          t2.children
        else
          ChildMap.fold
            (fun c x children -> ChildMap.add c x children)
            t2.children
            t1.children
    }
  };

  value rec map f trie =
  {
    children = ChildMap.map (map f) trie.children;
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
      children = ChildMap.mapi (fun c t -> iter [c :: str] t) trie.children;
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

      ChildMap.iter (fun c t -> loop [c :: str] t) trie.children
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
      ChildMap.fold (fun c t y -> iter [c :: str] t y) trie.children x;
    }
  };

  value rec depth trie = do
  {
    if ChildMap.is_empty trie.children then
      0
    else do
    {
      let d = ChildMap.fold
                (fun _ t d -> max d (depth t))
                trie.children
                0;

      d + 1
    }
  };

end;


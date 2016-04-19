
open Unicode;
open SymbolTable;

type elt = symbol;

type t = list symbol;

value empty = [];

value is_empty set = match set with
[ [] -> True
| _  -> False
];

value rec mem sym (set : t) = match set with
[ []      -> False
| [s::ss] -> if s < sym then
               mem sym ss
             else if s > sym then
               False
             else
               True
];

value rec add sym (set : t) = match set with
[ []      -> [sym]
| [s::ss] -> if s < sym then
               [s :: add sym ss]
             else if s > sym then
               [ sym :: set]
             else
               set
];

value singleton sym = [sym];

value rec remove sym (set : t) = match set with
[ []      -> []
| [s::ss] -> if s < sym then
               [ s :: remove sym ss]
             else if s > sym then
               set
             else
               ss
];

value fold f e (set : t) = do
{
  iter e set

  where rec iter x set = match set with
  [ []      -> x
  | [s::ss] -> iter (f x s) ss
  ]
};

value cardinal = List.length;

value elements (set : t) = set;

value min_elt (set : t) = match set with
[ []     -> raise Not_found
| [s::_] -> s
];

value max_elt (set : t) = match set with
[ []      -> raise Not_found
| [s::ss] -> do
  {
    iter s ss

    where rec iter last set = match set with
    [ []      -> last
    | [s::ss] -> iter s ss
    ]
  }
];

value choose = min_elt;

module SymbolTrie =
struct
  module ST = DynamicTrie.Make(struct type t = int; value compare = (compare : int -> int -> int); end);

  type trie 'a   = ST.t 'a;

  value empty    = ST.empty;
  value is_empty = ST.is_empty;

  value find     = ST.find_list;
  value mem      = ST.mem_list;
  value lookup   = ST.lookup_list;
  value add      = ST.add_list;
  value remove   = ST.remove_list;
  value merge    = ST.merge;
  value map      = ST.map;
end;



open Unicode;
open SymbolTable;

type elt = symbol;

type t;

value empty     : t;
value is_empty  : t -> bool;
value mem       : elt -> t -> bool;
value add       : elt -> t -> t;
value singleton : elt -> t;
value remove    : elt -> t -> t;

value fold      : ('a -> elt -> 'a) -> 'a -> t -> 'a;

value cardinal  : t -> int;
value elements  : t -> list elt;
value min_elt   : t -> elt;
value max_elt   : t -> elt;
value choose    : t -> elt;

module SymbolTrie :
sig
  type trie 'a;

  value empty    : trie 'a;
  value is_empty : trie 'a -> bool;

  value find     : t -> trie 'a -> 'a;
  value mem      : t -> trie 'a -> bool;
  value lookup   : t -> trie 'a -> option 'a;
  value add      : t -> 'a -> trie 'a -> trie 'a;
  value remove   : t -> trie 'a -> trie 'a;
  value merge    : trie 'a -> trie 'a -> trie 'a;
  value map      : ('a -> 'b) -> trie 'a -> trie 'b;
end;


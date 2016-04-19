
open Unicode.Types;

(* pointed tables *)

type table 'a;

value create  : uc_string -> 'a -> table 'a;
value sync    : table 'a -> table 'a;
value current : table 'a -> 'a;
value key     : table 'a -> uc_string;
value table   : table 'a -> DynUCTrie.t 'a;
value get     : table 'a -> uc_string -> 'a;
value select  : table 'a -> uc_string -> table 'a;
value add     : table 'a -> uc_string -> 'a -> table 'a;
value set     : table 'a -> 'a -> table 'a;
value update  : table 'a -> DynUCTrie.t 'a -> table 'a;
value map     : table 'a -> ('a -> 'b) -> table 'b;
value mapi    : table 'a -> (uc_string -> 'a -> 'b) -> table 'b;


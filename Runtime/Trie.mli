
open Unicode.Types;

type trie 'a =
{
  t_tree     : array int;   (* array of 3n entries: offset, char, data, offset, char, data,... *)
  t_data     : array 'a;    (* array of the actual data                                        *)
  t_data_len : mutable int  (* number of used enties in the |t_data| array                     *)
};

value get_offset : trie 'a -> int -> int;
value get_char   : trie 'a -> int -> int;
value get_data   : trie 'a -> int -> option 'a;

value lookup             : trie 'a -> int -> uc_string -> option 'a;
value lookup_prefix      : trie 'a -> int -> uc_string -> int -> option 'a;
value lookup_list        : trie 'a -> int -> uc_list -> option 'a;
value lookup_prefix_list : trie 'a -> int -> uc_list -> option 'a;

value build  : int -> list (uc_string * 'a) -> trie 'a;



open Unicode.Types;

type hyphen_table =
{
  ht_char_classes  : Unicode.Charmap.charmap int;
  ht_pattern_trie  : Trie.trie (array int);
  ht_pattern_start : int
};

value char_code : hyphen_table -> uc_char -> int;
value hyphenate : hyphen_table -> int -> int -> uc_string -> array bool;


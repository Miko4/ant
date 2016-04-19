
open Unicode.Types;

type hyphen_table =
{
  ht_char_classes  : Unicode.Charmap.charmap int;
  ht_pattern_trie  : Trie.trie (array int);
  ht_pattern_start : int
};


(*
  |code <char>| converts a character to some number such that characters with the same number are
  treated the same by the hyphenation routine.
*)

value char_code table chr = do
{
  if chr < 0 then
    0
  else
    Unicode.Charmap.lookup table.ht_char_classes chr
};

value hyphenate table left_min right_min str = do
{
  let len     = Array.length str;
  let numbers = Array.make (len + 1) 0;
  let codes   = Array.map (char_code table) str;

  for pos = 0 to len - 1 do
  {
    match Trie.lookup_prefix table.ht_pattern_trie table.ht_pattern_start codes pos with
    [ None    -> ()
    | Some ns -> do
      {
        for i = 0 to Array.length ns - 1 do
        {
          numbers.(pos + i) := max numbers.(pos + i) ns.(i)
        }
      }
    ]
  };

  (* Clear breaks within <left-min> and <right-min> of a word boundary. *)

  for pos = 0 to len - 1 do
  {
    if char_code table str.(pos) = 0 then do
    {
      for i = max (pos - right_min + 1) 0 to
              min (pos + left_min)    len do
      {
        numbers.(i) := 0
      }
    }
    else ()
  };

  Array.map (fun x -> x land 1 <> 0) numbers
};


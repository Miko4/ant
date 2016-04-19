
open Runtime;
open Unicode;
open Types;

(* parsing *)

value skip_comment stream = do
{
  while UCStream.pop stream != 10 do
  {
    ()
  }
};

value rec skip_blanks stream = do
{
  let c = UCStream.next_char stream;

  if 0 <= c && c <= 32 then do
  {
    UCStream.pop stream;
    skip_blanks stream;
  }
  else if c = 37 then       (* % *)
    skip_comment stream
  else
    ()
};

value read_next_word stream = do
{
  skip_blanks stream;

  iter ()

  where rec iter () = do
  {
    let c = UCStream.pop stream;

    if c <= 32 || c = 37 then
      []
    else
      [c :: iter ()]
  }
};

value parse_file stream = do
{
  let section_header word = match (UString.to_string word) with
  [ "substitute:" -> `Classes
  | "patterns:"   -> `Patterns
  | "exceptions:" -> `Exceptions
  | "language:"   -> `Language
  | _             -> `None
  ];

  let classes    = ListBuilder.make ();
  let patterns   = ListBuilder.make ();
  let exceptions = ListBuilder.make ();

  let rec read_classes language = do
  {
    let w = read_next_word stream;

    if w = [] then
      (language,
       ListBuilder.get classes,
       ListBuilder.get patterns,
       ListBuilder.get exceptions)
    else
      match section_header w with
      [ `Classes    -> read_classes    language
      | `Patterns   -> read_patterns   language
      | `Exceptions -> read_exceptions language
      | `Language   -> read_language   language
      | `None       -> do
        {
          ListBuilder.add classes w;
          read_classes language
        }
      ]
  }
  and read_patterns language = do
  {
    let w = read_next_word stream;

    if w = [] then
      (language,
       ListBuilder.get classes,
       ListBuilder.get patterns,
       ListBuilder.get exceptions)
    else
      match section_header w with
      [ `Classes    -> read_classes    language
      | `Patterns   -> read_patterns   language
      | `Exceptions -> read_exceptions language
      | `Language   -> read_language   language
      | `None       -> do
        {
          ListBuilder.add patterns w;
          read_patterns language
        }
      ]
  }
  and read_exceptions language = do
  {
    let w = read_next_word stream;

    if w = [] then
      (language,
       ListBuilder.get classes,
       ListBuilder.get patterns,
       ListBuilder.get exceptions)
    else
      match section_header w with
      [ `Classes    -> read_classes    language
      | `Patterns   -> read_patterns   language
      | `Exceptions -> read_exceptions language
      | `Language   -> read_language   language
      | `None       -> do
        {
          ListBuilder.add exceptions w;
          read_exceptions language
        }
      ]
  }
  and read_language language = do
  {
    let w = read_next_word stream;

    if w = [] then
      (language,
       ListBuilder.get classes,
       ListBuilder.get patterns,
       ListBuilder.get exceptions)
    else
      match section_header w with
      [ `Classes    -> read_classes    language
      | `Patterns   -> read_patterns   language
      | `Exceptions -> read_exceptions language
      | `Language   -> read_language   language
      | `None       -> read_language   w
      ]
  };

  find_section ()

  where rec find_section () = do
  {
    match section_header (read_next_word stream) with
    [ `Classes    -> read_classes    []
    | `Patterns   -> read_patterns   []
    | `Exceptions -> read_exceptions []
    | `Language   -> read_language   []
    | `None       -> find_section ()
    ]
  }
};

value empty_map = Charmap.create 0;

value make_charmap classes = do
{
  let map = Charmap.copy empty_map;

  iter 1 classes

  where rec iter n classes = match classes with
  [ []      -> map
  | [c::cs] -> do
    {
      List.iter (fun x -> Charmap.set map x n) c;

      iter (n+1) cs
    }
  ]
};

value parse_pattern class_table pattern = do
{
  iter pattern [] [0]

  where rec iter pattern ps ns = match pattern with
  [ []      -> (List.rev ps, List.rev ns)
  | [c::cs] -> if c >= 48 && c <= 57 then
                 iter cs ps [c - 48 :: List.tl ns]                        (* number *)
               else
                 iter cs [Charmap.lookup class_table c :: ps] [0 :: ns]   (* character *)
  ]
};

value force_break = 11;
value force_none  = 10;

value parse_exception class_table except = do
{
  (*
    We append code 0 characters at both sides of the word, insert a break of
    value 11 at every position with a hyphen and the value 10 at all other
    positions. These values are greater then all numbers in the patterns.
  *)

  iter except [0] [force_none; force_none]

  where rec iter except es hs = match except with
  [ []      -> (List.rev [0 :: es], List.rev [force_none :: hs])
  | [c::cs] -> if c = 45 then
                 iter cs es [force_break :: List.tl hs]
               else
                 iter cs [Charmap.lookup class_table c :: es] [force_none :: hs]
  ]
};

value build_trie max_code entries = do
{
  let rec merge i entries = match entries with
  [ []                      -> []
  | [(_, _, _, p, e) :: es] ->
      List.rev_map (fun (str, ns) -> (Array.of_list [i :: str], Array.of_list ns)) p
    @ List.rev_map (fun (str, ns) -> (Array.of_list [i :: str], Array.of_list ns)) e
    @ merge (i + 1) es
  ];

  Trie.build max_code (merge 0 entries)
};

value build_table data = do
{
  let parse_entry (l, c, p, e) = do
  {
    let cmap = make_charmap c;

    (List.length c,
     Array.of_list l,
     cmap,
     List.rev_map (parse_pattern cmap)   p,
     List.rev_map (parse_exception cmap) e)
  };

  let entries  = List.rev_map parse_entry data;

  let max_code = List.fold_left (fun x (y,_,_,_,_) -> max x y) 0 entries;

  let trie = build_trie max_code entries;

  (trie, iter 0 entries)

  where rec iter i entries = match entries with
  [ []                      -> []
  | [(_, l, c, _, _) :: xs] ->
      [(l,
        {
          Hyphenation.ht_char_classes  = c;
          Hyphenation.ht_pattern_trie  = trie;
          Hyphenation.ht_pattern_start = Trie.get_offset trie i
        })
       :: iter (i+1) xs]
  ]
};


(* output routines *)

value print_list p l = do
{
  print_string "[";

  iter l

  where rec iter l = match l with
  [ []  -> print_string "]"
  | [x] -> do
    {
      p x;
      print_string "]"
    }
  | [x::xs] -> do
    {
      p x;
      print_string "; ";
      iter xs
    }
  ]
};

value print_array p newline a = do
{
  let len = Array.length a;

  print_string "[|";

  if len > 1 then
    for i = 0 to len-2 do
    {
      p a.(i);
      if newline then
        print_string ";\n"
      else
        print_string "; "
    }
  else ();

  if len > 0 then
    p a.(len - 1)
  else ();

  print_string "|]"
};

value dump_trie trie p = do
{
  print_string "{ t_tree = ";
  print_array print_int False trie.Trie.t_tree;
  print_string "; t_data = ";
  print_array (print_array p False) True trie.Trie.t_data;
  print_string "; t_data_len = ";
  print_int    trie.Trie.t_data_len;
  print_string "};\n"
};

value dump_charmap cmap = do
{
  let is_empty cmap n = do
  {
    iter (256 * n)

    where rec iter i = do
    {
      if i = 256 * (n + 1) then
        True
      else if Charmap.lookup cmap i <> 0 then
        False
      else
        iter (i+1)
    }
  };

  print_string "Unicode.Charmap.build [|";

  for n = 0 to 255 do
  {
    if is_empty cmap n then
      print_string "empty_map"
    else do
    {
      let a = Array.init 256 (fun i -> Charmap.lookup cmap (256 * n + i));

      print_array print_int False a
    };

    if n < 255 then
      print_string ";\n"
    else
      print_string "\n"
  };

  print_string "|]"
};

value dump_table (lang, ht) = do
{
  print_string "(";
  print_array print_int False lang;
  print_string ",\n";
  print_string "{ ht_char_classes = ";
  dump_charmap ht.Hyphenation.ht_char_classes;
(*  print_string ";\n  ht_exceptions = ";
  dump_trie ht.Hyphenation.ht_exceptions
    (fun b -> if b then print_string "True" else print_string "False");*)
  print_string ";\n  ht_pattern_trie  = main_trie";
  print_string ";\n  ht_pattern_start = ";
  print_int ht.Hyphenation.ht_pattern_start;
  print_string "\n})\n";
};

value output_table (trie, tables) = do
{
  print_endline "open Runtime;";
  print_endline "open Trie;";
  print_endline "open Hyphenation;";
  print_endline "value main_trie =";
  dump_trie trie print_int;
  print_endline ";";
  print_endline "value empty_map =";
  print_array print_int False (Array.make 256 0);
  print_endline ";";
  print_endline "value tables =";
  print_list dump_table tables;
  print_endline ";"
};

value main () = do
{
  UString.set_string_format `UTF8;

  output_table (build_table (iter 1))

  where rec iter i = do
  {
    if i >= Array.length Sys.argv then
      []
    else do
    {
      let stream = UCStream.of_file Sys.argv.(i);

      let x = parse_file stream;

      [x :: iter (i+1)]
    }
  }
};

main ();


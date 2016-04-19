
open XNum;
open Unicode.Types;
open Logging;
open Dim;
open Substitute;
open FontMetric;

type simple_glyph_item   'box 'cmd = glyph_item font_metric 'box 'cmd;
type extended_glyph_item 'box 'cmd = glyph_item (font_metric * glyph_composer font_metric 'box 'cmd) 'box 'cmd;

(* debugging aids *)

value dump_item i = match i with
[ `Char c      -> log_uc_list [c]
| `Glyph (g,f) -> match get_unicode f g with
                  [ [||] -> log_string "?"
                  | str  -> log_uc_string str
                  ]
| `Kern _      -> log_string "¸"
| `Command _   -> log_string "!"
| `Box _       -> log_string "[]"
| `Break _     -> log_string "|"
];

value dump_item_list items = do
{
  log_string ">";
  List.iter dump_item items;
  log_string "<\n"
};

value dump_item_array items = do
{
  log_string ">";
  Array.iter dump_item items;
  log_string "<\n"
};

(* auxilliary functions *)

(*
  |convert_to_glyphs <font> <items>| replaces
  all |`Character| items in <items> by the corresponding |`Glyph| item.
*)

value rec convert_to_glyph font composer item = match item with
[ `Char c                  -> `Glyph (FontMetric.get_glyph font c, (font,composer))
| `Break (p,h,pre,post,no) -> `Break (p, h, (convert_to_glyphs font composer) pre,
                                            (convert_to_glyphs font composer) post,
                                            (convert_to_glyphs font composer) no)
| (`Kern _    as i)        -> i
| (`Command _ as i)        -> i
| (`Box _     as i)        -> i
]
and convert_to_glyphs font composer items = do
{
  let new_items = Array.make (List.length items) (`Glyph (Undef, (font,composer)));

  iter 0 items

  where rec iter k items = match items with
  [ []        -> new_items
  | [i :: is] -> do
    {
      new_items.(k) := convert_to_glyph font composer i;
      iter (k+1) is
    }
  ]
};

(* hyphenation *)

type hyphen_params =
{
  hyphen_table      : Hyphenation.hyphen_table;
  hyphen_penalty    : num;
  ex_hyphen_penalty : num;
  left_hyphen_min   : int;
  right_hyphen_min  : int;
  script_lang       : uc_string
};

(*
  |convert_to_glyphs_and_add_breaks <hyphen-params> <font> <items>| replaces
  all |`Character| items in <items> by the corresponding |`Glyph| item.
  It also performs hyphenation and inserts break points.
  The return value is a pair (<new-items>, <break-pos>) consisting of the obtained
  array of items and an array with the positions of all break points. The latter
  one is deliminated entries with value -1 and <length-of-new-items>.
*)

value rec convert_to_glyphs_and_add_breaks hyphen_params font composer items = do
{
  (* Collects all characters in <items> and returns them in an array.
     The boundary -1 is added as first and last element. *)

  let rec collect_characters n chars items = match items with
  [ [] -> do
    {
      let word = Array.make (n+2) (-1);

      List.fold_left
        (fun i c -> do
          {
            word.(i) := c;
            i - 1
          })
        n
        chars;

      word
    }
  | [`Char c :: is] -> collect_characters (n+1) [c :: chars] is
  | [_       :: is] -> collect_characters n     chars        is
  ];

  let hg     = get_hyphen_glyph font;
  let word   = collect_characters 0 [] items;
  let breaks = Hyphenation.hyphenate
                 hyphen_params.hyphen_table
                 hyphen_params.left_hyphen_min
                 hyphen_params.right_hyphen_min
                 word;

  let new_items = ListBuilder.make ();

  iter 2 items

  where rec iter k items = match items with
  [ []      -> ListBuilder.get new_items
  | [i::is] -> match i with
    [ `Char c -> do
      {
        let glyph = `Glyph (get_glyph font c, (font,composer));

        ListBuilder.add new_items glyph;

        if breaks.(k) then
          ListBuilder.add new_items
            (`Break (hyphen_params.hyphen_penalty, True, [|`Glyph (hg, (font,composer))|], [||], [||]))
        else ();

        iter (k+1) is
      }
    | `Break (p,h,pre,post,no) -> do
      {
        ListBuilder.add new_items
          (`Break (p, h, (convert_to_glyphs font composer pre),
                         (convert_to_glyphs font composer post),
                         (convert_to_glyphs font composer no)));
        iter k is
      }
    | (`Kern _ as i) -> do
      {
        ListBuilder.add new_items i;
        iter k is
      }
    | (`Command _ as i) -> do
      {
        ListBuilder.add new_items i;
        iter k is
      }
    | (`Box _ as i) -> do
      {
        ListBuilder.add new_items i;
        iter k is
      }
    ]
  ]
};

(* high-level interfaces to add ligatures and kerning *)

value rec strip_composer item = match item with
[ `Glyph (g,(f,_))             -> `Glyph (g,f)
| `Break (p, h, pre, post, no) -> `Break (p, h,
                                          Array.map strip_composer pre,
                                          Array.map strip_composer post,
                                          Array.map strip_composer no)
| (`Box     _ as i) -> i
| (`Command _ as i) -> i
| (`Kern    _ as i) -> i
];

(* |find_word_list <builder> <keep-breaks> <composer> <items>| adds the prefix of <items>
   to <builder> until a glyph with a different composer is encountered. It returns the remaining
   items and the number of items moved. *)

value rec find_word_list builder keep_breaks composer items = do
{
  iter 1 items

  where rec iter n items = match items with
  [ []      -> ([], n)
  | [i::is] -> match i with
    [ `Glyph (g,(f,c)) -> do
      {
        if c == composer then do  (* We assume that |c == composer| implies |f == font|. *)
        {
          ListBuilder.add builder (`Glyph (g,f));
          iter (n+1) is
        }
        else
          (items, n)
      }
    | (`Command _ as i) -> do
      {
        ListBuilder.add builder i;
        iter (n+1) is
      }
    | (`Kern    _ as i) -> do
      {
        ListBuilder.add builder i;
        iter (n+1) is
      }
    | `Break _ -> do
      {
        if keep_breaks then
          ListBuilder.add builder (strip_composer i)
        else ();

        iter (n+1) is
      }
    | `Box _ -> (items, n)
    ]
  ]
};

(* |find_word_array <builder> <keep-breaks> <composer> <form> <to> <items>| is the array version
   of |find_word_list|. It returns the starting position of the remaining items. *)

value rec find_word_array builder keep_breaks composer from_pos to_pos items = do
{
  if from_pos > to_pos then
    from_pos
  else do
  {
    let i = items.(from_pos);

    match i with
    [ `Glyph (g,(f,c)) -> do
      {
        if c == composer then do
        {
          ListBuilder.add builder (`Glyph (g,f));
          find_word_array builder keep_breaks composer (from_pos+1) to_pos items
        }
        else
          from_pos
      }
    | (`Command _ as i) -> do
      {
        ListBuilder.add builder i;
        find_word_array builder keep_breaks composer (from_pos+1) to_pos items
      }
    | (`Kern    _ as i) -> do
      {
        ListBuilder.add builder i;
        find_word_array builder keep_breaks composer (from_pos+1) to_pos items
      }
    | `Break _ -> do
      {
        if keep_breaks then
          ListBuilder.add builder (strip_composer i)
        else ();

        find_word_array builder keep_breaks composer (from_pos+1) to_pos items
      }
    | `Box _ -> from_pos
    ]
  }
};

value rec add_lig_kern keep_breaks items = do
{
  let word   = ListBuilder.make ();
  let result = ListBuilder.make ();

  iter items

  where rec iter items = match items with
  [ []      -> ListBuilder.get result
  | [i::is] -> match i with
    [ `Glyph (g, (f, composer)) -> do
      {
        ListBuilder.add word (`Glyph (g,f));

        let (rest, _) = find_word_list word keep_breaks composer is;

        ListBuilder.add_list result (composer (ListBuilder.get word));

        iter rest
      }
    | _ -> do
      {
        ListBuilder.add result (strip_composer i);
        iter is
      }
    ]
  ]
};

value add_lig_kern_iterative_list keep_breaks prefix items = do
{
  let word = ListBuilder.make ();

  iter prefix 0 items

  where rec iter prefix len items = match items with
  [ []      -> (prefix, len, [])
  | [i::is] -> match i with
      [ `Glyph (g, (f, composer)) -> do
        {
          ListBuilder.add word (`Glyph (g,f));

          let (rest,n) = find_word_list word keep_breaks composer is;

          match rest with
          [ [] -> (prefix, len, items)
          | _  -> iter (List.rev_append (composer (ListBuilder.get word)) prefix)
                       (len + n)
                       rest
          ]
        }
      | _ -> iter [strip_composer i :: prefix] (len + 1) is
      ]
  ]
};

value add_lig_kern_iterative_array keep_breaks prefix from_pos to_pos items = do
{
  let word = ListBuilder.make ();

  iter prefix from_pos

  where rec iter prefix current_pos = do
  {
    if current_pos > to_pos then
      (prefix, current_pos)
    else do
    {
      let i = items.(current_pos);

      match i with
      [ `Glyph (g, (f, composer)) -> do
        {
          ListBuilder.add word (`Glyph (g,f));

          let pos = find_word_array word keep_breaks composer (current_pos+1) to_pos items;

          if pos > to_pos then
            (prefix, current_pos)
          else
            iter (List.rev_append (composer (ListBuilder.get word)) prefix)
                 pos
        }
      | _ -> iter
               [strip_composer i :: prefix]
               (current_pos + 1)
      ]
    }
  }
};

(* |add_lig_kern_finish <prefix> <from> <to> <items>| processes those items
   left over by |add_lig_kern_iterative|. In particular, it assume that <from>
   is the value returned by |add_lig_kern_iterative|, that is, the item at
   position <from> is a glyph and the items between <from> and <to> form a
   single word. *)

value add_lig_kern_finish prefix from_pos to_pos items = do
{
  let rec get_word list to_pos = do
  {
    if from_pos > to_pos then
      list
    else
      get_word [strip_composer items.(to_pos) :: list] (to_pos-1)
  };
  if from_pos > to_pos then
    prefix
  else match items.(from_pos) with
  [ `Glyph (_, (_, composer)) -> do
    {
      List.rev_append (composer (get_word [] to_pos)) prefix
    }
  | _ -> assert False
  ]
};


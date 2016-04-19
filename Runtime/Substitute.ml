
open XNum;
open Maps;
open Unicode.Types;
open Logging;
open Dim;

(* glyph descriptions *)

type border_glyph =
[ Space   (* for kerning with spaces                *)
| Margin  (* for margin kerning                     *)
| Foreign (* for kerning with glyphs of other fonts *)
];

type glyph_desc =
[ Undef
| Simple     of int
| Accent     of int and int
| Sequence   of list int
| Extendable of glyph_desc and glyph_desc and glyph_desc and glyph_desc
| Border     of border_glyph
];

module OrderedGlyphDesc =
struct

  type t = glyph_desc;

  value compare = (compare : glyph_desc -> glyph_desc -> int);

end;

module GlyphMap  = Map.Make(OrderedGlyphDesc);
module GlyphSet  = Set.Make(OrderedGlyphDesc);
module GlyphTrie = DynamicTrie.Make(OrderedGlyphDesc);

type glyph_item 'font 'box 'cmd =
[= `Glyph of (glyph_desc * 'font)
|  `Kern of (num * num)
|  `Box of 'box
|  `Command of 'cmd
|  `Break of (num * bool
                  * array (glyph_item 'font 'box 'cmd)
                  * array (glyph_item 'font 'box 'cmd)
                  * array (glyph_item 'font 'box 'cmd))
];

type adjustment_command =
[ ConstGlyph of glyph_desc
| ConstKern of num and num
| CopyGlyph of int             (* glyph index               *)
| CopyCommands of int and int  (* first index, second index *)
];

type adjustment = (list adjustment_command * int);

type adjustment_table =
[ NoAdjustment
| DirectLookup of DynUCTrie.t adjustment
| ClassLookup of IntMap.t int and DynUCTrie.t adjustment
| ClassPairLookup of int and IntMap.t int and IntMap.t int and array adjustment
| AnchorPair of IntMap.t (num * num) and IntMap.t (num * num)
];

type glyph_composer 'font 'box 'cmd =
  list (glyph_item 'font 'box 'cmd) ->
  list (glyph_item 'font 'box 'cmd);

type substitution 'font 'box 'cmd =
  list (glyph_item 'font 'box 'cmd * list (glyph_item 'font 'box 'cmd)) ->
  option (list (glyph_item 'font 'box 'cmd * list (glyph_item 'font 'box 'cmd)) *
          list (glyph_item 'font 'box 'cmd * list (glyph_item 'font 'box 'cmd)) *
          adjustment);

type subst_trie 'a = (('a -> bool) * ('a -> uc_char -> 'a) * ('a -> option adjustment));

type adj_trie_state = (int * list int);

(* auxilliary functions *)

(* |first_real_item <items> <from> <to>| and |last_real_item <items> <from> <to>|
   return the the first/last |`Glyph| or |`Box| between <from> and <to> in <items>.
   |first_real_item_list <items>| is the list version of |first_real_item|. *)

value rec first_real_item_list items = match items with
[ []      -> None
| [i::is] -> match i with
    [ `Glyph _
    | `Box _   -> Some i
    | _        -> first_real_item_list is
    ]
];

value first_real_item items from_pos to_pos = do
{
  iter from_pos

  where rec iter i = do
  {
    if i > to_pos then
      None
    else match items.(i) with
    [ `Glyph _
    | `Box _   -> Some items.(i)
    | _        -> iter (i+1)
    ]
  }
};

value last_real_item items from_pos to_pos = do
{
  iter to_pos

  where rec iter i = do
  {
    if i < from_pos then
      None
    else match items.(i) with
    [ `Glyph _
    | `Box _   -> Some items.(i)
    | _        -> iter (i-1)
    ]
  }
};

(* adjustments *)

value single_positioning_cmd pre_kern shift post_kern = do
{
  [ConstKern pre_kern shift;
   CopyGlyph 0;
   ConstKern post_kern num_zero;
   CopyCommands 0 0]
};

value simple_pair_kerning_cmd kern = do
{
  [CopyGlyph 0;
   CopyCommands 0 0;
   ConstKern kern num_zero;
   CopyGlyph 1;
   CopyCommands 1 1]
};

value pair_positioning_cmd pre_kern shift1 mid_kern shift2 post_kern = do
{
  [ConstKern pre_kern shift1;
   CopyGlyph 0;
   CopyCommands 0 0;
   ConstKern mid_kern shift2;
   CopyGlyph 1;
   ConstKern post_kern num_zero;
   CopyCommands 1 1]
};

(* This does not work in general. We need a new adjustment_command! *)

value anchor_pair_cmd (x1,y1) (x2,y2) = do
{
  (* FIX: What if the first glyph is already shifted? *)
  [CopyGlyph 0;
   CopyCommands 0 0;
   ConstKern (x1 -/ x2) (y1 -/ y2);
   CopyGlyph 1;
   CopyCommands 1 1]
};

value replace_with_single_glyph_cmd num_glyphs new_glyph = do
{
  [ConstGlyph new_glyph;
   CopyCommands 0 (num_glyphs-1)]
};

value replace_with_multiple_glyphs_cmd num_glyphs new_glyphs = do
{
  Array.fold_right
    (fun g cmds -> [ConstGlyph g :: cmds])
    new_glyphs
    [CopyCommands 0 (num_glyphs-1)]
};

value tex_ligature_cmd lig keep1 keep2 = do
{
  if keep1 then
    if keep2 then
      [CopyGlyph 0; ConstGlyph lig; CopyCommands 0 0; CopyGlyph 1; CopyCommands 1 1]
    else
      [CopyGlyph 0; ConstGlyph lig; CopyCommands 0 1]
  else
    if keep2 then
      [ConstGlyph lig; CopyCommands 0 0; CopyGlyph 1; CopyCommands 1 1]
    else
      [ConstGlyph lig; CopyCommands 0 1]
};

value lookup_adjustment adj_table glyphs = match adj_table with
[ NoAdjustment                -> None
| DirectLookup trie           -> DynUCTrie.lookup_list glyphs trie
| ClassLookup classes trie    -> do
  {
    let c = List.map
             (fun g -> try IntMap.find g classes
                       with [ Not_found -> 0])
             glyphs;

    DynUCTrie.lookup_list c trie
  }
| ClassPairLookup n c1 c2 arr -> match glyphs with
  [ [g1; g2] -> do
    {
      try
        let i1 = IntMap.find g1 c1;
        let i2 = IntMap.find g2 c2;

        Some arr.(n * i1 + i2)
      with
      [ Not_found -> None ]
    }
  | _ -> None
  ]
| AnchorPair entry exit -> match glyphs with
  [ [g1; g2] -> do
    {
      try
        let a = IntMap.find g1 exit;
        let b = IntMap.find g2 entry;

        (* FIX: Replace the x coordinate of a = (x,y) by glyph_width - x *)
        Some (anchor_pair_cmd a b, 1)
      with
      [ Not_found -> None ]
    }
  | _ -> None
  ]
];

value get_lookup_depth adj = match adj with
[ NoAdjustment            -> 0
| DirectLookup trie       -> DynUCTrie.depth trie
| ClassLookup _ trie      -> DynUCTrie.depth trie
| ClassPairLookup _ _ _ _ -> 2
| AnchorPair _ _          -> 2
];

value lookup_adjustments adjustments glyphs = do
{
  iter adjustments

  where rec iter adjustments = match adjustments with
  [ []        -> None
  | [a::adjs] -> match lookup_adjustment a glyphs with
    [ None          -> iter adjs
    | (Some _) as r -> r
    ]
  ]
};

value max_adjustment_depth adjustments = do
{
  List.fold_left
    (fun depth adj -> max depth (get_lookup_depth adj))
    0
    adjustments
};

value lookup2_adjustments adjustments glyphs = do
{
  iter1 adjustments

  where rec iter1 adjustments = match adjustments with
  [ []        -> None
  | [a::adjs] -> do
    {
      iter2 0

      where rec iter2 i = do
      {
        if i >= Array.length a then
          iter1 adjs
        else match lookup_adjustment a.(i) glyphs with
        [ None          -> iter2 (i+1)
        | (Some _) as r -> r
        ]
      }
    }
  ]
};

value max2_adjustment_depth adjustments = do
{
  List.fold_left
    (fun depth adj ->
      Array.fold_left
        (fun depth a -> max depth (get_lookup_depth a))
        depth
        adj)
    0
    adjustments
};

value make_adjustment_trie adjustments = do
{
  let max_depth = max_adjustment_depth adjustments;

  let is_empty (n, _)        = (n > max_depth);
  let prefix (n, glyphs) g   = (n+1, [g :: glyphs]);
  let root_value (_, glyphs) =
    lookup_adjustments adjustments (List.rev glyphs);

  ((is_empty, prefix, root_value), (0, []))
};

(* applying adjustments *)

value match_substitution_trie get_border_glyph (is_empty, prefix_trie, root_value) subst_trie items = do
{
  let return_match found = match found with
  [ (_,      _,    None)     -> None
  | (prefix, rest, Some adj) -> Some (List.rev prefix, rest, adj)
  ];

  iter ([], items, None) [] items subst_trie

  where rec iter found prefix items trie = match items with
  [ [((`Glyph (g,_), _) as i) :: is] -> do
    {
      let rec match_with_glyph g = match g with
      [ Simple x -> do
        {
          let new_prefix = [i :: prefix];
          let next       = prefix_trie trie x;

          match root_value next with
          [ Some _ as adj -> iter (new_prefix, is, adj) new_prefix is next
          | None          -> iter found                 new_prefix is next
          ]
        }
      | Border b -> match_with_glyph (get_border_glyph b)
      | _        -> return_match found
      ];

      if is_empty trie then
        return_match found
      else
        match_with_glyph g
    }
  | _ -> return_match found
  ]
};

(*
value match_substitution_trie (*border_kerning*) (is_empty, prefix_trie, root_value) subst_trie items = do
{
  let return_match found = match found with
  [ (_,      _,    None)     -> None
  | (prefix, rest, Some adj) -> Some (prefix, rest, adj)
  ];

  iter ([], items, None) [] items subst_trie

  where rec iter found prefix items trie = match items with
  [ []                          -> return_match found
  | [(`Glyph (g,_) as i) :: is] -> do
    {
      if is_empty trie then
        return_match found
      else match g with
      [ Simple x -> do
        {
          let new_prefix = [i :: prefix];
          let next       = prefix_trie trie x;

          match root_value next with
          [ Some _ as val -> iter (new_prefix, is, val) new_prefix is next
          | None          -> iter found                 new_prefix is next
          ]
        }
      | _ -> return_match found
      ]
    }
  | [i :: is] -> iter found [i :: prefix] is trie
  ]
};
*)

value match_substitution_dyntrie get_border_glyph subst_trie items = do
{
  match_substitution_trie get_border_glyph (DynUCTrie.is_empty, DynUCTrie.prefix, DynUCTrie.root_value) subst_trie items
};


value apply_substitution font glyphs rest adjustments = do
{
  iter [] (`Glyph (Undef, font)) [] adjustments

  where rec iter result g cmds adjustment = match adjustment with
  [ [] -> match List.rev_append result [(g, List.rev cmds) :: rest] with
    [ [(_, first_cmds) :: glyphs] -> (first_cmds, glyphs)
    | []                          -> assert False
    ]
  | [a::adjs] -> match a with
    [ ConstGlyph glyph       -> iter [(g, List.rev cmds) :: result]
                                     (`Glyph (glyph, font))
                                     []
                                     adjs
    | CopyGlyph idx          -> iter [(g, List.rev cmds) :: result]
                                     (fst (List.nth glyphs idx))
                                     []
                                     adjs
    | ConstKern x y          -> if x <>/ num_zero || y <>/ num_zero then
                                  iter result g [`Kern x y :: cmds] adjs
                                else
                                  iter result g cmds adjs
    | CopyCommands idx1 idx2 -> do
      {
        (* FIX: also collect movable commands before and after the interval? *)

        add (idx2 - idx1) cmds (XList.drop idx1 glyphs)

        where rec add i cmds old_cmds = do
        {
          if i < 0 then
            iter result g cmds adjs
          else match old_cmds with
          [ []            -> iter result g cmds adjs
          | [(_,c) :: cs] -> add (i-1) (List.rev_append c cmds) cs
          ]
        }
      }
    ]
  ]
};

value separate_items items = do
{
  let rec get_commands cmds items = match items with
  [ [] -> (List.rev cmds, [])
  | [i :: is] -> match i with
    [ `Command _
    | `Break _
    | `Kern _  -> get_commands [i :: cmds] is
    | `Box _
    | `Glyph _ -> (List.rev cmds, items)
    ]
  ];

  let (first_commands, items) = get_commands [] items;

  iter [] items

  where rec iter result items = match items with
  [ []        -> (first_commands, List.rev result)
  | [g :: is] -> do
    {
      (* <g> is either a `Box or a `Glyph *)

      let (cmds, rest) = get_commands [] is;

      iter [(g,cmds) :: result] rest
    }
  ]
};

value substitute font find_subst items = do
{
  let result = ListBuilder.make ();

  let (first_commands, glyphs) = separate_items items;

  ListBuilder.add_list result first_commands;

  (* |skip_prefix <skip> <glyphs>| moves the first <skip> glyphs from <glyphs> to <result>. *)

  let rec skip_prefix skip glyphs = do
  {
    if skip <= 0 then
      glyphs
    else match glyphs with
    [ []               -> []
    | [(g,cmds) :: gs] -> do
      {
        ListBuilder.add      result g;
        ListBuilder.add_list result cmds;

        skip_prefix (skip - 1) gs
      }
    ]
  };

  iter glyphs

  where rec iter glyphs = match glyphs with
  [ [] -> ListBuilder.get result
  | _  -> match find_subst glyphs with
    [ Some (prefix, rest, (adj, skip)) -> do
      {
        let (cmds, new_glyphs) = apply_substitution font prefix rest adj;

        ListBuilder.add_list result cmds;

        iter (skip_prefix skip new_glyphs)
      }
    | None -> iter (skip_prefix 1 glyphs)
    ]
  ]
};

(*
value apply_substitution font glyphs cmds rest adjustment = do
{
  iter adjustment

  where rec iter adjustment = match adjustment with
  [ []      -> rest
  | [r::rs] -> match r with
    [ ConstGlyph glyph       -> [`Glyph (glyph, font) :: iter rs]
    | ConstKern x y          -> [`Kern x y            :: iter rs]
    | CopyGlyph idx          -> [`Glyph glyphs.(idx)  :: iter rs]
    | CopyCommands idx1 idx2 -> do
      {
        (* FIX: also collect movable commands before and after the interval? *)

        copy idx2 (iter rs)

        where rec copy i lst = do
        {
          if i < idx1 then
            lst
          else
            copy (i-1) (cmds.(i) @ lst)
        }
      }
    ]
  ]
};

value separate_items items = do
{
  (* <items> are in reversed order *)

  let collect_commands items = do
  {
    iter [] items

    where rec iter cmds items = match items with
    [ [i :: is] -> match i with
      [ `Command _
      | `Break _
      | `Kern _  -> iter [i :: cmds] is
      | `Box _
      | `Glyph _ -> (cmds, items)
      ]
    | [] -> (cmds, items)
    ]
  };

  iter 0 [] [] items

  where rec iter n glyphs cmds items = match items with
  [ [] -> do
    {
      let glyph_arr = Array.of_list glyphs;

      (* <cmds> might lack the last entry, so we build the array manually. *)

      let cmd_arr   = Array.make (n+1) [];

      List.fold_left
        (fun i c -> do
          {
            cmd_arr.(i) := c;
            i+1
          })
        0
        cmds;

      (glyph_arr, cmd_arr)
    }
  | _  -> do
    {
      let (new_cmds, rest) = collect_commands items;

      match rest with
      [ [`Glyph gf :: is] -> iter (n+1) [gf :: glyphs] [new_cmds :: cmds] is
      | _                 -> iter n     glyphs         [new_cmds :: cmds] []
      ]
    }
  ]
};

value substitute font find_subst items = do
{
  let result = ListBuilder.make ();

  (* |skip_prefix <skip> <items>| moves the first <skip> glyphs from <items> to <result>. *)

  let rec skip_prefix skip items = do
  {
    if skip <= 0 then
      items
    else match items with
    [ []      -> []
    | [i::is] -> do
      {
        ListBuilder.add result i;

        match i with
        [ `Glyph _ -> skip_prefix (skip-1) is
        | _        -> skip_prefix skip     is
        ]
      }
    ]
  };

  iter items

  where rec iter items = match items with
  [ [] -> ListBuilder.get result
  | _  -> match find_subst items with
    [ Some (prefix, rest, (adj, skip)) -> do
      {
        let (glyphs, cmds) = separate_items prefix;
        let new_items      = apply_substitution font glyphs cmds rest adj;

        iter (skip_prefix skip new_items)
      }
    | None -> iter (skip_prefix 1 items)
    ]
  ]
};
*)


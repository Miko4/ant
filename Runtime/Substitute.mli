
open XNum;
open Maps;
open Unicode.Types;

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

module OrderedGlyphDesc :
sig

  type t = glyph_desc;

  value compare : glyph_desc -> glyph_desc -> int;

end;

module GlyphMap  : Map.S with type key = glyph_desc;
module GlyphSet  : Set.S with type elt = glyph_desc;
module GlyphTrie : DynamicTrie.S with type elt = glyph_desc;

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
| CopyGlyph of int
| CopyCommands of int and int
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

type adj_trie_state;

value first_real_item_list : list (glyph_item 'font 'box 'cmd) -> option (glyph_item 'font 'box 'cmd);
value first_real_item      : array (glyph_item 'font 'box 'cmd) -> int -> int -> option (glyph_item 'font 'box 'cmd);
value last_real_item       : array (glyph_item 'font 'box 'cmd) -> int -> int -> option (glyph_item 'font 'box 'cmd);

value single_positioning_cmd           : num -> num -> num -> list adjustment_command;
value simple_pair_kerning_cmd          : num -> list adjustment_command;
value pair_positioning_cmd             : num -> num -> num -> num -> num -> list adjustment_command;
value replace_with_single_glyph_cmd    : int -> glyph_desc -> list adjustment_command;
value replace_with_multiple_glyphs_cmd : int -> array glyph_desc -> list adjustment_command;
value tex_ligature_cmd                 : glyph_desc -> bool -> bool -> list adjustment_command;

value lookup2_adjustments   : list (array adjustment_table) -> uc_list -> option adjustment;
value max2_adjustment_depth : list (array adjustment_table) -> int;

value make_adjustment_trie : list adjustment_table -> (subst_trie adj_trie_state * adj_trie_state);

value match_substitution_dyntrie : (border_glyph -> glyph_desc) -> DynUCTrie.t adjustment -> substitution 'font 'box 'cmd;
value match_substitution_trie    : (border_glyph -> glyph_desc) -> subst_trie 'a -> 'a -> substitution 'font 'box 'cmd;

value substitute           : 'font -> substitution 'font 'box 'cmd -> glyph_composer 'font 'box 'cmd;


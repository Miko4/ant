
open Maps;

module Tag:
sig

type tag = int32;

value make_tag      : string -> tag;
value make_tag_uc   : Unicode.Types.uc_string -> tag;
value tag_to_string : tag -> string;

module OrderedTag : sig type t = tag; value compare : tag -> tag -> int; end;
module TagMap : Map.S with type key = tag;
module TagSet : Set.S with type elt = tag;

value latn_tag : tag;
value dflt_tag : tag;

end;

module OTF_Pos_Subst :
sig

open Tag;

type feature =
{
  f_tag         : tag;
  f_params      : int;
  f_lookups     : array int;
  f_script_list : TagMap.t tag
};

value flags_right_to_left        : int;
value flags_ignore_base_glyphs   : int;
value flags_ignore_ligatures     : int;
value flags_ignore_marks         : int;
value flags_mark_attachment_type : int;

type lang_sys =
{
  ls_required : option feature;
  ls_features : array feature
};

type positioning =
{
  p_x_off     : int;
  p_y_off     : int;
  p_h_adv_off : int;
  p_v_adv_off : int
};

type pos_rule_record =
{
  prr_seq_idx : int;
  prr_lookup  : lookup
}

and pos_subst_rule 'a =
{
  psr_data    : 'a;
  psr_lookups : array pos_rule_record
}

and pos_subst_command =
[ NoCommand
| Position             of IntMap.t positioning
| CursiveAnchors       of IntMap.t (int * int) and IntMap.t (int * int)
| MarkToBaseAnchors    of array (int * int * int * int) and array (int * array (int * int))
| MarkToLigAnchors     of array (int * int * int * int) and array (int * array (array (int * int)))
| MarkToMarkAnchors    of array (int * int * int * int) and array (int * array (int * int))
| Kern                 of IntMap.t (IntMap.t (positioning * positioning))
| KernClass            of int and IntMap.t int and IntMap.t int and array positioning and array positioning
| Substitution         of IntMap.t int
| Multiple             of IntMap.t (array int)
| Alternate            of IntMap.t (array int)
| Ligature             of DynUCTrie.t int
| ContextGlyphPos      of DynUCTrie.t (array pos_rule_record)
| ContextGlyphSubst    of DynUCTrie.t (array pos_rule_record)
| ContextClassPos      of IntMap.t int and array (pos_subst_rule (array int))
| ContextClassSubst    of IntMap.t int and array (pos_subst_rule (array int))
| ContextCoveragePos   of array (pos_subst_rule (array (array int)))
| ContextCoverageSubst of array (pos_subst_rule (array (array int)))
| ChainGlyphPos        of (array (pos_subst_rule (array int * array int * array int)))
| ChainGlyphSubst      of (array (pos_subst_rule (array int * array int * array int)))
| ChainClassPos        of IntMap.t int and IntMap.t int and IntMap.t int and
                          array (pos_subst_rule (array int * array int * array int))
| ChainClassSubst      of IntMap.t int and IntMap.t int and IntMap.t int and
                          array (pos_subst_rule (array int * array int * array int))
| ChainCoveragePos     of array (pos_subst_rule (array (array int) * array (array int) * array (array int)))
| ChainCoverageSubst   of array (pos_subst_rule (array (array int) * array (array int) * array (array int)))
| ReverseSubst         of array int and array int and array (array int) and array (array int)
]

and lookup =
{
  l_flags    : int;
  l_commands : array pos_subst_command
};

type size_params =
{
  design_size         : int;
  font_style_id       : int;
  font_style_name     : int;
  design_range_bottom : int;
  design_range_top    : int
};

type pos_subst_table =
{
  t_scripts : TagMap.t (TagMap.t lang_sys);
  t_lookups : array lookup;
  t_size    : option size_params
};

value empty_pos_subst : pos_subst_table;

value get_lookups : pos_subst_table -> tag -> tag -> TagSet.t -> list lookup;

end;

type glyph_metric =
{
  g_adv_width : int;
  g_min_x     : int;
  g_min_y     : int;
  g_max_x     : int;
  g_max_y     : int
};

type otf_tables = Tag.TagMap.t string;

type otf_font =
{
  otf_units_per_em : int;
  otf_glyphs       : array glyph_metric;
  otf_gpos         : option OTF_Pos_Subst.pos_subst_table;
  otf_gsub         : option OTF_Pos_Subst.pos_subst_table
};

value read_font_tables : string -> otf_tables;
value get_pos_subst    : otf_tables -> (option OTF_Pos_Subst.pos_subst_table * option OTF_Pos_Subst.pos_subst_table);
value is_cff           : otf_tables -> bool;
value get_cff          : otf_tables -> string;
value read_font        : string -> otf_font;
value write_subset     : IO.ostream -> otf_tables -> array int -> unit;


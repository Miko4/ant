
open XNum;
open Unicode.Types;
open Dim;
open Graphic;
open Substitute;
open GlyphMetric;

(* font metrics *)

type font_parameter =
{
  hyphen_glyph     : glyph_desc;
  skew_glyph       : glyph_desc;
  margin_glyph     : glyph_desc;
  space_glyph      : glyph_desc;
  foreign_glyph    : glyph_desc;
  slant            : num;
  space            : num;
  space_stretch    : num;
  space_shrink     : num;
  x_height         : num;
  quad             : num;
  extra_space      : num;
  num_shift_1      : num;
  num_shift_2      : num;
  num_shift_3      : num;
  denom_shift_1    : num;
  denom_shift_2    : num;
  super_shift_1    : num;
  super_shift_2    : num;
  super_shift_3    : num;
  sub_shift_1      : num;
  sub_shift_2      : num;
  super_drop       : num;
  sub_drop         : num;
  delim_1          : num;
  delim_2          : num;
  axis_height      : num;
  rule_thickness   : num;
  big_op_spacing_1 : num;
  big_op_spacing_2 : num;
  big_op_spacing_3 : num;
  big_op_spacing_4 : num;
  big_op_spacing_5 : num
};

type font_type =
[ PostScript
| OpenTypeCFF
| TrueType
| Other
];

type font_metric =
{
  name                : string;
  ps_name             : string;
  file_name           : string;
  font_type           : font_type;
  first_glyph         : int;
  last_glyph          : int;
  design_size         : num;
  at_size             : num;
  check_sum           : num;
  parameter           : font_parameter;
  get_glyph           : uc_char -> glyph_desc;
  get_unicode         : glyph_desc -> uc_string;
  get_composer        : !'box 'cmd . get_composer_type 'box 'cmd;
  kerning             : font_metric -> int -> int -> lig_kern;
  draw_simple_glyph   : font_metric -> int -> simple_box;
  accent_base_point   : font_metric -> glyph_metric -> (num * num);
  accent_attach_point : font_metric -> glyph_metric -> (num * num);
  get_glyph_bitmap    : font_metric -> uc_char -> GlyphBitmap.glyph;
  get_glyph_name      : int -> string;
  glyph_metric        : array glyph_metric
}

and get_composer_type 'box 'cmd = font_metric -> uc_string -> SymbolSet.t -> glyph_composer font_metric 'box 'cmd

and simple_box =
[ Empty
| SimpleGlyph of int and font_metric
| Rule of num and num
| Image of num and num and string and LoadImage.format
| Group of list (graphic_command num simple_box)
| Command of simple_cmd
]

and simple_cmd =
[= `DVI_Special of string
];

type glyph_spec =
[ GlyphIndex of int
| GlyphChar of uc_char
| GlyphName of string
];

type adjustment_spec =
[ AdjKern of num
| AdjLig  of glyph_spec
];

module GlyphSpecTrie : DynamicTrie.S with type elt = glyph_spec;

(* User specified modifications of font parameters. *)

type font_load_params =
{
  flp_size           : num;                                (* scale font to this size     *)
  flp_encoding       : array uc_string;                    (* overrides built in encoding *)
  flp_hyphen_glyph   : glyph_desc;                         (* specifies the hyphen glyph  *)  (* FIX: replace these two by *)
  flp_skew_glyph     : glyph_desc;                         (* specifies the skew glyph    *)  (* a complete font_parameter *)
  flp_letter_spacing : num;                                (* additional letter spacing   *)
  flp_extra_pos      : GlyphSpecTrie.t adjustment_spec;    (* additional kerning pairs and ligatures *)
  flp_extra_subst    : GlyphSpecTrie.t adjustment_spec;    (* additional kerning pairs and ligatures *)
  flp_extra_kern     : list (glyph_spec * extra_kern_info) (* kerning with border glyphs  *)
};

(* pages *)

type page =
{
  p_contents : simple_box;
  p_number   : int;
  p_width    : num;
  p_height   : num
};

value default_bitmap_resolution : ref int;
value default_mf_mode           : ref string;

value get_glyph        : font_metric -> uc_char -> glyph_desc;
value get_unicode      : font_metric -> glyph_desc -> uc_string;
value index_to_glyph   : font_metric -> int -> glyph_desc;
value glyph_exists     : font_metric -> int -> bool;

value glyph_spec_to_index : (uc_char -> int) -> (string -> int) -> glyph_spec -> int;

value simple_ligature_substitution : font_metric -> substitution font_metric 'box 'cmd;
value simple_composer              : font_metric
                                       -> substitution font_metric 'box 'cmd
                                       -> glyph_composer font_metric 'box 'cmd;
value two_phase_composer           : font_metric
                                       -> substitution font_metric 'box 'cmd
                                       -> substitution font_metric 'box 'cmd
                                       -> glyph_composer font_metric 'box 'cmd;
value add_border_kern              : int -> int -> int -> num -> list (int * extra_kern_info)
                                       -> list adjustment_table -> list adjustment_table;
value adjustment_spec_to_table     : (uc_char -> int) -> (string -> int)
                                       -> GlyphSpecTrie.t adjustment_spec -> adjustment_table;

value get_glyph_metric            : font_metric -> glyph_desc -> glyph_metric;
value next_glyph                  : font_metric -> glyph_desc -> glyph_desc;
value get_glyph_composer          : get_composer_type 'box 'cmd;
value get_lig_kern                : font_metric -> glyph_desc -> glyph_desc -> lig_kern;
value draw_simple_glyph           : font_metric -> int -> simple_box;
value draw_displaced_simple_glyph : num -> num -> font_metric -> int -> simple_box;
value draw_glyph                  : font_metric -> glyph_desc -> simple_box;

value get_hyphen_glyph  : font_metric -> glyph_desc;
value get_skew_glyph    : font_metric -> glyph_desc;
value get_margin_glyph  : font_metric -> glyph_desc;
value get_space_glyph   : font_metric -> glyph_desc;
value get_foreign_glyph : font_metric -> glyph_desc;
value get_border_glyph  : font_metric -> border_glyph -> glyph_desc;

value accent_base_point           : font_metric -> glyph_metric -> (num * num);
value accent_attach_point         : font_metric -> glyph_metric -> (num * num);
value accent_base_point_x_height  : font_metric -> glyph_metric -> (num * num);
value accent_attach_point_top     : font_metric -> glyph_metric -> (num * num);
value accent_position             : font_metric -> glyph_metric ->
                                    font_metric -> glyph_metric -> (num * num);
value construct_accent            : font_metric -> glyph_desc -> font_metric -> glyph_desc -> glyph_metric;

(* Shorthand to access the dimension of a normal and an extended space of the font. *)

value space_glue  : font_metric -> dim;
value xspace_glue : font_metric -> dim;

value empty_font        : font_metric;
value empty_parameter   : font_parameter;
value empty_load_params : font_load_params;


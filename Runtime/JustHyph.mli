
open XNum;
open Unicode.Types;
open Substitute;
open GlyphMetric;
open FontMetric;

type hyphen_params =
{
  hyphen_table      : Hyphenation.hyphen_table;
  hyphen_penalty    : num;
  ex_hyphen_penalty : num;
  left_hyphen_min   : int;
  right_hyphen_min  : int;
  script_lang       : uc_string
};

type simple_glyph_item   'box 'cmd = glyph_item font_metric 'box 'cmd;
type extended_glyph_item 'box 'cmd = glyph_item (font_metric * glyph_composer font_metric 'box 'cmd) 'box 'cmd;

value strip_composer : extended_glyph_item 'box 'cmd -> simple_glyph_item 'box 'cmd;

value convert_to_glyph  : font_metric -> glyph_composer font_metric 'box 'cmd ->
                            char_item 'box 'cmd -> extended_glyph_item 'box 'cmd;
value convert_to_glyphs : font_metric -> glyph_composer font_metric 'box 'cmd ->
                            list (char_item 'box 'cmd) -> array (extended_glyph_item 'box 'cmd);

value convert_to_glyphs_and_add_breaks : hyphen_params -> font_metric -> glyph_composer font_metric 'box 'cmd ->
                                           list (char_item 'box 'cmd) -> list (extended_glyph_item 'box 'cmd);

value add_lig_kern                 : bool -> list (extended_glyph_item 'box 'cmd) -> list (simple_glyph_item 'box 'cmd);
value add_lig_kern_iterative_list  : bool -> list (simple_glyph_item 'box 'cmd) -> list (extended_glyph_item 'box 'cmd)
                                     -> (list (simple_glyph_item 'box 'cmd) * int * list (extended_glyph_item 'box 'cmd));
value add_lig_kern_iterative_array : bool -> list (simple_glyph_item 'box 'cmd) -> int -> int -> array (extended_glyph_item 'box 'cmd)
                                     -> (list (simple_glyph_item 'box 'cmd) * int);
value add_lig_kern_finish          : list (simple_glyph_item 'box 'cmd) -> int -> int -> array (extended_glyph_item 'box 'cmd)
                                     -> list (simple_glyph_item 'box 'cmd);

value dump_item :
  [< `Char of uc_char
  |  `Glyph of (glyph_desc * font_metric)
  |  `Kern of (num * num)
  |  `Box of 'box
  |  `Command of 'cmd
  |  `Break of 'break
  ] -> unit;

value dump_item_list :
  list [< `Char of uc_char
       |  `Glyph of (glyph_desc * font_metric)
       |  `Kern of (num * num)
       |  `Box of 'box
       |  `Command of 'cmd
       |  `Break of 'break
       ] -> unit;

value dump_item_array :
  array [< `Char of uc_char
        |  `Glyph of (glyph_desc * font_metric)
        |  `Kern of (num * num)
        |  `Box of 'box
        |  `Command of 'cmd
        |  `Break of 'break
        ] -> unit;


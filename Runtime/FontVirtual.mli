
open XNum;
open Substitute;
open GlyphMetric;
open FontMetric;

type virtual_glyph =
{
  vg_width  : num;
  vg_height : num;
  vg_depth  : num;
  vg_italic : num;
  vg_glyph  : simple_box
};

value make_virtual_font : string -> num -> array virtual_glyph ->
                         list ((int * int) * GlyphMetric.lig_kern) -> array XNum.num -> font_metric;



open XNum;
open Runtime;
open Substitute;
open FontMetric;
open Box;

value attach_accent         : font_metric -> glyph_desc -> font_metric -> glyph_desc -> box;
value vertical_extendable   : num -> font_metric ->
                              glyph_desc -> glyph_desc -> glyph_desc -> glyph_desc -> box;
value horizontal_extendable : num -> font_metric ->
                              glyph_desc -> glyph_desc -> glyph_desc -> glyph_desc -> box;


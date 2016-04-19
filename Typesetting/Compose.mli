
open Runtime;
open Substitute;
open FontMetric;
open GlyphMetric;
open Box;

value discard_glue       : list (glyph_item 'font box box) -> (list (glyph_item 'font box box) * list (glyph_item 'font box box));
value discard_glue_array : int -> int -> array (glyph_item 'font box box) -> (list (glyph_item 'font box box) * int);

value just_hyph_builder  : font_metric -> glyph_composer -> JustHyph.hyphen_params ->
                           Builder.builder (list box);
value ligature_builder   : font_metric -> glyph_composer -> JustHyph.hyphen_params ->
                           Builder.builder (list box);
value char_item_builder  : font_metric -> glyph_composer -> JustHyph.hyphen_params ->
                           Builder.builder (list (char_item box box));
value hyph_only_builder  : font_metric -> glyph_composer -> JustHyph.hyphen_params ->
                           Builder.builder (list extended_glyph_item);
value glyph_item_builder : font_metric -> glyph_composer -> JustHyph.hyphen_params ->
                           Builder.builder (list simple_glyph_item);

value box_add_lig_kern   : list box -> list box;


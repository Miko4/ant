
open XNum;
open Unicode.Types;
open Runtime;
open Substitute;
open GlyphMetric;
open FontMetric;
open Box;

type builder_interface =
{
  add_char  : uc_char -> unit;
  add_break : num -> bool -> list (char_item box box)
                          -> list (char_item box box)
                          -> list (char_item box box) -> unit;
  add_kern  : num -> num -> unit;
  add_box   : box -> unit;
  add_cmd   : box -> unit;

  set_font          : font_metric -> glyph_composer -> unit;
  set_hyphen_params : JustHyph.hyphen_params -> unit
};

type builder 'a = (builder_interface * (unit -> 'a));

value add_char          : builder_interface -> uc_char -> unit;
value add_break         : builder_interface -> num -> bool
                            -> list (char_item box box)
                            -> list (char_item box box)
                            -> list (char_item box box) -> unit;
value add_kern          : builder_interface -> num -> num -> unit;
value add_box           : builder_interface -> box -> unit;
value add_cmd           : builder_interface -> box -> unit;
value set_font          : builder_interface -> font_metric -> glyph_composer -> unit;
value set_hyphen_params : builder_interface -> JustHyph.hyphen_params -> unit;
value add_char_list     : builder_interface -> list uc_char -> unit;
value add_box_list      : builder_interface -> list box -> unit;
value add_cmd_list      : builder_interface -> list box -> unit;

value void_builder      : builder_interface;
value simple_builder    : font_metric -> glyph_composer -> builder (list box);


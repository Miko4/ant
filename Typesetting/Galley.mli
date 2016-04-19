
open XNum;
open Runtime;
open Dim;
open Substitute;
open FontMetric;
open Box;

type line_params =
{
  baseline_skip      : dim;               (* amount of glue between baselines           *)
  line_skip_limit    : num;               (* minimal amout of glue between lines        *)
  line_skip          : dim;               (* the amout if the lines are closer together *)
  leading            : box -> box -> line_params -> dim;
  club_widow_penalty : int -> int -> num  (* penalty between two lines                  *)
};

type space_params =
{
  space_factor      : num;
  space_skip        : option dim;
  xspace_skip       : option dim;
  victorian_spacing : bool
};

type graphics_params =
{
  gp_colour    : Graphic.colour;
  gp_bg_colour : Graphic.colour;
  gp_alpha     : num
};

type galley;

value leading_fixed    : box -> box -> line_params -> dim;
value leading_register : box -> box -> line_params -> dim;
value leading_TeX      : box -> box -> line_params -> dim;
value leading_skyline  : box -> box -> line_params -> dim;

value new_galley    : num -> line_params -> ParLayout.par_params -> ParLayout.line_break_params ->
                      JustHyph.hyphen_params -> space_params -> MathLayout.math_params -> galley;
value measure       : galley -> num;
value lines         : galley -> list box;
value get_line      : galley -> int -> box;
value keep_lines    : galley -> list box -> galley;
value modify_glue   : galley -> (list box -> list box) -> galley;

value graphics_params               : galley -> graphics_params;
value par_params                    : galley -> ParLayout.par_params;
value line_params                   : galley -> line_params;
value line_break_params             : galley -> ParLayout.line_break_params;
value hyphen_params                 : galley -> JustHyph.hyphen_params;
value space_params                  : galley -> space_params;
value math_params                   : galley -> MathLayout.math_params;
value current_par_params            : galley -> ParLayout.par_params;
value current_line_params           : galley -> line_params;
value current_line_break_params     : galley -> ParLayout.line_break_params;
value current_hyphen_params         : galley -> JustHyph.hyphen_params;
value current_space_params          : galley -> space_params;
value current_math_params           : galley -> MathLayout.math_params;
value set_graphics_params           : galley -> graphics_params -> galley;
value set_par_params                : galley -> ParLayout.par_params -> galley;
value set_line_params               : galley -> line_params -> galley;
value set_line_break_params         : galley -> ParLayout.line_break_params -> galley;
value set_hyphen_params             : galley -> JustHyph.hyphen_params -> galley;
value set_space_params              : galley -> space_params -> galley;
value set_math_params               : galley -> MathLayout.math_params -> galley;
value set_current_par_params        : galley -> ParLayout.par_params -> galley;
value set_current_line_params       : galley -> line_params -> galley;
value set_current_line_break_params : galley -> ParLayout.line_break_params -> galley;
value set_current_hyphen_params     : galley -> JustHyph.hyphen_params -> galley;
value set_current_space_params      : galley -> space_params -> galley;
value set_current_math_params       : galley -> MathLayout.math_params -> galley;

value copy_params           : galley -> galley -> galley;
value reset_params          : galley -> galley;

value add_line      : galley -> box -> galley;
value add_glue      : galley -> box -> galley;
value add_paragraph : galley -> UCStream.location -> list extended_glyph_item -> galley;

value put_in_vbox   : galley -> box;
value put_in_vtop   : galley -> box;


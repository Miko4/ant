
open XNum;
open Runtime;
open Unicode.Types;
open Dim;
open Substitute;
open FontMetric;
open Box;

type math_style =
[ Display | CrampedDisplay | Text    | CrampedText
| Script  | CrampedScript  | Script2 | CrampedScript2
];

value cramped_style     : math_style -> math_style;
value is_cramped        : math_style -> bool;
value is_display        : math_style -> bool;
value sub_style         : math_style -> math_style;
value super_style       : math_style -> math_style;
value numerator_style   : math_style -> math_style;
value denominator_style : math_style -> math_style;

type math_font_params = (font_parameter * font_parameter * font_parameter);

value make_font_params : font_metric -> font_metric -> font_parameter;
value get_font_params  : math_font_params -> math_style -> font_parameter;
value get_super_shift  : font_parameter -> math_style -> num;
value get_num_shift    : font_parameter -> math_style -> num -> num;
value get_denom_shift  : font_parameter -> math_style -> num;

type math_params =
{
  thin_math_skip       : dim;
  med_math_skip        : dim;
  thick_math_skip      : dim;
  script_space         : dim;
  rel_penalty          : num;
  binop_penalty        : num;
  delimiter_factor     : num;
  delimiter_shortfall  : num;
  null_delimiter_space : dim
};

value math_units_to_points : font_parameter -> num -> num;
value math_dim_to_points   : font_parameter -> dim -> dim;
value remove_math_box      : box -> box;

value layout : math_style -> list box -> math_font_params -> math_params -> list box;

type delimiter_code = (uc_char * list font_metric * uc_char * list font_metric);

value simple_attach_delimiters : math_style -> delimiter_code -> delimiter_code -> list box ->
                                 math_font_params -> math_params -> box;
value attach_delimiters        : math_style -> list delimiter_code -> list (list box) ->
                                 math_font_params -> math_params -> box;

value make_delimiter : math_style -> num -> delimiter_code -> math_font_params -> math_params -> box;
value make_operator  : math_style -> glyph_desc -> font_metric ->
                       math_font_params -> box;
value make_overline  : math_style -> list box -> math_font_params -> math_params -> box;
value make_underline : math_style -> list box -> math_font_params -> math_params -> box;
value make_fraction  : math_style -> list box -> list box ->
                       delimiter_code -> delimiter_code -> num ->
                       math_font_params -> math_params -> box;
value make_root      : math_style -> box -> delimiter_code -> math_font_params -> math_params ->
                       box;
value make_accent    : math_style -> uc_char -> font_metric -> list box ->
                       math_font_params -> math_params -> box;


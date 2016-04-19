
open XNum;
open Unicode.Types;
open Runtime;
open FontMetric;
open Dim;
open Typesetting;
open Box;

type environment;

type env_cmd = UCStream.location -> environment -> environment;

(*
  Since the current font is unknown to the parser, skips and dimensions are passed as functions
  which return the real value given the environment as argument.
*)

type skip_arg = environment -> num;
type dim_arg  = environment -> dim;

type line_param_arg =
  (dim_arg * skip_arg * dim_arg * (box -> box -> Galley.line_params -> dim) * (int -> int -> num));
type par_param_arg  =
  (num * dim_arg * dim_arg * dim_arg * dim_arg * (environment -> int -> (num * num)) * dim_arg *
   (environment -> list extended_glyph_item) *
   (environment -> list extended_glyph_item) *
   (environment -> list box -> list box));
type line_break_param_arg = (num * num * int * num * num * num * num * skip_arg * num * skip_arg * bool);
type hyphen_param_arg = (uc_string * num * num * int * int * uc_string);
type space_param_arg = (num * option dim_arg * option dim_arg * bool);
type math_param_arg =
  (dim_arg * dim_arg * dim_arg * dim_arg * num * num * num * skip_arg * dim_arg);

type par_param_modifier  =
  (option num * option dim_arg * option dim_arg * option dim_arg * option dim_arg *
   option (environment -> int -> (num * num)) * option dim_arg *
   option (environment -> list extended_glyph_item) *
   option (environment -> list extended_glyph_item) *
   option (environment -> list box -> list box));
type line_param_modifier =
  (option dim_arg * option skip_arg * option dim_arg * option (box -> box -> Galley.line_params -> dim) *
   option (int -> int -> num));
type line_break_param_modifier =
  (option num * option num * option int * option num * option num * option num *
   option num * option skip_arg * option num * option skip_arg * option bool);
type hyphen_param_modifier =
  (option uc_string * option num * option num * option int * option int * option uc_string);
type space_param_modifier =
  (option num * option dim_arg * option dim_arg * option bool);
type math_param_modifier =
  (option dim_arg * option dim_arg * option dim_arg * option dim_arg *
  option num * option num * option num * option skip_arg * option dim_arg);

type font_spec = (option uc_string * option uc_string * option uc_string * option num * option uc_string * option (list uc_string));

(* accessor methods *)

value galley_table                        : environment -> PTable.table Galley.galley;
value current_galley                      : environment -> Galley.galley;
value page_layout_table                   : environment -> PTable.table PageLayout.page_layout;
value current_page_layout                 : environment -> PageLayout.page_layout;
value current_page_number                 : environment -> int;
value current_float_misplacement_demerits : environment -> num;
value current_math_style                  : environment -> MathLayout.math_style;
value current_font                        : environment -> Fonts.font;
value current_math_fonts                  : environment -> array (Fonts.font * Fonts.font * Fonts.font);
value current_math_font_params            : environment -> MathLayout.math_font_params;
value current_script_size                 : environment -> num;
value current_script_script_size          : environment -> num;
value current_font_metric                 : environment -> font_metric;
value current_composer                    : environment -> glyph_composer;
value get_pages                           : environment -> list FontMetric.page;

value set_math_style : environment -> MathLayout.math_style -> environment;

value sync_tables         : environment -> environment;
value save_environment    : environment -> environment;
value restore_environment : environment -> environment;

value get_hyphen_table : UCStream.location -> uc_string -> Hyphenation.hyphen_table;

value new_galley       : uc_string -> num -> env_cmd;
value select_galley    : uc_string -> env_cmd;
value set_galley_table : environment -> PTable.table Galley.galley -> environment;
value set_galley       : Galley.galley -> env_cmd;

value set_par_params                : par_param_modifier -> env_cmd;
value set_line_params               : line_param_modifier -> env_cmd;
value set_line_break_params         : line_break_param_modifier -> env_cmd;
value set_hyphen_params             : hyphen_param_modifier -> env_cmd;
value set_space_params              : space_param_modifier -> env_cmd;
value set_math_params               : math_param_modifier -> env_cmd;
value set_current_par_params        : par_param_modifier -> env_cmd;
value set_current_line_params       : line_param_modifier -> env_cmd;
value set_current_line_break_params : line_break_param_modifier -> env_cmd;
value set_current_hyphen_params     : hyphen_param_modifier -> env_cmd;
value set_current_space_params      : space_param_modifier -> env_cmd;
value set_current_math_params       : math_param_modifier -> env_cmd;

value modify_par_params        : par_param_modifier -> environment ->
                                 ParLayout.par_params -> ParLayout.par_params;
value modify_line_params       : line_param_modifier -> environment ->
                                 Galley.line_params -> Galley.line_params;
value modify_line_break_params : line_break_param_modifier -> environment ->
                                 ParLayout.line_break_params -> ParLayout.line_break_params;
value modify_hyphen_params     : hyphen_param_modifier -> UCStream.location ->
                                 JustHyph.hyphen_params -> JustHyph.hyphen_params;
value modify_space_params      : space_param_modifier -> environment ->
                                 Galley.space_params -> Galley.space_params;
value modify_math_params       : math_param_modifier -> environment ->
                                 MathLayout.math_params -> MathLayout.math_params;

value set_colour               : Graphic.colour -> env_cmd;

value adjust_graphics_state : environment -> environment -> list box;

value new_page_layout     : uc_string -> num -> num -> env_cmd;
value select_page_layout  : uc_string -> env_cmd;
value set_page_layout     : PageLayout.page_layout -> env_cmd;
value add_pages           : int -> list FontMetric.page -> env_cmd;

value declare_font        : uc_string -> uc_string -> uc_string ->
                            uc_string -> (num * num) -> font_load_params -> env_cmd;
value set_font            : font_spec -> env_cmd;
value get_math_font       : environment -> MathLayout.math_style -> int -> font_metric;
value set_math_font       : (option int * option uc_string * option uc_string * option uc_string
                             * option num * option num * option num) -> env_cmd;

value adapt_fonts_to_math_style : env_cmd;

value set_space_factor    : environment -> num -> environment;
value get_space_factor    : environment -> uc_char -> num;
value adjust_space_factor : uc_char -> env_cmd;

value initialise_environment : unit -> environment;


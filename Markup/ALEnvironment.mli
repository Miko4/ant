
open XNum;
open Runtime;
open Unicode.Types;
open Unicode.SymbolTable;
open VM;
open Types;
open Engine;
open Environment;
open ALCoding;

value wrap_env   : environment -> partial_value;
value unwrap_env : string -> unknown -> environment;

value decode_env_cmd : string -> unknown -> env_cmd;
value encode_env_cmd : string -> env_cmd -> partial_value;

value encode_skip_arg : skip_arg -> partial_value;
value decode_skip_arg : string -> unknown -> skip_arg;

value encode_dim_arg : dim_arg -> partial_value;
value decode_dim_arg : string -> unknown -> dim_arg;

value lookup_skip : string -> SymbolMap.t unknown -> symbol -> option skip_arg;
value lookup_dim  : string -> SymbolMap.t unknown -> symbol -> option dim_arg;

value env_quad      : unknown -> unknown -> partial_value;
value env_x_height  : unknown -> unknown -> partial_value;
value env_math_unit : unknown -> unknown -> partial_value;

value decode_leading : string -> option symbol ->
                       option (Typesetting.Box.box -> Typesetting.Box.box ->
                               Typesetting.Galley.line_params -> Dim.dim);

value decode_par_params        : string -> unknown -> par_param_modifier;
value decode_line_params       : string -> unknown -> line_param_modifier;
value decode_line_break_params : string -> unknown -> line_break_param_modifier;
value decode_hyphen_params     : string -> unknown -> hyphen_param_modifier;
value decode_space_params      : string -> unknown -> space_param_modifier;
value decode_math_params       : string -> unknown -> math_param_modifier;

value prim_set_par_params                : unknown -> partial_value;
value prim_set_current_par_params        : unknown -> partial_value;
value prim_set_line_params               : unknown -> partial_value;
value prim_set_current_line_params       : unknown -> partial_value;
value prim_set_line_break_params         : unknown -> partial_value;
value prim_set_current_line_break_params : unknown -> partial_value;
value prim_set_hyphen_params             : unknown -> partial_value;
value prim_set_current_hyphen_params     : unknown -> partial_value;
value prim_set_space_params              : unknown -> partial_value;
value prim_set_current_space_params      : unknown -> partial_value;
value prim_set_math_params               : unknown -> partial_value;
value prim_set_current_math_params       : unknown -> partial_value;
value prim_new_galley                    : unknown -> unknown -> partial_value;
value prim_select_galley                 : unknown -> partial_value;
(*value prim_set_par_shape                 : unknown -> unknown -> unit;*)
value prim_set_colour                    : unknown -> partial_value;
value prim_new_page_layout               : list unknown -> partial_value;
value prim_select_page_layout            : unknown -> partial_value;
(*value prim_get_math_font                 : unknown -> list unknown -> unit;*)
value prim_set_math_font                 : unknown -> partial_value;
value prim_adapt_fonts_to_math_style     : partial_value;
value prim_get_space_factor              : unknown -> unknown -> partial_value;
value prim_adjust_space_factor           : unknown -> partial_value;


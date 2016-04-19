
open VM;
open Types;
open Machine;
open ALDim;
open ALEnvironment;
open ALNodes;
open ALParseState;

value add_primitives job scope = do
{
  Array.iter
    (fun (n,p) -> bind_primitive scope n p)
    [|
      ("ps_get_global",           PrimitiveN 3 ps_get_global);
      ("ps_set_global",           PrimitiveN 3 ps_set_global);
      ("ps_next_char",            Primitive2 ps_next_char);
      ("ps_get_char",             PrimitiveN 3 ps_get_char);
      ("ps_remove_chars",         Primitive2 ps_remove_chars);
      ("ps_insert_string",        Primitive2 ps_insert_string);
      ("ps_location",             Primitive2 ps_location);
      ("ps_read_arg",             Primitive2 ps_read_arg);
      ("ps_arg_expanded",         Primitive2 ps_arg_expanded);
      ("ps_arg_execute",          PrimitiveN 3 ps_arg_execute);
      ("ps_arg_num",              Primitive2 ps_arg_num);
      ("ps_arg_int",              Primitive2 ps_arg_int);
      ("ps_arg_skip",             Primitive2 ps_arg_skip);
      ("ps_arg_dim",              Primitive2 ps_arg_dim);
      ("ps_arg_key_val",          Primitive2 ps_arg_key_val);
      ("ps_arg_dict",             PrimitiveN 3 ps_arg_dict);
      ("ps_opt_expanded",         PrimitiveN 3 ps_opt_expanded);
      ("ps_opt_key_val",          Primitive2 ps_opt_key_val);
      ("ps_opt_int",              PrimitiveN 3 ps_opt_int);
      ("ps_arg_TeX_dim",          Primitive2 ps_arg_TeX_dim);

      ("ps_current_mode",         Primitive2 ps_current_mode);
      ("ps_open_node_list",       Primitive2 ps_open_node_list);
      ("ps_close_node_list",      PrimitiveN 3 ps_close_node_list);
      ("ps_add_node",             Primitive2 ps_add_node);

      ("ps_set_default_char_cmd", PrimitiveN 3 ps_set_default_char_cmd);
      ("ps_define_command",       PrimitiveN 4 ps_define_command);
      ("ps_define_pattern",       PrimitiveN 4 ps_define_pattern);
      ("ps_define_macro",         PrimitiveN 4 ps_define_macro);
      ("ps_save_command",         Primitive2 ps_save_command);
      ("ps_restore_command",      Primitive2 ps_restore_command);
      ("ps_save_pattern",         Primitive2 ps_save_pattern);
      ("ps_restore_pattern",      Primitive2 ps_restore_pattern);
      ("ps_lookup_command",       PrimitiveN 3 ps_lookup_command);

      ("ps_push_env",             PrimitiveN 3 ps_push_env);
      ("ps_pop_env",              PrimitiveN 3 ps_pop_env);
      ("ps_set_env_args",         Primitive2 ps_set_env_args);
      ("ps_top_env",              PrimitiveN 3 ps_top_env);
      ("ps_lookup_env",           PrimitiveN 3 ps_lookup_env);
      ("ps_define_env",           PrimitiveN 6 ps_define_env);

      ("ps_shipout_pages",        PrimitiveN 4 ps_shipout_pages);
      ("ps_new_page_layout",      PrimitiveN 4 ps_new_page_layout);
      ("ps_new_galley",           PrimitiveN 3 ps_new_galley);
      ("ps_new_area",             PrimitiveN 10 ps_new_area);

      ("ps_declare_font",         PrimitiveN 7 ps_declare_font);
      ("ps_define_math_symbol",   PrimitiveN 5 ps_define_math_symbol);
      ("ps_define_root_symbol",   PrimitiveN 6 ps_define_root_symbol);
      ("ps_define_math_accent",   PrimitiveN 4 ps_define_math_accent);
      ("ps_set_math_code",        PrimitiveN 7 ps_set_math_code);

      ("ps_set_colour",           Primitive2 ps_set_colour);
      ("ps_set_bg_colour",        Primitive2 ps_set_bg_colour);
      ("ps_set_alpha",            Primitive2 ps_set_alpha);
      ("ps_stroke",               Primitive2 (ps_draw "ps_stroke" Runtime.Graphic.Stroke));
      ("ps_fill",                 Primitive2 (ps_draw "ps_stroke" Runtime.Graphic.Fill));
      ("ps_clip",                 Primitive2 (ps_draw "ps_stroke" Runtime.Graphic.Clip));
      ("ps_set_line_width",       Primitive2 ps_set_line_width);
      ("ps_set_line_cap",         Primitive2 ps_set_line_cap);
      ("ps_set_line_join",        Primitive2 ps_set_line_join);
      ("ps_set_miter_limit",      Primitive2 ps_set_miter_limit);

      ("ps_page_command",         Primitive2 ps_page_command);
      ("ps_par_command",          Primitive2 ps_par_command);

      ("ps_new_counter",          PrimitiveN 4 ps_new_counter);
      ("ps_get_counter",          PrimitiveN 3 ps_get_counter);
      ("ps_set_counter",          PrimitiveN 3 ps_set_counter);

      ("ps_dvi_special",          Primitive2 ps_dvi_special);
      ("ps_warning",              Primitive2 ps_warning);
      ("ps_error",                Primitive2 ps_error);

      ("ps_execute_next_char",    Primitive2 ps_execute_next_char);
      ("ps_execute_stream",       Primitive2 ps_execute_stream);
      ("ps_execute_argument",     Primitive1 ps_execute_argument);
      ("ps_run_parser",           PrimitiveN 3 ps_run_parser);

      ("ps_job_name",             (ascii_to_char_list job.Engine.Job.jobname));

      ("new_page_layout",         PrimitiveN 3 prim_new_page_layout);
      ("select_page_layout",      Primitive1 prim_select_page_layout);
      ("new_galley",              Primitive2 prim_new_galley);
      ("select_galley",           Primitive1 prim_select_galley);

      ("set_colour",              Primitive1 prim_set_colour);

(*  bind_primitive scope "set_font"                (Primitive1 prim_set_font);*)
(*  bind_primitive scope "get_math_font"           (PrimitiveN 3 prim_get_math_font);*)
      ("set_math_font",           Primitive1 prim_set_math_font);

      ("adapt_fonts_to_math_style",     prim_adapt_fonts_to_math_style);
      ("set_par_params",                Primitive1 prim_set_par_params);
      ("set_line_params",               Primitive1 prim_set_line_params);
      ("set_line_break_params",         Primitive1 prim_set_line_break_params);
      ("set_hyphen_params",             Primitive1 prim_set_hyphen_params);
      ("set_space_params",              Primitive1 prim_set_space_params);
      ("set_math_params",               Primitive1 prim_set_math_params);
      ("set_current_par_params",        Primitive1 prim_set_current_par_params);
      ("set_current_line_params",       Primitive1 prim_set_current_line_params);
      ("set_current_line_break_params", Primitive1 prim_set_current_line_break_params);
      ("set_current_hyphen_params",     Primitive1 prim_set_current_hyphen_params);
      ("set_current_space_params",      Primitive1 prim_set_current_space_params);
      ("set_current_math_params",       Primitive1 prim_set_current_math_params);

      ("get_space_factor",     Primitive2 prim_get_space_factor);
      ("adjust_space_factor",  Primitive1 prim_adjust_space_factor);

      ("make_dim",             PrimitiveN 5 prim_make_dim);
      ("fixed_dim",            Primitive1 prim_fixed_dim);
      ("dim_zero",             prim_dim_zero);
      ("dim_1pt",              prim_dim_1pt);
      ("dim_12pt",             prim_dim_12pt);
      ("dim_fil",              prim_dim_fil);
      ("dim_fill",             prim_dim_fill);
      ("dim_ss",               prim_dim_ss);
      ("dim_filneg",           prim_dim_filneg);
      ("dim_equal",            Primitive2 prim_dim_equal);
      ("dim_add",              Primitive2 prim_dim_add);
      ("dim_neg",              Primitive1 prim_dim_neg);
      ("dim_sub",              Primitive2 prim_dim_sub);
      ("dim_mult",             Primitive2 prim_dim_mult);
      ("dim_max",              Primitive2 prim_dim_max);
      ("dim_min",              Primitive2 prim_dim_min);
      ("dim_max_stretch",      Primitive1 prim_dim_max_stretch);
      ("dim_max_shrink",       Primitive1 prim_dim_max_shrink);
      ("dim_max_value",        Primitive1 prim_dim_max_value);
      ("dim_min_value",        Primitive1 prim_dim_min_value);
      ("dim_shift_base",       Primitive2 prim_dim_shift_base);
      ("dim_shift_base_upto",  Primitive2 prim_dim_shift_base_upto);
      ("dim_inc_upto",         Primitive2 prim_dim_inc_upto);
      ("dim_dec_upto",         Primitive2 prim_dim_dec_upto);
      ("dim_resize_upto",      Primitive2 prim_dim_resize_upto);
      ("adjustment_ratio",     Primitive2 prim_adjustment_ratio);
      ("dim_scale_badness",    Primitive1 prim_dim_scale_badness);
      ("dim_scale",            Primitive2 prim_dim_scale);
      ("dim_scale_upto",       Primitive2 prim_dim_scale_upto)
    |];
  bind_post_op scope "em" (Primitive2 env_quad);
  bind_post_op scope "ex" (Primitive2 env_x_height);
  bind_post_op scope "mu" (Primitive2 env_math_unit);
};

